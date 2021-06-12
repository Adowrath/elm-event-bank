{-# LANGUAGE TemplateHaskell #-}

module Bank.Data
  ( -- * Users
    User (..),
    UserService (..),
    storeUser,
    loadUser,
    findUser,

    -- * Accounts
    AccountEvent (..),
    TimedEvent (..),
    AccountId (..),
    AccountService (..),
    openAccount,
    closeAccount,
    myAccounts,
    allAccounts,
    loadAccount,
    processEvent,
    AccountProcessResult (..),

    -- * Elm Types
    ElmTypes,
  )
where

import           Bank.Jwt        hiding (ElmTypes)
import           Data.Aeson      (FromJSON, ToJSON)
import           Data.Time.Clock (UTCTime)
import           Data.UUID       (UUID)
import           Elm
import           Polysemy
import           Relude

data User = User
  { userId       :: UUID,
    userUsername :: Text,
    userPassword :: ByteString,
    refreshToken :: Maybe RefreshToken
  }

data UserService m a where
  StoreUser :: User -> UserService m ()
  LoadUser :: UUID -> UserService m (Maybe User)
  FindUser :: Text -> UserService m (Maybe User)

makeSem ''UserService

-------------------- ACCOUNTS --------------------

data AccountEvent
  = Opened Text
  | Deposited Word
  | Withdrew Word
  | TransferFrom AccountId Word
  | TransferTo AccountId Word
  | Closed
  deriving stock (Generic)
  deriving (FromJSON, ToJSON, Elm) via ElmStreet AccountEvent

data TimedEvent = TimedEvent
  { time  :: UTCTime,
    event :: AccountEvent
  }
  deriving stock (Generic)
  deriving (ToJSON, Elm) via ElmStreet TimedEvent

newtype AccountId = AccountId UUID
  deriving newtype (FromJSON, ToJSON)

instance Elm AccountId where toElmDefinition _ = DefPrim ElmString

data Whose = Yours | Theirs
  deriving stock (Generic)
  deriving (ToJSON, Elm) via ElmStreet Whose

data AccountProcessResult = AccountOk | NotEnoughBalance | AccountClosed Whose | AccountDoesNotExist Whose
  deriving stock (Generic)
  deriving (ToJSON, Elm) via ElmStreet AccountProcessResult

data AccountData = AccountData
  { accountId :: AccountId,
    ownerName :: Text,
    balance   :: Int,
    history   :: [TimedEvent]
  }
  deriving stock (Generic)
  deriving (ToJSON, Elm) via ElmStreet AccountData

data AccountService m a where
  OpenAccount :: User -> AccountService m AccountId
  CloseAccount :: User -> AccountId -> AccountService m Bool
  MyAccounts :: User -> AccountService m [AccountId]
  AllAccounts :: AccountService m [AccountId]
  LoadAccount :: User -> AccountId -> AccountService m (Either AccountData (NonEmpty TimedEvent))
  ProcessEvent :: User -> AccountId -> AccountEvent -> AccountService m AccountProcessResult

makeSem ''AccountService

type ElmTypes = '[AccountEvent, TimedEvent, AccountId, Whose, AccountProcessResult]
