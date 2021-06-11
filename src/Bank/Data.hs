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
    accountHistory,
    processEvent,
    AccountProcessResult (..),
  )
where

import           Bank.Jwt
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
  | Deposited Int
  | Withdrew Int
  | TransferFrom AccountId Int
  | TransferTo AccountId Int
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

data AccountService m a where
  OpenAccount :: User -> AccountService m AccountId
  CloseAccount :: User -> AccountId -> AccountService m Bool
  MyAccounts :: User -> AccountService m [AccountId]
  AllAccounts :: AccountService m [AccountId]
  AccountHistory :: User -> AccountId -> AccountService m (Maybe (NonEmpty TimedEvent))
  ProcessEvent :: User -> AccountId -> AccountEvent -> AccountService m AccountProcessResult

makeSem ''AccountService
