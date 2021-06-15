{-# LANGUAGE TemplateHaskell #-}

module Bank.Data
  ( -- * Users
    User (..),
    UserId (..),
    UserService (..),
    storeUser,
    loadUser,
    findUser,
    allUsers,

    -- * Accounts
    AccountEventIn (..),
    AccountEvent (..),
    TimedEvent (..),
    AccountId (..),
    AccountService (..),
    openAccount,
    closeAccount,
    accountsBy,
    loadAccount,
    AccountData (..),
    AccountLoadResult (..),
    processEvent,
    Whose (..),
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

newtype UserId = UserId UUID
  deriving newtype (FromJSON, ToJSON, Ord, Eq)

instance Elm UserId where toElmDefinition _ = DefPrim ElmString

data User = User
  { userId       :: UserId,
    userUsername :: Text,
    userPassword :: ByteString,
    refreshToken :: Maybe RefreshToken
  }

data UserService m a where
  StoreUser :: User -> UserService m ()
  LoadUser :: UserId -> UserService m (Maybe User)
  FindUser :: Text -> UserService m (Maybe User)
  AllUsers :: UserService m [User]

makeSem ''UserService

-------------------- ACCOUNTS --------------------

data AccountEventIn
  = DepositedIn Word
  | WithdrewIn Word
  | TransferToIn AccountId Word
  deriving stock (Generic, Eq)
  deriving (FromJSON, Elm) via ElmStreet AccountEventIn

data AccountEvent
  = Opened Text
  | Deposited Word
  | Withdrew Word
  | TransferTo AccountId Word
  | TransferFrom AccountId Word
  | Closed
  deriving stock (Generic, Eq)
  deriving (ToJSON, Elm) via ElmStreet AccountEvent

data TimedEvent = TimedEvent
  { time  :: UTCTime,
    event :: AccountEvent
  }
  deriving stock (Generic)
  deriving (ToJSON, Elm) via ElmStreet TimedEvent

newtype AccountId = AccountId UUID
  deriving newtype (FromJSON, ToJSON, Ord, Eq)

instance Elm AccountId where toElmDefinition _ = DefPrim ElmString

data AccountData = AccountData
  { accountId   :: AccountId,
    accountName :: Text,
    balance     :: Word,
    history     :: NonEmpty TimedEvent
  }
  deriving stock (Generic)
  deriving (ToJSON, Elm) via ElmStreet AccountData

data AccountLoadResult = NoAccountFound | NotYourAccount | LoadResult AccountData
  deriving stock (Generic)
  deriving (ToJSON, Elm) via ElmStreet AccountLoadResult

data Whose = Yours | Theirs
  deriving stock (Generic)
  deriving (ToJSON, Elm) via ElmStreet Whose

data AccountProcessResult
  = AccountOk
  | NotYourAccountToModify
  | NotEnoughBalance
  | AccountClosed Whose
  | AccountDoesNotExist Whose
  deriving stock (Generic)
  deriving (ToJSON, Elm) via ElmStreet AccountProcessResult

data AccountService m a where
  OpenAccount :: User -> Text -> AccountService m AccountId
  CloseAccount :: User -> AccountId -> AccountService m Bool
  AccountsBy :: UserId -> AccountService m [(AccountId, Text)]
  LoadAccount :: User -> AccountId -> AccountService m AccountLoadResult
  ProcessEvent :: User -> AccountId -> AccountEventIn -> AccountService m AccountProcessResult

makeSem ''AccountService

type ElmTypes =
  '[ UserId,
     AccountEventIn,
     AccountEvent,
     TimedEvent,
     AccountId,
     Whose,
     AccountProcessResult,
     AccountData,
     AccountLoadResult
   ]
