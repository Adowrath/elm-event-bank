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
    getAccounts,
    getHistory,
    handleEvent,
    AccountEventResult (..),
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
  = Opened
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

data AccountEventResult = AccountOk | NotEnoughBalance | AccountClosed | AccountDoesNotExist
  deriving stock (Generic)
  deriving (ToJSON, Elm) via ElmStreet AccountEventResult

data AccountService m a where
  OpenAccount :: User -> AccountService m AccountId
  CloseAccount :: AccountId -> AccountService m Bool
  GetAccounts :: User -> AccountService m [AccountId]
  GetHistory :: AccountId -> AccountService m (Maybe (NonEmpty TimedEvent))
  HandleEvent :: AccountId -> AccountEvent -> AccountService m AccountEventResult

makeSem ''AccountService
