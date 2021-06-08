{-# LANGUAGE TemplateHaskell #-}

module Bank.Data
  ( User (..),
    UserData (..),
    storeUser,
    loadUser,
    findUser
  )
where

import           Bank.Jwt
import           Data.UUID (UUID)
import           Polysemy
import           Relude

data User = User
  { userId       :: UUID,
    userUsername :: Text,
    userPassword :: ByteString,
    refreshToken :: Maybe RefreshToken
  }

data UserData m a where
  StoreUser :: User -> UserData m ()
  LoadUser :: UUID -> UserData m (Maybe User)
  FindUser :: Text -> UserData m (Maybe User)

makeSem ''UserData
