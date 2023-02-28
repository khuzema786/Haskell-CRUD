{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Core.User.Type where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)

data User = User
  { name :: Maybe Text,
    age :: Maybe Int,
    email :: Text,
    password :: Text
  }
  deriving stock (Eq, Show, Generic)

--   deriving anyclass (FromJSON)
instance FromJSON User

defaultUserRegister :: User
defaultUserRegister = User (Just "Test") (Just 24) "test@example.com" "0000"

defaultUserLogin :: User
defaultUserLogin = User Nothing Nothing "test@example.com" "0000"