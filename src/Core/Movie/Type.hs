{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Core.Movie.Type where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import Database.Esqueleto.Legacy (Entity)
import Storage.Database.DB

data MovieReq = MovieReq
  { name :: Text,
    decription :: Text,
    rating :: Int
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON MovieReq

data MovieRes = MovieRes
  { movies :: [Entity Movie]
  , comments :: [Maybe (Entity Comment)]
  }
  deriving stock (Eq, Show, Read, Generic)

instance ToJSON MovieRes