{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}

module Storage.Database.DB where

import Control.Monad.Logger (runStdoutLoggingT, LoggingT)
import Control.Monad.Reader (runReaderT, ReaderT)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Database.Esqueleto.Legacy (runMigration, Entity (..), toSqlKey, fromSqlKey)
import Database.Persist.Postgresql (ConnectionString, withPostgresqlConn)
import Database.Persist.SqlBackend.Internal (SqlBackend)
import qualified Database.Persist.TH as PTH
import Data.Text (Text)
import Data.Aeson.Types (toJSON, Pair, parseJSON, Parser, Object, (.:), withObject, (.=), object)
import Data.Aeson (FromJSON, ToJSON)

PTH.share
  [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"]
  [PTH.persistLowerCase|
  User sql=users
    name Text
    age Int
    email Text
    password Text
    UniqueEmail email
    deriving Show Read Eq
  Movie sql=movies
    name Text
    description Text
    rating Int
    deriving Show Read Eq
  Comment sql=comments
    userId UserId
    movieId MovieId
    description Text
    deriving Show Read Eq
|]

instance ToJSON (Entity User) where
  toJSON (Entity id user) = object $
    "id" .= fromSqlKey id : userPairs user

instance ToJSON User where
  toJSON user = object (userPairs user)

userPairs :: User -> [Pair]
userPairs user =
  [ "name" .= userName user
  , "age" .= userAge user
  , "email" .= userEmail user
  , "password" .= userPassword user
  ]

instance FromJSON (Entity User) where
  parseJSON = withObject "User Entity" $ \o -> do
    user <- parseUser o
    id <- o .: "id"
    return $ Entity (toSqlKey id) user

instance FromJSON User where
  parseJSON = withObject "User" parseUser

parseUser :: Object -> Parser User
parseUser o = do
  uName <- o .: "name"
  uEmail <- o .: "email"
  uAge <- o .: "age"
  uPassword <- o .: "password"
  return User
    { userName = uName
    , userEmail = uEmail
    , userAge = uAge
    , userPassword = uPassword
    }

instance ToJSON (Entity Movie) where
  toJSON (Entity id movie) = object $
    "id" .= fromSqlKey id : moviePairs movie

instance ToJSON Movie where
  toJSON movie = object (moviePairs movie)

moviePairs :: Movie -> [Pair]
moviePairs movie =
  [ "name" .= movieName movie
  , "description" .= movieDescription movie
  , "rating" .= movieRating movie
  ]

instance FromJSON (Entity Movie) where
  parseJSON = withObject "Movie Entity" $ \o -> do
    movie <- parseMovie o
    id <- o .: "id"
    return $ Entity (toSqlKey id) movie

instance FromJSON Movie where
  parseJSON = withObject "Movie" parseMovie

parseMovie :: Object -> Parser Movie
parseMovie o = do
  mName <- o .: "name"
  mDescription <- o .: "description"
  mRating <- o .: "rating"
  return Movie
    { movieName = mName
    , movieDescription = mDescription
    , movieRating = mRating
    }

instance ToJSON (Entity Comment) where
  toJSON (Entity id comment) = object $
    "id" .= fromSqlKey id : commentPairs comment

instance ToJSON Comment where
  toJSON comment = object (commentPairs comment)

commentPairs :: Comment -> [Pair]
commentPairs comment =
  [ "userId" .= fromSqlKey (commentUserId comment)
  , "movieId" .= fromSqlKey (commentMovieId comment)
  , "description" .= commentDescription comment
  ]

instance FromJSON (Entity Comment) where
  parseJSON = withObject "Comment Entity" $ \o -> do
    comment <- parseComment o
    id <- o .: "id"
    return $ Entity (toSqlKey id) comment

instance FromJSON Comment where
  parseJSON = withObject "Comment" parseComment

parseComment :: Object -> Parser Comment
parseComment o = do
  cUserId <- o .: "user_id"
  cMovieId <- o .: "movie_id"
  cDescription <- o .: "description"
  return Comment
    { commentUserId = toSqlKey cUserId
    , commentMovieId = toSqlKey cMovieId
    , commentDescription = cDescription
    }

connString :: ConnectionString
connString = "host=127.0.0.1 port=5432 user=cloud dbname=crud_app password=scape"

runAction :: MonadUnliftIO m => ReaderT SqlBackend (LoggingT m) a -> m a
runAction action = runStdoutLoggingT $ withPostgresqlConn connString $ \backend ->
  runReaderT action backend

migrateDB :: IO ()
migrateDB = runAction $ do
  runMigration migrateAll