{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.Movie where

import Core.Movie.Handler (deleteMovieHandler, getMovieHandler, getMoviesCommentsHandler, getMoviesHandler, setMovieHandler, updateMovieHandler)
import Data.Int (Int64)
import Data.Text (Text)
import Database.Esqueleto.Legacy (Entity)
import Servant
  ( Capture,
    Delete,
    Get,
    JSON,
    PlainText,
    Post,
    Put,
    ReqBody,
    Server,
    type (:<|>) (..),
    type (:>),
  )
import Storage.Database.DB
import Utils (Account (..))
import Prelude hiding (id)
import Core.Movie.Type (MovieRes(..))

type MovieAPI =
  Get '[JSON] [Movie]
    :<|> ReqBody '[JSON] Movie :> Post '[PlainText] Text
    :<|> Capture "movie_id" Int64
      :> ( Get '[JSON] MovieRes
             :<|> ReqBody '[JSON] Movie :> Put '[PlainText] Text
             :<|> Delete '[PlainText] Text
         )
    :<|> "comment" :> Get '[JSON] MovieRes

serverMovieAPI :: Account -> Server MovieAPI
serverMovieAPI (Account _) =
  getMoviesHandler
    :<|> setMovieHandler
    :<|> ( \id ->
             getMovieHandler id
               :<|> updateMovieHandler id
               :<|> deleteMovieHandler id
         )
    :<|> getMoviesCommentsHandler