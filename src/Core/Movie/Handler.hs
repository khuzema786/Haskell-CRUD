{-# LANGUAGE OverloadedStrings #-}

module Core.Movie.Handler where

import Servant (Handler, err402, ServerError (errBody))
import Data.Int (Int64)
import Storage.Database.Query.Movie (findAllMoviesWithComments, findAllMovies, findMovieWithCommentById, createMovie, deleteMovieById, updateMovieById, findAllMovies)
import Storage.Database.DB ( Movie )
import Control.Monad.Cont (liftIO)
import Data.Text (Text, pack)
import Control.Exception (throw)
import qualified Data.ByteString.Lazy.Char8 as B
import Core.Movie.Type (MovieRes(..))
import Control.Monad (void)
import qualified Storage.Cache.Query.Movie as Cache
import Database.Redis (RedisCtx)

getMoviesHandler :: Handler [Movie]
getMoviesHandler = liftIO findAllMovies

getMoviesCommentsHandler :: Handler MovieRes
getMoviesCommentsHandler = do
    movie <- liftIO findAllMoviesWithComments
    let movies = map fst movie
    let comments = map snd movie
    return $ MovieRes movies comments

getMovieHandler :: Int64 -> Handler MovieRes
getMovieHandler id = do 
    maybeMovieRes <- liftIO $ Cache.findMovieWithCommentById id
    case maybeMovieRes of
        Just movieRes -> return movieRes
        Nothing -> do
            maybeMovie <- liftIO $ findMovieWithCommentById id
            case maybeMovie of
                Just (movie, comment) -> do
                    void $ liftIO $ Cache.cacheMovieWithCommentById id $ MovieRes { movies = [movie], comments = [comment] }
                    return $ MovieRes { movies = [movie], comments = [comment] }
                Nothing -> throw err402 { errBody = B.pack $ "Could not find movie with ID : " ++ show id }

setMovieHandler :: Movie -> Handler Text
setMovieHandler movie = do
    id <- liftIO $ createMovie movie
    return $ pack $ "Movie with ID : " ++ show id ++ " is created successfully !"

updateMovieHandler :: Int64 -> Movie -> Handler Text
updateMovieHandler id movie = do
    _ <- liftIO $ updateMovieById id movie
    return $ pack $ "Movie with ID : " ++ show id ++ " is updated successfully !"

deleteMovieHandler :: Int64 -> Handler Text
deleteMovieHandler id = do
    _ <- liftIO $ deleteMovieById id
    return $ pack $ "Movie with ID : " ++ show id ++ " is deleted successfully !"