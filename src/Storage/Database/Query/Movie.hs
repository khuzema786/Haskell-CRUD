module Storage.Database.Query.Movie where

import Control.Monad.Logger (LoggingT)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Database.Esqueleto.Legacy (Entity, LeftOuterJoin (..), SqlPersistT, delete, entityVal, from, fromSqlKey, insert, just, on, replace, select, toSqlKey, val, where_, (==.), (?.), (^.))
import Storage.Database.DB (Comment, EntityField (CommentMovieId, MovieId), Movie, runAction)

findMovieById :: Int64 -> IO (Maybe Movie)
findMovieById id = runAction action
  where
    action :: SqlPersistT (LoggingT IO) (Maybe Movie)
    action =
      fmap entityVal . listToMaybe
        <$> ( select . from $ \movies -> do
                where_ (movies ^. MovieId ==. val (toSqlKey id))
                return movies
            )

findMovieWithCommentById :: Int64 -> IO (Maybe (Entity Movie, Maybe (Entity Comment)))
findMovieWithCommentById id = runAction action
  where
    action :: SqlPersistT (LoggingT IO) (Maybe (Entity Movie, Maybe (Entity Comment)))
    action =
      listToMaybe
        <$> ( select . from $ \(movies `LeftOuterJoin` comments) -> do
                on (just (movies ^. MovieId) ==. comments ?. CommentMovieId)
                where_ (movies ^. MovieId ==. val (toSqlKey id))
                return (movies, comments)
            )

findAllMovies :: IO [Movie]
findAllMovies = runAction action
  where
    action :: SqlPersistT (LoggingT IO) [Movie]
    action =
      fmap entityVal
        <$> ( select . from $ \movies -> do
                return movies
            )

findAllMoviesWithComments :: IO [(Entity Movie, Maybe (Entity Comment))]
findAllMoviesWithComments = runAction action
  where
    action :: SqlPersistT (LoggingT IO) [(Entity Movie, Maybe (Entity Comment))]
    action =
      select . from $ \(movies `LeftOuterJoin` comments) -> do
        on (just (movies ^. MovieId) ==. comments ?. CommentMovieId)
        return (movies, comments)

createMovie :: Movie -> IO Int64
createMovie movie = fromSqlKey <$> runAction (insert movie)

updateMovieById :: Int64 -> Movie -> IO ()
updateMovieById id movie = runAction $ replace (toSqlKey id) movie

deleteMovieById :: Int64 -> IO ()
deleteMovieById id = runAction action
  where
    action :: SqlPersistT (LoggingT IO) ()
    action =
      delete . from $ \movie -> do
        where_ (movie ^. MovieId ==. val (toSqlKey id))