{-# LANGUAGE FlexibleContexts #-}

module Storage.Cache.Query.Movie where

import Control.Monad (void)
import Core.Movie.Type (MovieRes (..))
import Data.ByteString.Char8 (pack, unpack)
import Data.Int (Int64)
import Database.Redis (get, setex)
import Storage.Database.Redis (runRedisAction)

cacheMovieWithCommentById :: Int64 -> MovieRes -> IO ()
cacheMovieWithCommentById id movie =
  runRedisAction $
    void $
      setex (pack ("MovieWithCommentById::" ++ show id)) 3600 (pack . show $ movie)

findMovieWithCommentById :: Int64 -> IO (Maybe MovieRes)
findMovieWithCommentById id = runRedisAction $ do
  result <- get (pack ("MovieWithCommentById::" ++ show id))
  case result of
    Right (Just movieString) -> return $ Just (read . unpack $ movieString)
    _ -> return Nothing