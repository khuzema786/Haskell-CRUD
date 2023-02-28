module Storage.Database.Redis where

import Database.Redis
    ( connect, defaultConnectInfo, runRedis, Redis )

runRedisAction :: Redis a -> IO a
runRedisAction action = do
  connection <- connect defaultConnectInfo
  runRedis connection action