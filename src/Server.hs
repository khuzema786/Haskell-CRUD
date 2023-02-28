{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Server (runServer) where

import API.Movie (MovieAPI, serverMovieAPI)
import API.User (UserAPI, serverUserAPI)
import Control.Exception (throw)
import Control.Logging as Logger (log, withStdoutLogging)
import Data.ByteString (ByteString)
import Network.Wai (Request, requestHeaders)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setLogger, setPort)
import Network.Wai.Logger (withStdoutLogger)
import Servant
  ( Application,
    Context (EmptyContext, (:.)),
    Handler,
    Proxy (..),
    Server,
    err401,
    err403,
    errBody,
    serveWithContext,
    type (:<|>) (..),
    type (:>),
  )
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import Utils (Account (..))
import Web.Cookie (parseCookies)

type API =
  "user" :> UserAPI
    :<|> "movie" :> AuthProtect "cookie-auth" :> MovieAPI

serverAPI :: Proxy API
serverAPI = Proxy

authenticateAccount :: ByteString -> Handler Account
authenticateAccount key = 
  if key == "Authenticated" 
    then return $ Account "Hello"
    else throw (err403 {errBody = "Invalid Cookie"})

authHandler :: AuthHandler Request Account
authHandler = mkAuthHandler handler
  where
    maybeToEither e = maybe (Left e) Right
    throw401 msg = throw $ err401 {errBody = msg}
    handler req = either throw401 authenticateAccount $ do
      cookie <- maybeToEither "Missing cookie header" $ lookup "cookie" $ requestHeaders req
      maybeToEither "Missing token in cookie" $ lookup "servant-auth-cookie" $ parseCookies cookie

type instance AuthServerData (AuthProtect "cookie-auth") = Account

serverContext :: Context (AuthHandler Request Account ': '[])
serverContext = authHandler :. EmptyContext

server :: Server API
server = serverUserAPI :<|> serverMovieAPI

app :: Application
app = serveWithContext serverAPI serverContext server

runServer :: IO ()
runServer = do
  Logger.withStdoutLogging $ do
    Logger.log "Server Started At Port 8081"
  withStdoutLogger $ \aplogger -> do
    let settings = setPort 8081 $ setLogger aplogger defaultSettings
    runSettings settings app
