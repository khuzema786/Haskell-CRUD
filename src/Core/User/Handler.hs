{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.User.Handler where

import Control.Exception (throw)
import Control.Monad.Cont (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Text (Text, pack)
import Servant (Handler, Header, Headers, ServerError (errBody), addHeader, err402)
import Storage.Database.DB (User, userEmail, userPassword)
import Storage.Database.Query.User (createUser, findUserByEmailPassword)

registerUserHandler :: User -> Handler Text
registerUserHandler user = do
  id <- liftIO $ createUser user
  return $ pack $ "User with ID : " ++ show id ++ " is registered successfully !"

loginUserHandler :: User -> Handler (Headers '[Header "Set-Cookie" Text] Text)
loginUserHandler user = do
  maybeUser <- liftIO $ findUserByEmailPassword (userEmail user) (userPassword user)
  case maybeUser of
    Just _ -> return $ addHeader "Set-Cookie : Authenticated" $ pack "User is logged in successfully !"
    Nothing -> throw err402 {errBody = B.pack "User does not exist, please try again !"}