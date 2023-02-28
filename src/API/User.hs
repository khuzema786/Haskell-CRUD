{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module API.User where

import Core.User.Handler (loginUserHandler, registerUserHandler)
import Data.Text (Text)
import Servant
  ( Header,
    Headers,
    JSON,
    PlainText,
    Post,
    ReqBody,
    Server,
    type (:<|>) (..),
    type (:>),
  )
import Storage.Database.DB ( User )

type UserAPI =
  "register" :> ReqBody '[JSON] User :> Post '[PlainText, JSON] Text
    :<|> "login" :> ReqBody '[JSON] User :> Post '[JSON] (Headers '[Header "Set-Cookie" Text] Text)

serverUserAPI :: Server UserAPI
serverUserAPI = registerUserHandler :<|> loginUserHandler