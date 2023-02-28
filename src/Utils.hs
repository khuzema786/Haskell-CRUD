module Utils where

import Data.Text (Text, pack)

stringToText :: String -> Text
stringToText = pack . show

newtype Account = Account {unAccount :: Text}