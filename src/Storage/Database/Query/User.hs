module Storage.Database.Query.User where

import Control.Monad.Logger (LoggingT)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Database.Esqueleto.Legacy (SqlPersistT, entityVal, from, fromSqlKey, insert, select, val, where_, (==.), (^.), (&&.))
import Storage.Database.DB (EntityField (UserEmail, UserPassword), User, runAction)
import Data.Text (Text)

createUser :: User -> IO Int64
createUser user = fromSqlKey <$> runAction (insert user)

findUserByEmailPassword :: Text -> Text -> IO (Maybe User)
findUserByEmailPassword email password = runAction action
  where
    action :: SqlPersistT (LoggingT IO) (Maybe User)
    action =
      fmap entityVal . listToMaybe
        <$> ( select . from $ \users -> do
                where_ (users ^. UserEmail ==. val email &&. users ^. UserPassword ==. val password)
                return users
            )