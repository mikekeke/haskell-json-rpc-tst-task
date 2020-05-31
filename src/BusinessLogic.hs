module BusinessLogic where

import           Data.Text (Text)
import           Domain


class UserStorage m where
  saveUser :: User -> m ()
  getAll :: m [User]

class CheckUser m where
  validateAge :: Int -> m ValidAge

createUser :: (Monad m, UserStorage m, CheckUser m) => Text -> Int -> m ()
createUser uName uAge = validateAge uAge >>= saveUser . user uName
