module BusinessLogic where

import           Data.Text (Text)
import           Models

data AppError
  = IllegalUserAge Int
  deriving (Show) -- todo IsString instance?

class UserStorage m where
  saveUser :: User -> m ()
  getAll :: m [User]

class CheckUser m where
  validateAge :: Int -> m ValidAge

createUser :: (Monad m, UserStorage m, CheckUser m) => Text -> Int -> m ()
createUser uName uAge = validateAge uAge >>= saveUser . user uName
