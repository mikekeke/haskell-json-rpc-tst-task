module BusinessLogic where
import Models
import Control.Monad ((>=>))
import Data.Text (Text)

data AppError =
  IllegalUserAge Int
  | SqlFail Text
  deriving (Show)

class UserStorage m where
  saveUser :: User -> m ()
  getAll :: m [User]
  
class CheckUser m where
  validateAge :: Int ->  m ValidAge

createUser :: (Monad m, UserStorage m, CheckUser m) => Text -> Int -> m ()
createUser uName uAge = do 
  validatedAge <- validateAge uAge
  saveUser $ user uName validatedAge
