module BusinessLogic where
import Models
import Control.Monad ((>=>))
import Data.Text (Text)

data AppError =
  IllegalUserAge Int
  | SqlFail Text
  deriving (Show)

class UserStorage m where
  saveUser :: User -> m (Either AppError User)
  getAll :: m (Either AppError [User])
  
class CheckUser m where
  validateAge :: Int ->  m (Either AppError ValidAge)

createUser :: (Monad m, UserStorage m, CheckUser m) => Text -> Int -> m (Either AppError User)
createUser uName uAge = do 
  validatedAge <- validateAge uAge
  case validatedAge of
    Right a -> saveUser (user uName a)
    Left e -> return (Left e)
