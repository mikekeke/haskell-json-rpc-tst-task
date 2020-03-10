module BusinessLogic where
import Models
import Control.Monad ((>=>))
import Data.Text (Text)

class UserStorage m where
  saveUser :: User -> m ()
  getAll :: m [User]
  
class CheckUser m where
  checkAge :: Int ->  m ValidAge

createUser :: (Monad m, UserStorage m, CheckUser m) => Text -> Int -> m () 
createUser uName uAge = checkAge uAge >>= \validAge -> saveUser $ user uName validAge 
