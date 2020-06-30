{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}

import           Conduit             (MonadThrow)
import           Control.Exception   (SomeException)
import           Control.Monad.Catch (fromException)
import           Control.Monad.State (State, evalState, get, modify)
import           Data.Either         (fromRight)
import           Data.UUID
import           Domain
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = do
  quickCheckWith stdArgs {maxSuccess = 1000} prop_ageParsingDintAffectAge

prop_ageParsingDintAffectAge age =
  case parseAge age of
    Right (ValidAge age') -> age == age'
    Left (fromException -> Just (IllegalUserAge invalidAge)) -> invalidAge == age
