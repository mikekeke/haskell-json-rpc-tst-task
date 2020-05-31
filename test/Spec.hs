{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

import           BusinessLogic
import           Data.Either            (fromRight)
import           Data.UUID
import           Domain
import           Test.Hspec
import           Test.QuickCheck
import Control.Monad.State (State, evalState, modify, get)

prop_ageValidation age =
  case parseAge age of
    Right validAge                   -> unValidAge validAge == age
    Left (IllegalUserAge invalidAge) -> invalidAge == age

main :: IO ()
main = do
  quickCheck prop_ageValidation
  hspec ageSpec
  hspec persistUserSpec

ageSpec :: Spec
ageSpec =
  describe "Age parsing check" $ do
    it "checks illlegal age" $
      let illegalAge = 17
       in parseAge illegalAge `shouldBe` (Left $ IllegalUserAge illegalAge)
    it "checks legal age" $
      let legalAge = 18
       in unValidAge <$> parseAge legalAge `shouldBe` Right legalAge

persistUserSpec :: Spec
persistUserSpec =
  describe "Check persistance" $ do
    it "Check user add/get" $ runTstPersist (saveUser testUser >> getAll) `shouldBe` [testUser]

testUser :: User
testUser = user "TestGuy" $ fromRight (error "WTF") (parseAge 18)

runTstPersist :: TestPersist a -> a
runTstPersist p = evalState (unTestPrs p) []

newtype TestPersist a =
  TestPrst
    { unTestPrs :: State [User] a
    }
  deriving (Functor, Applicative, Monad)

instance UserStorage TestPersist where
  saveUser u = TestPrst $ modify (testUser :)
  getAll = TestPrst $ get

instance CheckUser TestPersist where
  validateAge a = either (error "should not happen") (TestPrst . pure) (parseAge a)
