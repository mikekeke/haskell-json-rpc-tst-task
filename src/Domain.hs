{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Domain
  ( user
  , User
  , getId
  , ValidAge
  , parseAge
  , migrateAll
  , DomainError(..)
  , unValidAge
  ) where

import           Data.Aeson              (ToJSON, object, toJSON, (.=))
import           Data.Text
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
User
    name Text
    UniqueName name
    age Int
    deriving Eq
    deriving Show
|]

data DomainError =
  IllegalUserAge Int
  deriving (Eq, Show) -- todo IsString instance? (https://etorreborre.blogspot.com/2019/09/processing-csv-files-in-haskell.html)

newtype ValidAge =
  ValidAge
    { unValidAge :: Int
    }
  deriving (Eq, Show)

legalAge = 18 -- todo probably need tobe read from config

isLegalAge :: Int -> Bool
isLegalAge userAge = userAge >= legalAge

parseAge :: Int -> Either DomainError ValidAge
parseAge x =
  if isLegalAge x
    then Right (ValidAge x)
    else Left (IllegalUserAge x)

user :: Text -> ValidAge -> User
user uName (ValidAge v) = User uName v

getId = unSqlBackendKey . unUserKey

getName (User uName _) = uName

getAge (User _ uAge) = uAge

instance ToJSON User where
  toJSON (User uName uAge) = object ["name" .= uName, "age" .= uAge]
