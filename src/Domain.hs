{-# LANGUAGE FlexibleContexts           #-}
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
  , ValidAge(..)
  , parseAge
  , migrateAll
  , DomainError(..)
  , parseUser
  ) where

import           Control.Monad.Catch     (Exception, throwM)
import           Data.Aeson              (ToJSON, object, toJSON, (.=))
import           Data.Text
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH
import           Utils                   (toText)

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
  deriving (Show) -- todo IsString instance? (https://etorreborre.blogspot.com/2019/09/processing-csv-files-in-haskell.html)

instance Exception DomainError

newtype ValidAge =
  ValidAge Int
  deriving (Eq, Show)

legalAge = 18 -- todo probably need to be read from config

isLegalAge :: Int -> Bool
isLegalAge userAge = userAge >= legalAge

parseAge x =
  if isLegalAge x
    then pure (ValidAge x)
    else throwM (IllegalUserAge x)

user :: Text -> ValidAge -> User
user uName (ValidAge v) = User uName v

parseUser name age = user name <$> parseAge age

getId = unSqlBackendKey . unUserKey

instance ToJSON User where
  toJSON (User uName uAge) = object ["name" .= uName, "age" .= uAge]
