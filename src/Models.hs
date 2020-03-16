{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Models
  ( user
  , User
  , isLegalAge
  , getId
  , ValidAge(..)
  , migrateAll
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
    deriving Show
|]

newtype ValidAge =
  ValidAge Int

legalAge = 18

isLegalAge :: Int -> Bool
isLegalAge userAge = userAge >= legalAge

user :: Text -> ValidAge -> User
user uName (ValidAge v) = User uName v

getId = unSqlBackendKey . unUserKey

instance ToJSON User where
  toJSON (User uName uAge) = object ["name" .= uName, "age" .= uAge]
