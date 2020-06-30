{-# LANGUAGE OverloadedStrings    #-}

module AppConfig where

import Data.Text (Text)
appConf = AppConf ":memory:"

newtype AppConf = AppConf{dbConn :: Text}

