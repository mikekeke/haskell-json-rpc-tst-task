module Utils where

import Data.Text (pack, Text)

import Data.Text (Text)

toText :: Show a => a -> Text
toText = pack . show

