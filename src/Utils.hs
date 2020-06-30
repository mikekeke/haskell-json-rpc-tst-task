module Utils where

import Data.Text (pack, Text)

toText :: Show a => a -> Text
toText = pack . show

