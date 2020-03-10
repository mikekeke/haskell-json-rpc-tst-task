module Logging where

import Control.Monad.Logger (LogLevel(LevelDebug), LogSource, filterLogger)
import Data.Foldable (foldl')


noDebugFilter :: LogSource -> LogLevel -> Bool
noDebugFilter _ lvl = lvl /= LevelDebug

logFilters = [noDebugFilter]

applyAllLogFilters = foldl' (.) id (fmap filterLogger logFilters)

