{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage s = case words s of
  ("I" : n : rest) -> LogMessage Error
