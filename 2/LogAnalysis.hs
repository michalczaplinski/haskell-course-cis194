{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage message = case take 3 (words message) of
  ["I", timestamp, _ ] -> LogMessage Info (read timestamp) (unwords (drop 2  (words message)))
  ["W", timestamp, _ ] -> LogMessage Warning (read timestamp) (unwords (drop 2  (words message)))
  ["E", severity, timestamp] -> LogMessage (Error (read severity))
                                           (read timestamp)
                                           (unwords (drop 3  (words message)))
  _  -> Unknown message


parse :: String -> [LogMessage]
parse messages = map parseMessage (lines messages)
