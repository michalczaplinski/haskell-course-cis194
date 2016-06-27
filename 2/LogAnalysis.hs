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


insert :: LogMessage -> MessageTree -> MessageTree
insert logMessage Leaf = Node Leaf logMessage Leaf
insert (Unknown _) tree = tree
insert logMessage tree
  | timestamp logMessage > treeTime tree = insert logMessage (leftTree tree)
  | otherwise = insert logMessage (rightTree tree)
  where
    timestamp (LogMessage _ ts _) = ts
    treeTime (Node _ (LogMessage _ ts _) _) = ts
    leftTree (Node left _ _) = left
    rightTree (Node _ _ right) = right
