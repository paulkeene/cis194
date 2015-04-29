{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Control.Applicative
import Data.List (intercalate)
import Data.String.Utils
import Log
import Safe


-- Exercise 1

parseMessage :: String -> LogMessage
parseMessage s = message
  where
    tokens = words s
    (mayMessageType, mayTimestamp, messageText) = case tokens `atMay` 0 of
      Just "I" -> (Just Info, maybeRead =<< (tokens `atMay` 1),
                   unwords (drop 2 tokens))
      Just "W" -> (Just Warning, maybeRead =<< (tokens `atMay` 1),
                   unwords (drop 2 tokens))
      Just "E" -> (Error <$> (maybeRead =<< (tokens `atMay` 1)),
                   maybeRead =<< (tokens `atMay` 2),
                   unwords (drop 3 tokens))
      _ -> (Nothing, Nothing, "")
    message = case mayMessageType of
      Just mt -> case mayTimestamp of
        Just ts -> LogMessage mt ts messageText
        _ -> Unknown s
      _ -> Unknown s

parse :: String -> [LogMessage]
parse = (map parseMessage) . lines


-- Exercise 2

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert lm Leaf = Node Leaf lm Leaf
insert new_lm@(LogMessage _ new_ts _) (Node left lm@(LogMessage _ ts _) right)
  | new_ts < ts = Node (insert new_lm left) lm right
  | otherwise = Node left lm (insert new_lm right)


-- Exercise 3

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf


-- Exercise 4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left lm right) = (inOrder left) ++ [lm] ++ (inOrder right)


-- Exercise 5

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = (map extract) . (filter f) . inOrder . build
  where
    f (LogMessage (Error e) _ s) = e >= 50
    f _ = False
    extract (LogMessage _ _ s) = s
