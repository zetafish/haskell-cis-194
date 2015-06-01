{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 1
parseMessage :: String -> LogMessage
--parseMessage s = (Unknown s)
parseMessage s =
  case words s of
   ("E":num:stamp:msg) -> (LogMessage
                           (Error (read num))
                           (read stamp)
                           (unwords msg))
   ("I":num:msg) -> (LogMessage Info (read num) (unwords msg))
   _ -> (Unknown s)

parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) x = x
insert m Leaf = Node Leaf m Leaf
insert m1@(LogMessage _ s1 _) (Node a m2@(LogMessage _ s2 _) b)
  | s1 < s2 = Node (insert m1 a) m2 b
  | s2 < s1 = Node a m2 (insert m1 b)

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (x:xs) = insert x (build xs)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node a m b) = (inOrder a) ++ [m] ++ (inOrder b)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong mm =
  map f $ filter p (inOrder (build mm))
  where f (LogMessage _ _ text) = text
        p (LogMessage (Error s) _ _) = s>50
        p _ = False
