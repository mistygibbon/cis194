{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
import Log

-- Exercise 1
parseMessage :: String -> LogMessage
parseMessage string = case string of
  'I':_ -> LogMessage Info (read ((words string)!!1)) (unwords (drop 2 (words string)))
  'W':_ -> LogMessage Warning (read ((words string)!!1)) (unwords (drop 2 (words string)))
  'E':_ -> LogMessage (Error (read ((words string)!!1))) (read ((words string)!!2)) (unwords (drop 3 (words string)))
  _ -> Unknown string

parse :: String -> [LogMessage]
parse logs = map parseMessage (lines logs)

-- Exercise 2
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) messageTree = messageTree
insert message (Leaf) = Node Leaf message Leaf
insert message@(LogMessage _ timeStamp _) (Node left nodeMessage@(LogMessage _ nodeTimeStamp _) right)
  | (timeStamp > nodeTimeStamp) = Node left nodeMessage (insert message right)
  | (timeStamp < nodeTimeStamp) = Node (insert message left) nodeMessage right
insert _ tree = tree

-- Exercise 3
build :: [LogMessage] -> MessageTree
build list = case list of
  [] -> Leaf
  x:[] -> insert x Leaf
  x:xs -> insert x (build xs)

-- Exercise 4
inOrder :: MessageTree -> [LogMessage]
inOrder tree = case tree of
  -- (Node (Leaf) logMessage (Leaf)) -> [logMessage]
  -- (Node left logMessage (Leaf)) -> (inOrder left) ++ [logMessage]
  -- (Node (Leaf) logMessage right) -> [logMessage] ++ (inOrder right)
  (Node left logMessage right) -> (inOrder left) ++ [logMessage] ++ (inOrder right)
  (Leaf) -> []

-- Exercise 5
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = map getString (filter filterMessage (inOrder (build messages))) where

  getString :: LogMessage -> String
  getString (LogMessage _ _ string) = string
  getString (Unknown string) = string

  filterMessage :: LogMessage -> Bool
  filterMessage (LogMessage (Error severity) _ _) = severity >= 50
  filterMessage _ = True

-- Exercise 6 (incomplete)
-- Trying to obtain useful log information by filtering out Alice's Adventures in Wonderland
containsNum :: String -> Bool
containsNum [] = False
containsNum (x:xs)
  | ((x) `elem` ['1','2','3','4','5','6','7','8','9','0']) = True
  | otherwise = containsNum xs
