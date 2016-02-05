{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import           Log

-- Exercise 1

parseInfo :: [String] -> LogMessage
parseInfo (s : ss) = LogMessage Info (read s) (unwords ss)

parseWarning :: [String] -> LogMessage
parseWarning (s : ss) = LogMessage Warning (read s) (unwords ss)

parseError :: [String] -> LogMessage
parseError (s : t : ss) = LogMessage (Error (read s)) (read t) (unwords ss)

parseMessage :: String -> LogMessage
parseMessage line = case id of
                                        "I"    -> parseInfo (tail lineWords)
                                        "W"  -> parseWarning (tail lineWords)
                                        "E"   -> parseError (tail lineWords)
                                        _     -> Unknown line
                                where
                                        lineWords = words line
                                        id = head lineWords

parseLines :: [String] -> [LogMessage]
parseLines = map parseMessage


parse :: String -> [LogMessage]
parse = parseLines . lines

-- Exercise 2

getTime :: LogMessage -> TimeStamp
getTime (LogMessage _ time _) = time

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert logM Leaf = Node Leaf logM Leaf
insert logM (Node leftT nodeM rightT)
                                | logTime < nodeTime = Node (insert logM leftT) nodeM rightT
                                | otherwise                = Node leftT nodeM (insert logM rightT)
                                where
                                    logTime   = getTime logM
                                    nodeTime = getTime nodeM

-- Exercise 3

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Exercise 4

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left node right) = inOrder left ++ [node] ++ inOrder right

-- Exercise 5

isSerious :: LogMessage -> Bool
isSerious (LogMessage (Error severity) _ _) = severity >= 50
isSerious _ = False

getMessage :: LogMessage -> String
getMessage (LogMessage _ _ m) = m

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logs =   map getMessage seriousMessages
                                    where
                                        sortedMessage = inOrder (build logs)
                                        seriousMessages = filter isSerious sortedMessage









