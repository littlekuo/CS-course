{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log


parseMessage :: String -> LogMessage
parseMessage str = let wordLists = words str in
                   case wordLists of
                     ("I":ts:msg) -> LogMessage Info (read ts) (unwords msg)
                     ("W":ts:msg) -> LogMessage Warning (read ts) (unwords msg)
                     ("E":e:ts:msg) -> LogMessage (Error (read e)) (read ts) (unwords msg)
                     _ -> Unknown (unwords wordLists)


parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert localmg@(LogMessage _ _ _) Leaf = Node Leaf localmg Leaf
insert localmg1@(LogMessage _ ts1 _) (Node left localmg2@(LogMessage _ ts2 _) right) 
                | ts1 > ts2 = Node left localmg2 (insert localmg1 right)
                | otherwise = Node (insert localmg1 left) localmg2 right
insert _ tree = tree


build :: [LogMessage] -> MessageTree
build = foldl (\m l -> insert l m) Leaf 


inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left logM right) =  (inOrder left) ++  (logM : (inOrder right))


isSevere :: Int -> LogMessage -> Bool
isSevere l (LogMessage (Error lvl) _ _)
               | lvl > l  = True
               | otherwise = False
isSevere _ _ = False


toStr :: [LogMessage] -> [String]
toStr (LogMessage _ _ msg : rs) = msg : toStr rs
toStr _ = []

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong =  toStr . inOrder . build . filter (isSevere 50)
                                                          




