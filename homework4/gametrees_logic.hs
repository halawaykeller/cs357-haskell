-- Tic-tac-toe example from chapter 11 of Programming in Haskell,
-- Graham Hutton, Cambridge University Press, 2016.

-- Basic declarations

-- R = O
-- G = X

import Data.Char
import Data.List
import System.IO

size :: Int
size = 3

type Board = [Field]

data Field = R | B | G
              deriving (Eq, Ord, Show)

next :: Field -> Field
next R = G
next B = B
next G = R

-- Board utilities

empty :: Board 
empty = replicate size B

full :: Board -> Bool
full = all (/= B)

turn :: Board -> Field
turn g = if os <= xs then R else G
         where
            os = length (filter (== R) ps)
            xs = length (filter (== G) ps)
            ps = g

wins :: Field -> Board -> Bool
wins p g = any line (rows ++ cols ++ dias)
           where
              grid = chop 3 g
              line = all (== p)
              rows = grid
              cols = transpose grid
              dias = [diag grid, diag (map reverse grid)]

diag :: [Board] -> [Field]
diag g = [g !! n !! n | n <- [0..size-1]]

won :: Board -> Bool
won g = wins R g || wins G g

-- -- Making a move

valid :: Board -> Int -> Bool
valid g i = 0 <= i && i < size^2 && g !! i == B

move:: Board -> Int -> Field -> [Board]
move g i p =
   if valid g i then [xs ++ [p] ++ ys] else []
   where (xs,B:ys) = splitAt i g

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)


-- -- Game trees

data Tree a = Node a [Tree a]
              deriving Show

gametree :: Board -> Field -> Tree Board
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]

moves :: Board -> Field -> [Board]
moves g p | won g     = []
          | full g    = []
          | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _)  = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]

depth :: Int
depth = 9

-- -- Minimax

minimax :: Tree Board -> Tree (Board,Field)
minimax (Node g [])
   | wins R g  = Node (g,R) []
   | wins G g  = Node (g,G) []
   | otherwise = Node (g,B) []
minimax (Node g ts) 
   | turn g == R = Node (g, minimum ps) ts'
   | turn g == G = Node (g, maximum ps) ts'
                   where
                      ts' = map minimax ts
                      ps  = [p | Node (_,p) _ <- ts']

bestmove :: Board -> Field -> Board
bestmove g p = head [g' | Node (g',p') _ <- ts, p' == best]
               where 
                  tree = prune depth (gametree g p)
                  Node (_,best) ts = minimax tree


-- Testing

-- Since Green always starts second, these are the only valid starting boards for Green. 
startingPositionsForGreen :: [Board]
startingPositionsForGreen = [ [R,B,B,B,B,B,B,B,B],
                              [B,R,B,B,B,B,B,B,B],
                              [B,B,R,B,B,B,B,B,B],
                              [B,B,B,R,B,B,B,B,B],
                              [B,B,B,B,R,B,B,B,B],
                              [B,B,B,B,B,R,B,B,B],
                              [B,B,B,B,B,B,R,B,B],
                              [B,B,B,B,B,B,B,R,B] ] 

validNextMoves :: Board -> Field-> [Board]
validNextMoves g p 
                | won g = []
                | otherwise = [concat (move g i p) | i <- [0..((size^2)-1)], valid g i]

-- This tests the strategy for the SECOND player. Given a starting board, it tests all possible
-- outcomes from that starting position 

testAllStartingPosForGreen :: [Bool]
testAllStartingPosForGreen = concat [testStrategy g G | g <- startingPositionsForGreen]

testStrategy :: Board -> Field -> [Bool]
testStrategy g p
                | wins (next p) g = [False]
                | wins p g || full g = [True]
                | otherwise = concat [testStrategy x p | x <- g']
                        where g' = validNextMoves (bestmove g p) (next p)


