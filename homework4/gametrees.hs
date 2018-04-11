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

-- -- Displaying a Board

-- putBoard :: Board -> IO ()
-- putBoard =
--    putStrLn . unlines . concat . interleave bar . map showRow
--    where bar = [replicate ((size*4)-1) '-']

-- showRow :: [Field] -> [String]
-- showRow = beside . interleave bar . map showField
--           where
--              beside = foldr1 (zipWith (++))
--              bar    = replicate 3 "|"

-- showField :: Field -> [String]
-- showField R = ["   ", " R ", "   "]
-- showField B = ["   ", "   ", "   "]
-- showField G = ["   ", " G ", "   "]

-- interleave :: a -> [a] -> [a]
-- interleave x []     = []
-- interleave x [y]    = [y]
-- interleave x (y:ys) = y : x : interleave x ys

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

-- -- Reading a natural number

-- getNat :: String -> IO Int
-- getNat prompt = do putStr prompt
--                    xs <- getLine
--                    if xs /= [] && all isDigit xs then
--                       return (read xs)
--                    else
--                       do putStrLn "ERROR: Invalid number"
--                          getNat prompt

-- -- Human vs human

-- tictactoe :: IO ()
-- tictactoe = run empty R 

-- run :: Board -> Field -> IO ()
-- run g p = do cls
--              goto (1,1)
--              putBoard g
--              run' g p

-- run' :: Board -> Field -> IO ()
-- run' g p | wins R g  = putStrLn "Field R wins!\n"
--          | wins G g  = putStrLn "Field G wins!\n"
--          | full g    = putStrLn "It's a draw!\n"
--          | otherwise =
--               do i <- getNat (prompt p)
--                  case move g i p of
--                     []   -> do putStrLn "ERROR: Invalid move"
--                                run' g p
--                     [g'] -> run g' (next p)

-- prompt :: Field -> String
-- prompt p = "Field " ++ show p ++ ", enter your move: "

-- cls :: IO ()
-- cls = putStr "\ESC[2J"

-- goto :: (Int,Int) -> IO ()
-- goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

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

-- -- Human vs computer

-- main :: IO ()
-- main = do hSetBuffering stdout NoBuffering
--           play empty R

-- play :: Board -> Field -> IO ()
-- play g p = do cls
--               goto (1,1)
--               putBoard g
--               play' g p

-- play' :: Board -> Field -> IO ()
-- play' g p
--    | wins R g = putStrLn "Field R wins!\n"
--    | wins G g = putStrLn "Field G wins!\n"
--    | full g   = putStrLn "It's a draw!\n"
--    | p == R   = do i <- getNat (prompt p)
--                    case move g i p of
--                       []   -> do putStrLn "ERROR: Invalid move"
--                                  play' g p
--                       [g'] -> play g' (next p)
--    | p == G   = do putStr "Field G is thinking... "
--                    (play $! (bestmove g p)) (next p)
