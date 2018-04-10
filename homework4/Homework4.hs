module Homework4 where

--No other imports allowed
import qualified Data.List as L

--4.1 Genome Lists (40pts)

bases :: String
bases = "AGCT"

-- From the countdown problem in section 9.4
interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

insertions :: String -> [String]
insertions xs = concat [interleave m xs | m <- bases]

deletions :: String -> [String]
deletions xs = [L.delete m xs | m <- xs]

replace :: Eq a => a -> a -> [a] -> [a]
replace n m xs = map (\x -> if (n == x) then m else x) xs

substitutions :: String -> [String]
substitutions xs = [replace n m xs | n <- xs, m <- bases]

transpositions :: String -> [String]
transpositions [] = []
transpositions (x:y:[]) = [y:x:[]]
transpositions (x:y:xs) = [y:x:xs] ++ map (x:) (transpositions (y:xs))

--4.2 Sorting (20pts)
insert :: Ord a => a -> [a] -> [a]
insert n [] = [n]
insert n (x:xs) 
        | n <= x = n:x:xs
        | n > x = x:(insert n xs)

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

sortFile :: String -> String
sortFile f = (unlines . isort . lines) f

fileisort :: String -> String -> IO ()
fileisort fn1 fn2 = do 
                    file1 <- readFile fn1
                    writeFile fn2 (sortFile file1)

--4.3 Game Trees (40pts)

data Field = B | R | G deriving (Eq, Ord, Show)

type Board = [Field]

data Tree a = Node a [Tree a] deriving (Show)

-- given a board, produce all the possible moves for that board

size :: Int 
size = 3

next :: Field -> Field
next R = G 
next B = B 
next G = R

turn :: Board -> Field
turn g = if os <= xs then G else R
            where 
                os = length (filter (== G) ps) 
                xs = length (filter (== R) ps) 
                ps = g

empty :: Board
empty = replicate (size^2) B

full :: Board -> Bool 
full = all (/= B) 

diag :: [Board] -> [Field]
diag g = [g !! n !! n | n <- [0..size-1]]

wins :: Field -> Board -> Bool
wins p g = any line (rows ++ cols ++ dias)
            where 
                grid = chop 3 g
                line = all (==p)
                rows = grid
                cols = L.transpose grid
                dias = [diag grid, diag (map reverse grid)]

won :: Board -> Bool
won g = wins R g || wins G g

valid :: Board -> Int -> Bool
valid g i = 0 <= i && i < size^2 && g !! i == B

move :: Board -> Int -> Field -> Board
move g i p = if valid g i then xs ++ [p] ++ ys else [] 
                    where (xs,B:ys) = splitAt i g

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

depth :: Int
depth = 9

moves :: Board -> Field -> [Board]
moves g p
    | won g = []
    | full g = []
    | otherwise = [move g i p | i <- [0..(size^2)-1]]


gametree :: Board -> Field -> Tree Board
gametree g p = Node g [gametree g' (next p) | g' <- moves g p, g' /= []]

prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x [prune (n-1) t | t <- ts]


countNodes :: Tree a -> Int
countNodes (Node _ []) = 1
countNodes (Node _ xs) = 1 +  (sum (map countNodes xs))

minimax :: Tree Board -> Tree (Board, Field)
minimax (Node g []) 
            | wins G g = Node (g, G) []
            | wins R g = Node (g, R) []
            | otherwise = Node (g, B) []
minimax (Node g xs) 
            | turn g == G = Node (g, minimum ps) xs'
            | turn g == R = Node (g, maximum ps) xs'
                    where 
                        xs' = map minimax xs
                        ps = [p | Node (_,p) _ <- xs']            
                    
-- bestmove :: Board -> Field -> Board
-- bestmove g p = if board == [] then g else head board
--                     where 
--                         tree = prune depth (gametree g p)
--                         Node (_, best) ts = minimax tree
--                         board = [g' | Node (g', p') _ <- ts, p' == best]

bestmove :: Board -> Field -> Board
bestmove g p = head board
                    where 
                        tree = prune depth (gametree g p)
                        Node (_, best) ts = minimax tree
                        board = [g' | Node (g', p') _ <- ts, p' == best]                        

bestStrategy :: Board -> Field -> Int
bestStrategy g p = head [m | m <- [0..((size^2)-1)], (g !! m) /= (b !! m)]
                    where 
                        b = bestmove g p


strategyForRed :: Board -> Int
strategyForRed g =  bestStrategy g R

strategyForGreen :: Board -> Int
strategyForGreen g = bestStrategy g G


-- Start by taking an empty board
-- Make the best move for Red and return the board
-- Make the next valid move for Green and return the board

-- validNextMove :: Board -> Field -> Board
-- validNextMove (g:gs) p
--             | valid (g:gs) i = g:move (g:gs) i p
--             | otherwise = validNextMove gs p
--                 where i = 2 --((size^2)-1) - (length (g:gs))


validNextMove :: Board -> Field-> Board
validNextMove gs p = head [move gs i p | i <- [0..((size^2)-1)], valid gs i]

validNextMoves :: Board -> Field-> [Board]
validNextMoves gs p = [move gs i p | i <- [0..((size^2)-1)], valid gs i]


-- validNextMove :: Board -> Field-> Board
-- validNextMove gs p = if validB == [] then gs else validB
--                         where validB = head [move gs i p | i <- [0..((size^2)-1)], valid gs i]
                

-- WORKING
-- testStrategyForRed :: Board -> Field -> Bool
-- testStrategyForRed g p
--                 | wins G g = False
--                 | wins R g || full g = True
--                 | otherwise = testStrategyForRed g' p
--                         where g' = validNextMove (bestmove g R) G


-- Get all the valid next moves
-- Test strategy for red on each valid next move
-- recurse


-- Pass an empty board
-- Make the best move for Red
-- Get a list of all the boards with valid next moves for green
-- Map TestStrategy onto all the possible valid boards
-- Make the best move for red based on that board

-- testStrategyForRed :: [Board] -> Field -> Bool
-- testStrategyForRed gs p
--                 | and (map (wins G) gs) = False
--                 | and (map (wins R) gs) || and (map full gs) = True
--                 | otherwise = and [testStrategyForRed x p | x <- gs']
--                         where gs' = map (flip validNextMoves G) ms
--                               ms = map (flip bestmove R) gs

-- testStrategyForRed :: [Board] -> Field -> [Board]
-- testStrategyForRed gs p
--                 | and (map (wins G) gs) = gs
--                 | and (map (wins R) gs) || and (map full gs) = gs
--                 | otherwise = concat [testStrategyForRed x p | x <- gs']
--                         where gs' = map (flip validNextMoves G) ms
--                               ms = map (flip bestmove R) gs                              


testStrategyForRed :: Board -> Field -> Board
testStrategyForRed g p
                | wins G g = g
                | wins R g || full g = g
                | otherwise = testStrategyForRed g' p
                        where g' = validNextMove (bestmove g R) G


getLabels :: Tree (Board, Field)  -> [Field]
getLabels (Node (g, p) []) = if (p == R) then [] else [p]
getLabels (Node _ xs) = concat (map getLabels xs)

playgame :: Board -> Field -> Board
playgame g p
        | full g = g
        | otherwise = playgame (bestmove g p) (next p)


--4.4 (Optional) Drawing Game Trees and Strategies (30pts EC)
drawStrategy :: Bool -> String -> IO ()
drawStrategy = undefined
