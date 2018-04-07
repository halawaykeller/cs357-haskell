module Homework4 where

--No other imports allowed
import qualified Data.List as L

--4.1 Genome Lists (40pts)
insertions :: String -> [String]
insertions = undefined

deletions :: String -> [String]
deletions = undefined

substitutions :: String -> [String]
substitutions = undefined

transpositions :: String -> [String]
transpositions = undefined

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
data Field = B | R | G
             deriving (Eq, Ord, Show)
type Board = [Field]

data Tree a = Node a [Tree a] deriving (Show)

-- given a board, produce all the possible moves for that board

size :: Int 
size = 3

next :: Field -> Field
next R = G 
next B = B 
next G = R

empty :: Board
empty = replicate (size^2) B

full :: Board -> Bool 
full = all (/= B) 

diag :: Board -> [Field]
diag g = [g !! n | n <- [0..size-1]]

wins :: Field -> Board -> Bool
wins p g = any line (rows ++ cols ++ dias)
            where 
                grid = chop 3 g
                line = all (==p)
                rows = grid
                cols = L.transpose grid
                dias = [diag g, diag (reverse g)]

won :: Board -> Bool
won g = wins R g || wins G g

valid :: Board -> Int -> Bool
valid g i = 0 <= i && i < size^2 && g !! i == B

move :: Board -> Int -> Field -> [Board]
move g i p = if valid g i then chop size (xs ++ [p] ++ ys) else [] 
                    where (xs,B:ys) = splitAt i g

chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)


moves :: Board -> Field -> [Board]
moves g p
    | won g = []
    | full g = []
    | otherwise = concat [move g i p | i <- [0..((size^2)-1)]]


gametree :: Board -> Field -> Tree Board
gametree g p = Node g [gametree g' (next p) | g' <- moves g p]


strategyForRed :: Board -> Int
strategyForRed = undefined

strategyForGreen :: Board -> Int
strategyForGreen = undefined

--4.4 (Optional) Drawing Game Trees and Strategies (30pts EC)
drawStrategy :: Bool -> String -> IO ()
drawStrategy = undefined
