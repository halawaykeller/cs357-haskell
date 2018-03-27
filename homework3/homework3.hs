--No Other Imports Are Allowed
import Data.List

--3.1 Lists And Trees (10pts)
data Tree a = LeafT a | NodeT (Tree a) (Tree a) deriving (Eq, Show)

splitList :: [a] -> ([a], [a])
splitList xs = splitAt n xs 
                where n = (length xs) `div` 2


balance :: [a] -> Tree a
balance [a] = LeafT a
balance xs = NodeT l r 
                where l = (balance . fst) ns 
                      r = (balance . snd) ns 
                      ns = splitList xs

--3.2 Simple Functions On Numbers (10pts)
goldbach :: Int -> [(Int,Int)]
goldbach = undefined

--3.3 Higher-Order Functions (10pts)
church :: Int -> (c -> c) -> c -> c
church n f z = foldr(\x y -> f y) z [1..n]

--3.4 Recursive Functions Over Lists (10pts)
type Set = [Int]

-- Credit to Graham Hutton on page 114, the function "subs" is also the powerset
powerset :: [Int] -> [[Int]]
powerset [] = [[]]
powerset (x:xs) = yss ++ map (x:) yss
                  where yss = powerset xs

--3.5 Lists And Strings (10pts)
example :: [[(Double, Double)]]
example = [[(100.0,100.0),(100.0,200.0),(200.0,100.0)],
  [(150.0,150.0),(150.0,200.0),(200.0,200.0),(200.0,150.0)]]

makeCommand :: [[(Double, Double)]] -> String
makeCommand = undefined

--3.6 Trees (25pts)
data T = Leaf | Node T T deriving (Eq, Show)

data P = GoLeft P | GoRight P | This deriving (Eq, Show)

allpaths :: T -> [P]
allpaths = undefined

--3.7 Logic (25pts)
type Expr = [[Int]]

eval :: (Int -> Bool) -> Expr -> Bool
eval = undefined

satisfiable :: Expr -> Bool
satisfiable = undefined