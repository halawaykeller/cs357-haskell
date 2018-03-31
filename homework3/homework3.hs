module Homework3
( Tree(LeafT, NodeT),
  splitList, 
  balance, 
  factors, 
  prime, 
  primes, 
  goldbach, 
  church,  
  powerset, 
  example, 
  makeCommand,
  T(Leaf, Node),
  P(GoLeft, GoRight, This),
  allpaths,
  eval,
  satisfiable,
) where

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
-- get the primes from 2 to n where n is the target number

factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

prime :: Int -> Bool 
prime n = factors n == [1,n]

primes :: Int -> [Int]
primes n = [x | x <- [1..n], prime x]

goldbach :: Int -> [(Int,Int)]
goldbach n = [(x, y) | x <- primes (n `div` 2), y <- primes n, x + y == n]

--3.3 Higher-Order Functions (10pts)
church :: Int -> (c -> c) -> c -> c
church n f z = foldr(\x y -> f y) z [1..n]

--3.4 Recursive Functions Over Lists (10pts)
type Set = [Int]

-- Credit to Graham Hutton on page 114, the function "subs" is also the powerset, the logic 
-- in this version is the same, except I used a list comprehension just for kicks.
powerset :: [Int] -> [[Int]]
powerset [] = [[]]
powerset (x:xs) = yss ++ [x:n | n <- yss]
                  where yss = powerset xs                  

--3.5 Lists And Strings (10pts)
example :: [[(Double, Double)]]
example = [[(100.0,100.0),(100.0,200.0),(200.0,100.0)],
  [(150.0,150.0),(150.0,200.0),(200.0,200.0),(200.0,150.0)]]

-- getBoundingBox :: [[(Double, Double)]] -> String
-- getBoundingBox 

moveTo :: (Double, Double) -> String
moveTo (x, y) = show x ++ show y ++ " moveto \n"

lineTo :: (Double, Double) -> String
lineTo (x, y) = show x ++ show y ++ " lineto \n"

polygons :: [[(Double, Double)]] -> String
polygons [] = ""
polygons (x:xss) = (moveTo (head x) ++ concatMap lineTo (tail x)) ++ endPoly ++ polygons xss
                    where endPoly = "closepath\nstroke\n"

makeCommand :: [[(Double, Double)]] -> String
makeCommand xss = start ++ (polygons xss) ++ end
                        where 
                            start = "%!PS-Adobe-3.0 EPSF-3.0 \n"
                            end = "showpage\n%%EOF"

--3.6 Trees (25pts)
data T = Leaf | Node T T deriving (Eq, Show)

data P = GoLeft P | GoRight P | This deriving (Eq, Show)

allpaths :: T -> [P]
allpaths Leaf = [This]
allpaths (Node x y) = This : map GoLeft (allpaths x) ++ map GoRight (allpaths y)


--3.7 Logic (25pts)
type Expr = [[Int]]

checkVal :: (Int -> Bool) -> Int -> Bool
checkVal f n = if n < 0 then not (f (abs n)) else f n

eval :: (Int -> Bool) -> Expr -> Bool
eval f ess = and (map or (map (map (checkVal f)) ess))
                
p :: [Bool] -> Int -> Bool
p bs i = bs !! (i - 1)


process :: Expr -> [Int]
process e = nub $ map abs $ concat e

makeTable :: [Int] -> [[Bool]]
makeTable [] = []
makeTable [x] = [[True], [False]]
makeTable (x:xs) = [True:n | n <- r] ++ [False:w | w <- r]
                    where r = makeTable xs

satisfiable :: Expr -> Bool
satisfiable e = or [eval (p bs) e | bs <- table]
                    where table = (makeTable . process) e







