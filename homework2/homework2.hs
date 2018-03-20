{-
Kimberly Keller
kimberlykeller

2/27/18
Homework 2
CS 357

-}

-- module Homework2
-- ( collatz, 
--   haskellFileNames, 
--   select, 
--   prefixSum, 
--   numbers, 
--   Numeral, 
--   makeLongInt, 
--   evaluateLongInt, 
--   changeRadixLongInt, 
--   addLongInts, 
--   mulLongInts
-- ) where

import Data.List

--2.1


tupleBuilder :: Int -> ( Int, Int )
tupleBuilder n = (length(n : collatzHelper n), n )

collatzHelper :: Int -> [Int]
collatzHelper 1 = []
collatzHelper n 
              | odd n = 3*n + 1 : collatzHelper (3*n + 1)
              | even n = n `div` 2 : collatzHelper (n `div` 2)


collatz :: [Int] -> Int
collatz xs = snd.head.reverse.sort $ map tupleBuilder xs


--2.2

stripSpace :: String -> String
stripSpace [] = []
stripSpace (x:xs)
        | x == ' ' = stripSpace xs
        | otherwise = x:stripSpace xs

isHaskellFile :: String -> String
isHaskellFile xs
            | isSuffixOf ".hs" ns = xs
            | isSuffixOf ".lhs" ns = xs
            | otherwise = ""
                where ns = stripSpace xs

haskellFileNames :: [String] -> [String]
haskellFileNames xs = filter (\n -> n /= "") (map isHaskellFile xs)


--2.3
select :: (t -> Bool) -> [t] -> [a] -> [a]
select p [] [] = []
select p [] ys = []
select p xs [] = []
select p (x:xs) (y:ys) = if p x then y:select p xs ys else select p xs ys 
         

--2.4
prefixSum :: [Int] -> [Int]
prefixSum xs = tail (scanl (+) 0 xs)

--2.5
numbers :: [Int] -> Int
numbers xs = read $ foldr (++) [] (map show xs) :: Int

--2.6
type Numeral = (Int, [Int])

example = (10, [1,2,3,4,5,6,7,8,9,0,1,2,3,4,5,6,7,8,9,0])

--2.6 1
-- I stole this from myself, from HW 1 where I did this for Digial Root
intList::Integer->Int->[Integer]
intList n r
    | n < 1 = []
    | otherwise = intList(n `div` m ) r ++ [ n `mod` m] 
            where m = toInteger r

makeLongInt :: Integer -> Int -> Numeral
makeLongInt x r = (r, map fromIntegral $ intList x r)

--2.6 2

powerList :: Integer -> [Integer] -> [Integer]
powerList r [] = []
powerList r (x:xs) = x*r^n : powerList r xs where n = length xs 

evaluateLongInt :: Numeral -> Integer
evaluateLongInt (r, xs) =  sum $ map toInteger $ powerList z ys
                                                     where z = toInteger r
                                                           ys = map toInteger xs


--2.6 3
changeRadixLongInt :: Numeral -> Int -> Numeral 
changeRadixLongInt (r1, xs) r2 = (r2, stripAllLeadZeros(changeRadixHelper xs r1 r2 [0]))


changeRadixHelper :: [Int] -> Int -> Int -> [Int] -> [Int]
changeRadixHelper [] _ _ ns = ns
changeRadixHelper (x:xs) r1 r2 ns = changeRadixHelper xs r1 r2 num
                                        where num = adderNum( mult ns r1 r2) x r2


nestedList :: [[Int]] -> [Int]
nestedList xss = foldr (++) [] xss


--2.6 4

stripAllLeadZeros :: [Int] -> [Int]
stripAllLeadZeros (x:xs) = if x == 0 then stripAllLeadZeros xs else (x:xs)


addLeadingZeros :: [Int] -> Int -> [Int]
addLeadingZeros xs n = replicate n 0 ++ xs

stripLeadZero :: [Int] -> [Int]
stripLeadZero xs = if head xs == 0 then tail xs else xs

adder1 :: [Int] -> [Int] -> Int -> [Int]
adder1 xs [] _ = xs
adder1 [] ys _ = ys
adder1 (x:[]) (y:[]) r = (x + y) `div` r : [(x + y) `mod` r]
adder1 (x:xs) (y:ys) r = xn `div` r : xn `mod` r : ds
                   where (c:ds) = adder1 xs ys r
                         xn = x + y + c  


adder :: [Int] -> [Int] -> Int -> [Int]
adder xs ys r = stripLeadZero(adder1 xs ys r)

addLongInts :: Numeral -> Numeral -> Numeral
addLongInts (r1, xs) (r2, ys)
        | r1 == r2 = (r1, stripLeadZero( adder ds1 ds2 r1 ))
        | r1 < r2 = addLongInts (changeRadixLongInt (r1, ds1) r2) (r2, ds2)
        | r1 > r2 = addLongInts (r1, ds1) (changeRadixLongInt (r2, ds2) r1)
              where m = max (length xs) (length ys)
                    ds1 = addLeadingZeros xs (m - length xs)
                    ds2 = addLeadingZeros ys (m - length ys)                    


--2.6 5

adderNum :: [Int] -> Int -> Int -> [Int]
adderNum [] n r = []
adderNum xs 0 r = xs
adderNum (x:[]) n r = (x + n) `div` r : (x + n) `mod` r : []
adderNum (x:xs) n r = xn `div` r : xn `mod` r : tail v
                  where v = adderNum xs n r
                        xn = x  + head v 



mult1 :: [Int] -> Int -> Int -> [Int]
mult1 [] n r = []
mult1 xs 0 r = [0]
mult1 (x:[]) n r = (x * n) `div` r : (x * n) `mod` r : []
mult1 (x:xs) n r = xn `div` r : xn `mod` r : tail v
                  where v = mult1 xs n r
                        xn = (x * n) + head v   


mult :: [Int] -> Int -> Int -> [Int]
mult xs y r = stripLeadZero( mult1 xs y r)

multHelper :: [Int] -> [Int] -> Int -> [Int]
multHelper xs (y:[]) r = mult xs y r
multHelper xs (y:ys) r = adder a1' a2' r
                          where a1 = multHelper xs ys r
                                a2 = (mult xs y r) ++ (replicate (length ys) 0)
                                m = max (length a1) (length a2)
                                a1' = addLeadingZeros a1 (m - length a1)
                                a2' = addLeadingZeros a2 (m - length a2) 




mulLongInts :: Numeral -> Numeral -> Numeral
mulLongInts (r1, xs) (r2, ys)
          | r1 == r2 = (r1, multHelper xs ys r1)
          | r1 < r2 = mulLongInts (changeRadixLongInt (r1, xs) r2) (r2, ys)
          | r1 > r2 = mulLongInts (r1, xs) (changeRadixLongInt (r2, ys) r1)



