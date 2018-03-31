{- 
    Kimberly Keller
    CS 357 
    Homework 1 
    2/9/18
-}


test::Int->Int->Bool
test m n = if m `mod` 2 /= 0 && n `mod` 2 /= 0 
           then True 
           else False

stutter::[Char]->[Char]
stutter [] = []
stutter (x:xs) = x:x:stutter xs

compress::[Char]->[Char]
compress [] = []
compress (x:y:xs) = x:compress xs

zipSum::[Int]->[Int]->[Int]
zipSum [] (x:xs) = (x:xs)
zipSum (x:xs) [] = (x:xs)
zipSum [] [] = []
zipSum (y:ys) (x:xs) = (y + x):zipSum ys xs
                   

{- 
    Helper functions: 
        Remove duplicates
        Sort list
        Get duplicates
    I made these to help me manipulate lists for the Set functions. I decided 
    to create a sort function, because while I knew each list was increasing I wasn't
    sure if the first list or the second list would contain larger values. So now the
    output is always sorted regardless of which list comes first. 
-}

sortList::[Integer]->[Integer]
sortList [] = []
sortList (x:xs) = sortList lessThan ++ equal ++ sortList greaterThan
                  where
                    lessThan = [y | y<-xs, y<x]
                    equal   = [y | y<-xs, y==x] ++ [x]
                    greaterThan = [y | y<-xs, y>x]


rmDuplicates::[Integer]->[Integer]
rmDuplicates [] = []
rmDuplicates (x:xs) = if x `elem` xs 
                      then rmDuplicates xs 
                      else x:rmDuplicates xs


getDuplicates::[Integer]->[Integer]
getDuplicates [] = []
getDuplicates (x:xs) = if x `elem` xs
                       then x:getDuplicates xs
                       else getDuplicates xs

-- Set Union
setUnion::[Integer]->[Integer]->[Integer]
setUnion [] [] = []
setUnion _ [] = []
setUnion [] _ = []
setUnion ys xs = (sortList.rmDuplicates)(ys ++ xs)


-- Set Intersection
setIntersection::[Integer]->[Integer]->[Integer]
setIntersection [] [] = []
setIntersection _ [] = []
setIntersection [] _ = []
setIntersection ys xs = (sortList.getDuplicates)(ys ++ xs)


-- Set Difference
setDifference::[Integer]->[Integer]->[Integer]
setDifference [] [] = []
setDifference _ [] = []
setDifference [] _ = []
setDifference (x:xs) ys = if x `notElem` ys
                          then x:setDifference xs ys
                          else setDifference xs ys


--Set Equal
setEqual::[Integer]->[Integer]->Bool
setEqual [] [] = True
setEqual _ [] = False
setEqual [] _ = False
setEqual (x:xs) (y:ys) = if x == y 
                         then setEqual xs ys
                         else False


--Digital Root
mod10::Integer->[Integer]
mod10 n 
    | n < 1 = []
    | otherwise = mod10(n `div` 10) ++ [ n `mod` 10 ]


dr::Integer -> Int
dr n
    | n > 9 = dr ( (sum.mod10) n )
    | n <= 9 = fromIntegral n 


{- 
    This rootSum takes an Integer, converts it into a String, uses the
    fact that a String is a list of characters to map the Read function onto
    each character, turning each element back into an integer. Then it sums all the integers.
    The digital root function uses this to recursively sum until we have a single digit. 

    ...This way of doing this is weird and stupid, but I wanted to see if I could do it,
        so I did. 
-}

rootSum::Integer->Integer
rootSum n = sum(map (\x -> read [x]::Integer) (show n))

drTest::Integer->Int
drTest n 
        | n > 9 = dr (rootSum n) 
        | n <= 9 = fromIntegral n

{- 
    I wrote some additional functions that use list comprehensions instead of
    explicit recursion. I did this for my own benefit, but I figured I would
    include them here in addition to the original, more naive, functions I wrote previously.
-}

setIntersectionListComp::[Integer]->[Integer]->[Integer]
setIntersectionListComp ys xs = sortList(list)
                                    where 
                                        list = [ x | x <- xs, y <- ys, x==y ] 


setUnionListComp::[Integer]->[Integer]->[Integer]
setUnionListComp ys xs = sortList(list)
                            where 
                                list = ys ++ [ x | x <- xs, x `notElem` ys]


setDifferenceListComp::[Integer]->[Integer]->[Integer]
setDifferenceListComp xs ys = [ x | x <- xs, x `notElem` ys] 






