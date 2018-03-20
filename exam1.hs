product'::[Int]->Int
product' [] = 1
product' (x:xs) = x*product' xs

qsort::[Int]->[Int]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
                    where 
                        smaller = [y | y <- xs, y < x]
                        larger = [z | z <- xs, z > x]


reverse'::[Int]->[Int]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]


take'::Int->[Int]->[Int]
take' _ [] = []
take' 0 _ = []
take' n (x:xs) = x:take' (n-1) xs

drop'::Int->[Int]->[Int]
drop' _ [] = []
drop' 0 xs = xs
drop' n (x:xs) = drop' (n-1) xs 

length'::[Int]->Int
length' [] = 0
length' (x:xs) = 1 + length' xs

concat'::[Int]->[Int]->[Int]
concat' [] ys = ys
concat' (x:xs) ys = x:concat' xs ys

-- init'::[Int]->[Int]
-- init' [] = []
-- init' xs = reverse ( (drop(1).reverse) xs )

init'::[Int]->[Int]
init' [x] = []
init' (x:xs) = x : init' xs

last'::[Int]->Int
last' [x] = x
last' (x:xs) = last' xs

null'::[Int]->Bool
null' [] = True
null' _ = False

tail'::[Int]->[Int]
tail' [] = []
tail' (x:xs) = xs

intersperse'::a->[a]->[a]
intersperse' _ [] = []
intersperse' n (x:xs) = x:n:intersperse' n xs

second::[Int]->Int
second xs = head(tail xs)

swap::(a,b)->(b,a)
swap (x,y) = (y,x)

pair::a->b->(a,b)
pair x y = (x,y)

double::Int->Int
double x = x*2

palindrome:: Eq a => [a]->Bool
palindrome xs = reverse xs == xs

twice:: (a->a)->a->a
twice f x = f (f x)

halve::[a]->([a], [a])
halve [] = ([], [])
halve xs = (take n xs, drop n xs) where n = (length xs) `div` 2

sumdown::Int->Int
sumdown 0 = 0
sumdown n = n + sumdown(n-1)

toPower::Int->Int->Int
toPower 1 _ = 1
toPower 0 _ = 1
toPower _ 0 = 1
toPower n 1 = n
toPower n m = n * toPower n (m-1)



isOrdered::Ord a => [a]->Bool
isOrdered xs =  if [ x | (x,y) <- zip(tail xs) xs, x < y ] == [] 
                then True
                else False 


inits'::[a]->[[a]]
inits' [] = [[]]
inits' (x:xs) = []:map (x:) (inits' xs)





