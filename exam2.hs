factor' :: Int -> [Int]
factor' n = [x | x <- [1..n], n `mod` x == 0]

prime' :: Int -> Bool
prime' n = factor' n == [1,n]

primes' :: Int -> [Int]
primes' n = [x | x <- [1..n], prime' x]

even' :: Int -> [Int]
even' n = [x | x <- [1..n], x `mod` 2 == 0]


odd' :: Int -> [Int]
odd' n = [x | x <- [1..n], x `mod` 2 == 1]