data Shape = Circle Float | Rect Float Float

square :: Float -> Shape
square n = Rect n n

area :: Shape -> Float
area(Circle r) = pi * r^2 where pi = 3.14
area(Rect x y) = x + y

square1 :: Shape
square1 = Rect 3 4

newtype Nat = N Int
data Nat = Zero | Succ Nat

f :: Nat -> Int
f n = Nat n