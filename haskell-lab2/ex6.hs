fib :: (Num a, Eq a) => a -> a
fib n = 
    if n ==0 || n == 1 then n 
    else fib (n-2) + fib (n-1)

fibs = 0 : 1 : zipWith (+) fibs (tail fibs) :: [Int]

fib2 p = fibs !! p   

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

prod' :: Num a => [a] -> a
prod' [] = 0
prod' (x:[]) = x
prod' (x:xs) = x * prod' xs

length' :: [a] -> Double
length' [] = 0
length' (x:xs) = 1+length' xs

or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) = x || (or' xs)

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && (and' xs)

elem' :: Eq a => a -> [a] -> Bool
elem' p [] = False
elem' p (x:xs) = p==x || elem' p xs

doubleAll :: Num t => [t] -> [t]
doubleAll [] = []
doubleAll (x:xs) = [x*2] ++ doubleAll xs

squareAll :: Num t => [t] -> [t]
squareAll [] = []
squareAll (x:xs) = [x^2] ++ squareAll xs

selectEven :: Integral t => [t] -> [t] 
selectEven [] = []
selectEven (x:xs) = (if x `mod` 2 == 0 then [x] else []) ++ selectEven xs

sum'2 :: Num a =>[a] ->a
sum'2 xs = loop 0 xs
    where 
        loop acc [] = acc
        loop acc (x:xs) = loop (x + acc) xs

prod'2 :: Num a => [a] -> a
prod'2 xs = loop 1 xs
    where
        loop acc [] = acc
        loop acc (x:xs) = loop (acc*x) xs

length'2 :: [a] -> Int
length'2 xs = loop 0 xs
    where
        loop acc [] = acc
        loop acc (x:xs) = loop (acc+1) xs

sum'3 :: Num a => [a] -> a
sum'3 = loop 0
    where 
        loop acc [] = acc
        loop acc (x:xs) = loop (x + acc) xs