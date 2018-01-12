import Data.Char

isPalindrome :: [Char] -> Bool
isPalindrome xs = if (xs ==reverse (xs)) then True else False

getElemAtId :: [Char] -> Int -> Char
getElemAtId xs n = head (drop n xs)

primes :: [Int]
primes = eratoSieve[2..]
    where 
        eratoSieve :: [Int]->[Int]
        eratoSieve (p : xs) = p : eratoSieve [x| x<-xs, x `mod` p /= 0]

allEqual :: Eq a => [a] -> Bool
allEqual (p:xs) = [x | x <-xs, x /=p] ==[] 

isPrime p = [x| x<-(take p primes),x<=p,  x==p] == [p] 