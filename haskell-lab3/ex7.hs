onlyEven [] = []
onlyEven (x:xs)
    | x `mod` 2 == 0 = x : onlyEven xs
    | otherwise      = onlyEven xs

onlyOdd [] = []
onlyOdd (x:xs)
    | x `mod` 2 /= 0 = x : onlyOdd xs
    | otherwise      = onlyOdd xs

onlyUpper [] = []
onlyUpper (x:xs)
    | fromEnum (x) < fromEnum ('Z') && fromEnum (x) > fromEnum ('A') = x : onlyUpper xs
    | otherwise = onlyUpper xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p [] = []
filter' p (x:xs) 
    | p x == True = x : filter' p xs
    | otherwise = filter' p xs

onlyEven' = filter' (\x -> x `mod` 2 == 0)
onlyOdd' = filter' (\x -> x `mod` 2 /= 0)
onlyUpper' =  filter' (\x ->  fromEnum (x) < fromEnum ('Z') && fromEnum (x) > fromEnum ('A'))

