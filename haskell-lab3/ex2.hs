sum' :: Num a => [a] -> a
sum' []     = 0
sum' (x:xs) = x + sum' xs

sumWith :: Num a => (a -> a) -> [a] -> a
sumWith f [] = 0
sumWith f (x:xs) = f x + sumWith f xs

listLength :: (Num a, Eq a) => [a] -> a
listLength xs = sumWith (\x -> 1) xs 

prod' :: Num a => [a] -> a
prod' [] = 1
prod' (x:xs) = x*prod' xs

prodWith :: Num a => (a -> a) -> [a] -> a
prodWith f [] = 1
prodWith f (x:xs) = f x * prodWith f xs

prod xs = prodWith (*1) xs

prodSqr xs = prodWith (^2) xs

prodCube xs = prodWith (^3) xs


with' :: (Num a, Eq a) => (a->a->a) -> (a->a) -> [a] -> a
with' g f (x:xs) = if (listLength xs == 0)
                    then f x
                    else (g) (f (x) ) (with' g f xs) 
