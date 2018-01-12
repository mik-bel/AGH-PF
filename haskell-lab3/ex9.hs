map' :: Num a => (a->a) -> [a] -> [a] 
map' f x = foldl (\acc y -> acc ++ [f y]) [] x

map''::  Num a => (a->a) -> [a] -> [a]
map'' f x = foldr (\y acc -> f y : acc) [] x

concat' :: [[a]] -> [a]
concat' []  =[]
concat' (x:xs) = x ++ concat' xs