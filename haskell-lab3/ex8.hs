import Data.Char

doubleElems []     = []
doubleElems (x:xs) = 2 * x : doubleElems xs

sqrElems [] = []
sqrElems (x:xs) = x ^ 2 : sqrElems xs

lowerCase [] = []
lowerCase (x:xs) = toLower x : lowerCase xs

map' :: (a->b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs


