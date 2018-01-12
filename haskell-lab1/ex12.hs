addC :: Num a => a -> a -> a -> a
addC x y z = x + y - z

add1To :: Num a => a -> a -> a
-- partially applied function
add1To = addC 1
add1and2To :: Num a => a -> a
add1and2To = add1To 4
-- = addC 1 2
add1and2And3 :: Num a => a
add1and2And3 = add1and2To 3
-- = addC 1 2 3

--add1and2And3 = add1and2To 3 = add1To 4 3 = addC 1 4 3 

flip :: (a -> b -> c) -> b -> a -> c
flip (a, b, c) = (b, a, c)