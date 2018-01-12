import Data.List

sortDesc :: Ord a => [a] -> [a]
sortDesc xs = (reverse . sort ) xs


sortDescPF :: Ord a => [a] -> [a]
sortDescPF = (reverse . sort )

are2FunsEqAt :: Eq a => (t -> a) -> (t->a) -> [t] -> Bool
are2FunsEqAt f g xs = length [(f x== g x)| x <- xs, (f x == g x) /= True] == 0

infixl 9 >.>
(>.>) :: (a->b) -> (b->c) -> (a -> c)
g >.> f = f . g

composeFunList :: [a->a] -> (a->a)
composeFunList [] = \x -> x
composeFunList (x:xs) = x . composeFunList xs