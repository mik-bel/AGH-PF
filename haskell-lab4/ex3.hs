data BinIntTree = EmptyIntBT |
                  IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt


data BinTree a= EmptyBT |
                NodeBT a (BinTree a) (BinTree a)
                deriving (Show)

sumBinTree :: (Num a) => BinTree a-> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt

data Expr a = Lit a |
              Add (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n) = n
eval (Add e1 e2) = eval e1 + eval e2 

show' :: Show a => Expr a -> String
show' (Lit n) = show n
show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"

depthOfBT :: BinTree a -> Int -- głębokość drzewa binarnego
depthOfBT (EmptyBT)= 0
depthOfBT (NodeBT a t1 t2) = 1 + max (depthOfBT t1) (depthOfBT t2)


flattenBTpr :: BinTree a -> [a]  -- napisać trzy wersje: preorder, inorder, postorder
flattenBTpr EmptyBT = []
flattenBTpr (NodeBT a t1 t2) = [a] ++ flattenBTpr t1 ++ flattenBTpr t2

flattenBTin :: BinTree a -> [a]  -- napisać trzy wersje: preorder, inorder, postorder
flattenBTin EmptyBT = []
flattenBTin (NodeBT a t1 t2) = flattenBTin t1 ++ [a] ++ flattenBTin t2

flattenBTpo :: BinTree a -> [a]  -- napisać trzy wersje: preorder, inorder, postorder
flattenBTpo EmptyBT = []
flattenBTpo (NodeBT a t1 t2) = flattenBTpo t1 ++ flattenBTpo t2 ++ [a]

mapBT :: (a -> b) -> BinTree a -> BinTree b -- funkcja map dla drzewa binarnego
mapBT f EmptyBT = EmptyBT
mapBT f (NodeBT a lt rt) = (NodeBT (f a)) (mapBT f lt) (mapBT f rt)

insertND :: Ord a => a -> BinTree a -> BinTree a -- insert element into BinTree
-- Przyjmiemy ze jest to BST
insertND a EmptyBT = NodeBT a EmptyBT EmptyBT
insertND a (NodeBT s lt rt) = if a > s 
                                then NodeBT s lt (insertND a rt) 
                                else NodeBT s (insertND a lt) rt


list2BST :: Ord a => [a] -> BinTree a -- list to Binary Search Tree (BST)
list2BST x =  foldl (\x y -> insertND y x ) EmptyBT x

instance Eq a => Eq (BinTree a) where
    (==) (EmptyBT) (EmptyBT) = True
    (==) (NodeBT s1 lt1 rt1) (NodeBT s2 lt2 rt2) = (s1 == s2) && (lt1 == lt2) && (rt1 == rt2)