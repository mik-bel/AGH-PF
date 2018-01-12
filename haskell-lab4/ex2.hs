data CartInt2DVec = MkCartInt2DVec Int Int

xCoord :: CartInt2DVec -> Int
xCoord (MkCartInt2DVec x _) = x

yCoord :: CartInt2DVec -> Int
yCoord (MkCartInt2DVec _ y) = y

data Cart2DVec' a = MkCart2DVec' a a

xCoord' :: Cart2DVec' a -> a
xCoord' (MkCart2DVec' x _) = x

yCoord' :: Cart2DVec' a -> a
yCoord' (MkCart2DVec' _ y) = y

data Cart2DVec'' a = MkCart2DVec'' {x::a, y::a}

data List a = EmptyL | Cons a (List a) deriving Show

head' :: List a -> a
head' EmptyL = error "Head': the empty list has no head!"
head' (Cons x xs) = x


data Cart3DVec a = Cart3DVec a a a

xCoord3D :: Cart3DVec a -> a
xCoord3D (Cart3DVec x _ _) = x

yCoord3D :: Cart3DVec a -> a
yCoord3D (Cart3DVec _ y _) = y


zCoord3D :: Cart3DVec a -> a
zCoord3D (Cart3DVec _ _ z) = z

data Cart3DVec' a = Cart3DVec' {x' :: a, y' :: a, z' :: a}

data Shape = Circle Float | Rectangle Float Float 

area :: Shape -> Float
area (Circle r) = 3.14 * r * r 
area (Rectangle a b) = a * b 

data Tree a = EmptyT |
              Node a (Tree a) (Tree a)
              deriving Show

rootValue :: Tree a -> a
rootValue EmptyT = error "Empty tree hasn't got roots"
rootValue (Node a _ _) = a

data TrafficLights = Red |
                     Green |
                     Yellow

actionFor :: TrafficLights -> String
actionFor Red = "Stop!"
actionFor Green = "Go!"
actionFor Yellow = "Slow down!"