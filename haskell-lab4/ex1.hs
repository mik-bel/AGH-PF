import Data.Char

polarToCartesian :: Floating a => (a, a) -> (a, a)
polarToCartesian (r, phi) = (r * cos phi, r * sin phi)

type CartesianCoord' a = (a,a)
type PolarCoord' a = (a, a)

polarToCartesian' :: Floating a => PolarCoord' a -> CartesianCoord' a
polarToCartesian' (r, phi) = (r * cos phi, r * sin phi)

newtype CartesianCoord'' a = MkCartesianCoord'' (a,a)
newtype PolarCoord'' a = MkPolarCoord'' (a,a)

polarToCartesian'' :: Floating a => PolarCoord'' a -> CartesianCoord'' a
polarToCartesian'' (MkPolarCoord'' (r,phi)) = MkCartesianCoord'' (r * cos phi, r * sin phi)

personInfoToString :: (String,String,String) -> String
personInfoToString (nm,snm,addr) = 
    "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr

type Name' = String
type Surname' = String
type Address' = String
type PersonInfo' = (Name', Surname', Address')
type PersonInfoToStringType' = PersonInfo' -> String

personInfoToString' :: PersonInfoToStringType'
personInfoToString' (nm,snm,addr) = 
    "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr
    
newtype PersonInfo'' = MakePersonInfo'' (String, String, String)
personInfoToString'' :: PersonInfo'' -> String
personInfoToString'' (MakePersonInfo'' (nm,snm,addr)) = 
    "name: " ++ nm ++ ", surname: " ++ snm ++ ", addr: " ++ addr


newtype FirstName = FirstName String
formatFstName :: FirstName -> String
formatFstName (FirstName s) = case s of
    (x:xs) -> toUpper x : map toLower xs
    [] -> []
    

data Person n s = Person n s 