sqr :: Double -> Double
sqr x = x * x

vec2DLen :: (Double, Double) -> Double
vec2DLen (x,y) = sqrt(sqr(x)+sqr(y))

swap :: (Char, Int) -> (Int, Char)
swap (a,b) = (b,a)

threeErual :: (Int, Int, Int) -> Bool 
threeErual (a, b, c) = (a == b) && (b == c)

sgn :: Int -> Int
sgn n = 
    if n < 0
        then -1
        else
            if n==0 
                then 0
                else 1

abbs :: Int -> Int
abbs n = 
    if n>=0 
        then n
        else -n

min2Int :: (Int, Int) -> Int
min2Int (a, b) = 
    if (a > b)
        then b 
        else a 

min3Int :: (Int, Int, Int) -> Int
min3Int (a, b, c) =  min2Int (min2Int(a, b), c)

toUpper :: Char -> Char
toUpper a = 
    if (fromEnum (a) <= fromEnum 'z' && fromEnum a >= fromEnum 'a')
        then toEnum (fromEnum a - fromEnum 'a' + fromEnum 'A')
        else a

toLower :: Char -> Char
toLower a =
    if (fromEnum a <= fromEnum 'Z' && fromEnum a >= fromEnum 'A')
        then toEnum (fromEnum a - fromEnum 'A' + fromEnum 'a')
        else a

isDigit :: Char -> Bool
isDigit a =
    if (fromEnum a <= fromEnum '9' && fromEnum a >= fromEnum '0')
        then True
        else False 


charToNum :: Char -> Int
charToNum c = if isDigit c
                then toEnum (fromEnum c - fromEnum '0')
                else 0

romanDigit :: Char -> String
romanDigit n = 
    if (isDigit n)
        then
            ["0", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"] !! (fromEnum n - fromEnum '0')
        else
            ""
