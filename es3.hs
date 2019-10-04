sgn :: Int -> Int
sgn n = if n < 0
    then -1
    else if n == 0
        then 0
        else 1

absInt :: Int -> Int
absInt x = if x < 0
    then -x
    else x

min2Int :: (Int, Int) -> Int
min2Int (x,y) = if x < y
    then x
    else y

min3Int :: (Int, Int, Int) -> Int
min3Int (x,y,z) = if x < min2Int (y,z)
    then x
    else min2Int (y,z)

toUpper :: Char -> Char
toUpper x = if x >= 'a' && x <='z'
    then toEnum(fromEnum(x)-32)::Char
    else x

toLower :: Char -> Char
toLower x = if x >= 'A' && x <= 'Z'
    then toEnum(fromEnum(x)+32)::Char
    else x

isDigit :: Char -> Bool
isDigit x = if fromEnum(x) >= 48 && fromEnum(x) <=57
    then True
    else False

charToNum :: Char -> Int
charToNum x = if x <= '9' && x >= '0'
    then fromEnum(x) - 48
    else -1

romanDigit :: Char -> String
romanDigit x = if x == '1'
    then "I"
    else if x == '2'
        then "II"
    else if x == '3'
        then "III"
    else if x == '4'
        then "IV"
    else if x == '5'
        then "V"    
    else if x == '6'
        then "VI"
    else if x == '7'
        then "VII"
    else if x == '8'
        then "VIII"
    else if x == '9'
        then "IX"   
    else ""  