absInt :: Int -> Int
absInt x | x >= 0 = x
        | otherwise = -x

sgn :: Int -> Int
sgn x | x < 0 = -1
    | x == 0 = 0
    | otherwise = 1

min3Int :: (Int, Int, Int) -> Int
min3Int (x,y,z) | x <=y && x <=z = x
                | y <= x && y <= z = y
                | otherwise = z

toUpper :: Char-> Char
toUpper x | x <= 'z' && x >= 'a' = toEnum(fromEnum(x) - 32)
        | otherwise = x