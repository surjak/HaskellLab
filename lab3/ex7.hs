onlyEven [] = []
onlyEven (x:xs)
 | x `mod` 2 == 0 = x : onlyEven xs
 | otherwise      = onlyEven xs

onlyOdd [] = []
onlyOdd (x:xs)
 | x `mod` 2 /= 0 = x : onlyOdd xs
 | otherwise      = onlyOdd xs


onlyUpper [] = []
onlyUpper (x:xs)
    | x `elem` ['A'..'Z']   = x : onlyUpper xs
    | otherwise             = onlyUpper xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)  | p x = x : filter' p xs
                  | otherwise = filter' p xs

onlyEven'  = filter' even
onlyOdd'   = filter' odd
onlyUpper' = filter' (\x -> x `elem` ['A'..'Z'])

