qSort :: Ord a => [a] -> [a]
qSort []     = []
qSort (x:xs) = qSort (leftPart xs) ++ [x] ++ qSort (rightPart xs)
 where
   leftPart  xs = filter (<=x) xs
   rightPart xs = filter (>x) xs

mSort :: Ord a => [a] -> [a]
mSort []  = []
mSort [x] = [x]
mSort xs  = merge (mSort left) (mSort right)
    where n = length xs `div` 2
          (left, right) = splitAt n xs
          merge [] ys = ys
          merge xs [] = xs
          merge (x:xs) (y:ys)
                        | x < y = x : merge xs (y:ys)
                        | otherwise = y : merge (x:xs) ys

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x < y     = x:y:ys
                | otherwise = y:(insert x ys)

iSort :: Ord a => [a] -> [a]
iSort [] = []
iSort (x:xs) = insert x (iSort xs)


concat' :: [[a]] -> [a]
concat' xs = [y |  x <- xs , y <- x]

concat'' :: [[a]] -> [a]
concat'' [] = []
concat'' (x:xs) = x ++ concat'' xs


isSorted :: [Int] -> Bool
isSorted [] = True
isSorted [a] = True
isSorted (x:y:xs) | x<= y = isSorted(y:xs)
                  |otherwise = False

reverse' :: [a] -> [a]
reverse' [] = []
reverse' [a] = [a]
reverse'(x:xs) = reverse' xs ++ [x]


zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip'(x:xs) (y:ys) = [(x,y)] ++ zip' xs ys

unzip' :: [(a, b)] -> ([a],[b])
unzip' = loop [] []
    where 
        loop acc1 acc2 [] = (acc1, acc2)
        loop acc1 acc2 ((x, y):xs) = loop (acc1 ++ [x]) (acc2 ++ [y]) xs

zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' _ _ [] = []
zip3' _ [] _ = []
zip3' [] _ _ = []
zip3'(x:xs) (y:ys) (z:zs) = [(x,y,z)] ++ zip3' xs ys zs


subList :: Eq a => [a] -> [a] -> Bool
subList [] _ = True
subList (x:xs) ys = x `elem` ys && subList xs ys