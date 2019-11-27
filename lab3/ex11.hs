concat' :: [[a]] -> [a]
concat' []     = []
concat' (x:xs) = x ++ concat' xs

concat'' :: [[a]] -> [a]
concat'' xs = [y  | x<-xs , y <- x]

concat''' :: [[a]] -> [a]
concat''' xs = foldr (++) [] xs