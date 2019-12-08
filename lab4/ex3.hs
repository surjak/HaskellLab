data BinIntTree = EmptyIntBT |
                  IntNodeBT Int BinIntTree BinIntTree

sumBinIntTree :: BinIntTree -> Int
sumBinIntTree EmptyIntBT = 0
sumBinIntTree (IntNodeBT n lt rt) = n + sumBinIntTree lt + sumBinIntTree rt



data BinTree a = EmptyBT |
                 NodeBT a (BinTree a) (BinTree a)

sumBinTree :: (Num a) => BinTree a -> a
sumBinTree EmptyBT = 0
sumBinTree (NodeBT n lt rt) = n + sumBinTree lt + sumBinTree rt

-- data Expr a = Lit a | 
--               Add (Expr a) (Expr a)

-- eval :: Num a => Expr a -> a
-- eval (Lit n) = n
-- eval (Add e1 e2) = eval e1 + eval e2

-- show' :: Show a => Expr a -> String
-- show' (Lit n) = show n
-- show' (Add e1 e2) = "(" ++ show' e1 ++ "+" ++ show' e2 ++ ")"

depthOfBT :: BinTree a -> Int
depthOfBT EmptyBT = 0
depthOfBT (NodeBT _ b c) = 1 + max (depthOfBT b) (depthOfBT c)

flattenBTpreorder :: BinTree a -> [a]
flattenBTpreorder EmptyBT = []
flattenBTpreorder (NodeBT a b c) = [a] ++ flattenBTpreorder b ++ flattenBTpreorder c

flattenBTinorder :: BinTree a -> [a]
flattenBTinorder EmptyBT = []
flattenBTinorder (NodeBT x a b) = flattenBTinorder a ++ [x] ++ flattenBTinorder b

flattenBTpostorder :: BinTree a -> [a]
flattenBTpostorder EmptyBT = []
flattenBTpostorder (NodeBT x a b) = flattenBTpostorder a ++ flattenBTpostorder b ++ [x]

mapBT :: (a -> b) -> BinTree a -> BinTree b
mapBT _ EmptyBT = EmptyBT
mapBT f (NodeBT x a b) = NodeBT (f x) (mapBT f a) (mapBT f b)



insert :: Ord a => a -> BinTree a -> BinTree a
insert x EmptyBT = NodeBT x EmptyBT EmptyBT
insert x (NodeBT a l r) = if x > a
                            then NodeBT a l (insert x r)
                            else NodeBT a (insert x l) r


list2BST :: Ord a => [a] -> BinTree a
list2BST = loop EmptyBT
    where loop acc []     = acc
          loop acc (x:xs) = loop (insert x acc) xs

data Expr a = Lit a |
                Add (Expr a) (Expr a) |
                Subtract (Expr a) (Expr a) |
                Multiply (Expr a) (Expr a)

eval :: Num a => Expr a -> a
eval (Lit n)           = n
eval (Add e1 e2)       = eval e1 + eval e2
eval (Subtract e1 e2) = eval e1 - eval e2
eval (Multiply e1 e2)  = eval e1 * eval e2