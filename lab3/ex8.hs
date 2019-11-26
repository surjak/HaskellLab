import Data.Char 

doubleElems []     = []
doubleElems (x:xs) = 2 * x : doubleElems xs

sqrElems :: Num a => [a] -> [a]
sqrElems []     = []
sqrElems (x:xs) = x ^ 2 : sqrElems xs

lowerCase :: [Char] -> [Char]
lowerCase []     = []
lowerCase (x:xs) = toLower x : lowerCase xs

map' :: (a -> b) -> [a] -> [b]
map' f (x:xs) = f x : map' f xs

doubleElems' = map' (*2)

sqrElems'    = map' (^2)

lowerCase'   = map' toLower


evalFuncListAt :: a -> [a -> b] -> [b]
evalFuncListAt x = map ($ x)