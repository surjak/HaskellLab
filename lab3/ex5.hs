import Data.List
sortDesc :: Ord a => [a] -> [a]
sortDesc xs =  (reverse . sort) xs


are2FunsEqAt :: Eq a => (t -> a) -> (t -> a) -> [t] -> Bool
are2FunsEqAt f g = all  (uncurry (==)) . map (\x -> (f x, g x)) 

infixl 9 >.>
(>.>) :: (a -> b) -> (b -> c) -> (a -> c)
g >.> f = f . g

composeFunList :: [a -> a] -> (a -> a)
composeFunList (x:xs) = x . composeFunList xs

func = ((.).(.)) (^2) (*)