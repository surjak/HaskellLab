not' :: Bool -> Bool
not' b = case b of
    True -> False
    False -> True

absInt :: Int -> Int
absInt x = case x >= 0 of
    True -> x
    _ -> -x


isItTheAnswer :: String -> Bool
isItTheAnswer x = case x of
    "Password" ->  True
    _ -> False

or' :: (Bool, Bool) -> Bool
or' (x,y) = case (x,y) of
    (False, False)->False
    (_,_)->True

and' :: (Bool, Bool) -> Bool
and' (x,y) = case (x,y) of
    (True,True)->True
    (_,_)->False

nand' :: (Bool, Bool) -> Bool
nand' (x,y) = case (x,y) of
    (True,True)->False
    (_,_)->True

xor' :: (Bool, Bool) -> Bool
xor' (x,y) = case (x,y) of
    (True, True) -> False
    (True, False) -> True
    (False, True) -> True
    (False, False) -> False