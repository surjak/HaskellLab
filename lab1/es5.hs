not' :: Bool -> Bool
not' True = False
not' False = True

isItTheAnswer :: String -> Bool
isItTheAnswer "Password" = True
isItTheAnswer _ = False

or' :: (Bool, Bool) -> Bool
or' (False, False) = False
or' (_,_) = True

and' :: (Bool, Bool) -> Bool
and' (True, True) = True
and' (_,_) = False

nand' :: (Bool, Bool) -> Bool
nand' (x,y) = not'(and'(x,y))

xor' :: (Bool, Bool) -> Bool
xor' (False, False) = False
xor' (False, True) = True
xor' (True, False) = True
xor' (True, True) = False