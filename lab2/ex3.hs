isPalindrome :: [Char] -> Bool
isPalindrome a  | length a <=1 = True
isPalindrome a = if head a == last a
    then isPalindrome (init(drop 1 a)) 
    else False

isPalindrome2 :: [Char] -> Bool
isPalindrome2 a = a==reverse a

getElemAtIndex :: Int -> [a] -> a
getElemAtIndex a x = x!!a

getElemAtIndexV2 :: Int -> [a] -> a
getElemAtIndexV2 x = head . drop x

capitalize :: [Char] -> [Char]
capitalize w =  toEnum(fromEnum(head w) - 32) : tail w