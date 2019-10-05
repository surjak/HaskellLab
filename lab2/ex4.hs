-- length [(x,y,z) | x <- [1..100], y <- [x..100], z <- [y..100], x^2 + y^2 == z^2] - zad 1


isPrime :: Integral t => t -> Bool
isPrime 1 = False
isPrime n = [i | i <- [2..n-1], n `mod` i == 0] == []
    

--length [x | x<-[2..10000], isPrime x]

primes :: [Int]
primes = eratoSieve [2..]
 where
   eratoSieve :: [Int] -> [Int]
   eratoSieve (p : xs) = p : eratoSieve [x | x <- xs, x `mod` p /= 0]


isPrime2 :: Int -> Bool
isPrime2 x = x `elem` (take x primes)

numberOfPrimes :: Int -> Int
numberOfPrimes a = length [x | x<-[2..a], isPrime x]

allEqual :: Eq a => [a] -> Bool
allEqual [] = True
allEqual (x:xs) = all (==x) xs