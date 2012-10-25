import Data.List (intersect, union, (\\))

-- Should word, but *too* slow.
divisibleBy20 :: Integer
divisibleBy20 = minimum $ [x | x<- [1..], all (== 0) (map (mod x) [1..20])]

-- Easier alternative : prime factorization of all numbers from 1..20 and a 'union' of sorts
-- of these factors.


divisibleBy20' :: Integer
divisibleBy20' = product $ combinedFactors $ map primeFactors [1..20]

combinedFactors :: [[Integer]] -> [Integer]
combinedFactors x = combinedFactors' x [1]
    where combinedFactors' [] acc = acc
          combinedFactors' (x:xs) acc 
                -- If all elems of x in combinedFactors xs, return combinedFactors of xs
                |  (x `intersect` acc) ==  acc = acc
                -- otherwise include missing factors as well.
                | otherwise = combinedFactors' xs ((x \\ acc) ++ acc)


primeFactors :: Integer -> [Integer]
primeFactors n = primeFactors' n 2
             where primeFactors' 1 _ = []
                   primeFactors' n f
                        | mod n f == 0 = f: primeFactors' (n `div` f) f
                        | otherwise    = primeFactors' n (f+1)