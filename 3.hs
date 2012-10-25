largestPrime :: Integer
largestPrime = maximum $ filter isPrime $ filter (\x -> 600851475143 `mod` x == 0)[1..(ceiling $ sqrt(600851475143))]

isPrime :: Integer -> Bool
isPrime x = length [y | y <- [2..(floor $ sqrt $ fromIntegral x)], mod x y == 0] == 0