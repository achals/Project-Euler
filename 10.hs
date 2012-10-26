sumprime :: Integer -> Integer
sumprime n = prime' n 3 2

prime' :: Integer -> Integer -> Integer -> Integer
prime' lim num primes
             | num>=lim  = primes
             | isPrime num = prime' lim (num+2) (num +primes)
             | otherwise =  prime' lim (num+2) primes

isPrime :: Integer -> Bool
isPrime x = length [y | y <- [2..(floor $ sqrt $ fromIntegral x)], mod x y == 0] == 0