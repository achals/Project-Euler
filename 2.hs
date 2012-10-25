fib:: Integer -> Integer
fib 0 = 1
fib 1 = 2
fib n 
    | n > 0 = fib (n-1) + fib (n-2)
    | otherwise = 0

countFib :: Integer
countFib = sum (filter (\x -> x`mod` 2 == 0) (takeWhile (\x -> x <4000000) (map fib [1..])))
         