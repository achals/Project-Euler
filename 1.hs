sum35 :: Int -> Int
sum35 x 
    | x == 0 = 0
    | x`mod` 3 == 0 || x `mod` 5 == 0 = x + sum35 (x-1)
    | otherwise = sum35 (x-1)

-- Thanks to chris

sum35' :: Int-> Int
sum35' x = (3 * sum [1..x `div` 3]) + (5 * sum[1..x `div` 5]) - (15 * sum[1..x `div` 15])