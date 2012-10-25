diffSumSqSqSum :: Integer -> Integer
diffSumSqSqSum x =  (\x -> x*x) (sum [1..x]) - (sum $ map (\x-> x*x) [1..x]) 