isPalindrome :: Integer -> Bool
isPalindrome x = (reverse $ show x) == (show x)

maxPalindrome :: Integer
maxPalindrome = maximum $ filter isPalindrome [x*y | x <- [100..999], y<-[100..999]]