triplet :: Integer
triplet = head [x*y*z | x<-[1..1000],  y<-[1..1000], z<-[1..1000], x+y+z == 1000, pyth x y z]

pyth:: Integer -> Integer -> Integer -> Bool
pyth x y z = x*x + y*y == z*z