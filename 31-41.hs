-- problem 31
prime :: Integer -> Bool
prime 0 = True
prime 1 = True
prime x = go x 2
  where go x n
          | n*n > x = True
          | x `mod` n == 0 = False
          | otherwise = True && go x (n+1)

-- problem 32
gcd' :: Integer -> Integer -> Integer
gcd' x 0 = x
gcd' x y = gcd y $ x `mod` y

-- problem 33
coprime :: Integer -> Integer -> Bool
coprime = ((== 1) .) . gcd'

-- problem 34
totient_phi :: Integer -> Int
totient_phi x = length $ filter (coprime x) [1..x-1]

