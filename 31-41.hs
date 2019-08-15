import Data.List

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

-- problem 35
primer_factors :: Integer -> [Integer]
primer_factors 0 = []
primer_factors 1 = []
primer_factors x = n: primer_factors (x `div` n)
  where
    n = head $ dropWhile (not . is_factor x) primes
    is_factor a b = a `mod` b == 0
    primes = takeWhile (<= x) $ filter prime [2..]

-- problem 36
prime_factors_mult :: Integer -> [(Integer, Integer)]
prime_factors_mult = go . group . primer_factors
  where go = map ((,) <$> head <*> toInteger . length)

-- problem 37
phi :: Integer -> Integer
phi n = foldr go 1 ps
  where ps = prime_factors_mult n
        go (p,m) n = ((p - 1) * p ^ (m - 1)) * n

-- problem 38
-- 34 is faster than 37

-- problem 39
primesR :: Integer -> Integer -> [Integer]
primesR l u = filter prime [l..u]

-- problem 40
goldbach :: Integer -> (Integer, Integer)
goldbach n = head [(x, y) | x <- primes, y <- primes, x+y == n]
  where primes = takeWhile (<= n) $ filter prime [2..]

-- problem 41
goldbach_list :: Integer -> Integer -> [(Integer, Integer)]
goldbach_list l u = map goldbach evens
  where evens = filter even [l..u]

goldbach_list' :: Integer -> Integer -> Integer -> [(Integer, Integer)]
goldbach_list' l u n = filter f $ map goldbach evens
  where evens = filter even [l..u]
        f (x, y) = x > n && y > n
