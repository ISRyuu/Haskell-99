import Data.List
import Control.Monad
import Control.Monad.State

-- problem 11
data Encoding a =
  Mutiple Integer a | Single a
  deriving (Show, Eq)

encodeModified :: Eq a => [a] -> [Encoding a]
encodeModified = map go . group
  where
    go x
      | n == 1 = Single h
      | otherwise = Mutiple n h
      where 
        n = toInteger . length $ x
        h = head x

-- problem 12
decodeModified :: [Encoding a] -> [a]
decodeModified = concat . fmap go
  where
    go (Single a) = [a]
    go (Mutiple n a) = replicate (fromInteger n) a

-- problem 13
encodeDirect :: Eq a => [a] -> [Encoding a]
encodeDirect [] = []
encodeDirect (x:xs) = go x 1 xs
  where
    go z n [] = [encoding z n]
    go z n (x:xs)
      | z == x = go z (n+1) xs
      | otherwise = (encoding z n) : (go x 1 xs)
    encoding x n
      | n <= 0 = error "n must be greater than 0!"
      | n == 1 = Single x
      | otherwise = Mutiple n x

-- problem 14
dupli :: [a] -> [a]
dupli x = concat $ fmap go x
  where go x = [x,x]

-- problem 15
repli :: [a] -> Int -> [a]
repli xs n =  concat $ fmap (go n) xs
  where go n x = replicate n x

-- problem 16
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = go (n-1) xs
  where
    go _ []     = []
    go 0 (x:xs) = go (n-1) xs
    go m (x:xs) = x : go (m-1) xs

-- problem 17
split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)

-- problem 18
slice :: [a] -> Int -> Int -> [a]
slice xs l r = drop (l-1) . take r $ xs 

-- problem 19
rotate :: [a] -> Int -> [a]
rotate xs n = take m . drop (n `mod` m) . cycle $ xs
  where
    m = length xs

-- problem 20
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (xs !! n, take (n-1) xs ++ drop n xs)

main = undefined
