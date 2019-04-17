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

--dropEvery :: [a] -> Int -> [a]
main = undefined
