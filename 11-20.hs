import Data.List

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
