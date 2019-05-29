import Data.List
import Control.Monad
import System.Random
import Control.Monad.Trans.State

-- problem 21
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = foldr go [] $ zip [1..] xs
  where
    go (i, a) xs
      | n == i = x:a:xs
      | otherwise = a:xs
 
-- problem 22
range :: Int -> Int -> [Int]
range a b = [a..b]

-- problem 23
rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do
  replicateM n $ (xs !!) <$> randomRIO (0, l)
    where l = length xs

-- from: problem 20
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (xs !! (n-1), take (n-1) xs ++ drop n xs)

-- problem 24
diff_select :: Int -> Int -> IO [Int]
diff_select n y
  | y <= n = return [1..y]
  | otherwise = fst <$> runStateT (replicateM n go) [1..y]
  where go = StateT $ \s -> (flip removeAt $ s) <$> randomRIO (1, length s)

main = undefined
