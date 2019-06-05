import Data.List
import Control.Monad.Primitive
import Control.Monad
import Control.Applicative
import System.Random
import Control.Monad.Primitive
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Vector.Unboxed as VU
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
  | otherwise = evalStateT (replicateM n go) [1..y]
  where go = StateT $ \s -> (flip removeAt $ s) <$> randomRIO (1, length s)

-- problem 25: helper functions
shuffleSwapPairs :: Int -> IO [(Int, Int)]
shuffleSwapPairs n = liftA2 fmap zip go $ [0..n-1]
  where go = traverse (curry randomRIO $ (n-1))

-- problem 25
rnd_permu :: (VU.Unbox a) => VU.Vector a -> IO (VU.Vector a)
rnd_permu v = do
  let l = VU.length v  
  mv <- VU.thaw v
  sps <- shuffleSwapPairs l
  mapM_ (uncurry $ VUM.swap mv) sps
  VU.freeze mv

-- problem 26
combinations :: [a] -> Int -> [[a]]
combinations arra 0 = [[]]
combinations arra n = [y:ys | y:xs <- tails arra, ys <- combinations xs (n-1)]

main = undefined

