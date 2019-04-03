import Control.Applicative
import Data.Monoid
import Control.Monad.Trans.State
import Data.List

-- problem 1
lastElem :: [a] -> a
lastElem = foldr1 (flip const)

-- problem 2
lastButOne :: [a] -> a
lastButOne []       = error "list is empty"
lastButOne (y:[])   = error "no enough elems"
lastButOne (x:y:[]) = x
-- this can also be written as:
-- lastButOne [x, _] = x
lastButOne (x:xs)   = lastButOne xs

lastButOne' :: [a] -> a
lastButOne' = head . reverse . init

-- problem 3
elementAt :: [a] -> Int -> a
elementAt [] _     = error "not enough elems"
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n-1)

-- problem 4
myLength :: Integral b => [a] -> b
myLength = foldr (\_ x -> x + 1) 0

-- problem 5
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- problem 6
isPalindrome :: Eq a => [a] -> Bool
isPalindrome = (==) <$> reverse <*> id

isPalindrome' :: Eq a => [a] -> Bool
isPalindrome' []  = True
isPalindrome' [_] = True
isPalindrome' x   =
  liftA2 (&&) (liftA2 (==) head last) (isPalindrome' . tail . init) x

-- problem 7
data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem a)      = [a]
flatten (List [])     = []
flatten (List (x:xs)) = (flatten x) ++ (flatten $ List xs)

-- problem 8
compress :: String -> String
compress = (fmap head) . group

-- problem 9
-- pack = group
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack w@(x:xs) =
  let sp = go x w
      (h, t) = sp
      go x w = (takeWhile (==x) w, dropWhile (==x) w) 
  in h : pack t

-- problem 10
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . pack
