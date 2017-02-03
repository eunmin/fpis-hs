module Ch2
    (
      fib,
      isSorted,
      myCurry,
      myUncurry,
      compose
    ) where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 0
fib n = fib' 0 1 0
  where
    fib' pp p i | i == (n - 2) = pp + p
                | otherwise = fib' p (pp + p) (i + 1)

isSorted :: [a] -> (a -> a -> Bool) -> Bool
isSorted (x:xs) ordered = isSorted' x xs
  where
    isSorted' a (y:ys) | null ys = True
                       | not (ordered a y) = False
                       | otherwise = isSorted' y ys

myCurry :: (a -> b -> c) -> (a -> (b -> c))
myCurry f = \a -> (\b -> f a b)

myUncurry :: (a -> (b -> c)) -> (a -> b -> c)
myUncurry f = \x y -> ((f x) y)

compose :: (a -> b) -> (b -> c) -> (a -> c)
compose f g = \a -> g (f a)
