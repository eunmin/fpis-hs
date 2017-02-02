module Ch2
    (
      fib
    ) where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 0
fib n = fib' 0 1 0
  where
    fib' pp p i | i == (n - 2) = pp + p
                | otherwise = fib' p (pp + p) (i + 1)
