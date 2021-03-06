module Ch3
    (
    myTail,
    setHead,
    myDrop,
    myDropWhile,
    myInit,
    foldRight,
    sum2,
    product2,
    myLength,
    foldLeft,
    sum3,
    product3,
    myReverse,
    append2,
    succAll,
    toStringAll,
    myFilter,
    flatMap,
    filter2,
    sumList,
    myZipWith,
    allSubList,
    hasSubsequence,
    Tree(Leaf, Branch),
    nodeCount,
    maxValue,
    treeDepth,
    treeMap,
    nodeCount2,
    maxValue2,
    treeDepth2
    ) where

myTail :: [a] -> [a]
myTail (_:xs) = xs

setHead :: [a] -> a -> [a]
setHead (_:xs) x = x : xs

myDrop :: [a] -> Integer -> [a]
myDrop xs n | n == 0 = xs
            | otherwise = myDrop (myTail xs) (pred n)

myDropWhile :: [a] -> (a -> Bool) -> [a]
myDropWhile xs f | f (head xs) = xs
                 | otherwise = myDropWhile (myTail xs) f

myInit :: [a] -> [a]
myInit [_] = []
myInit (x:xs) = x : myInit xs

foldRight :: [a] -> b -> (a -> b -> b) -> b
foldRight [] b _ = b
foldRight (x:xs) b f = f x (foldRight xs b f)

sum2 :: [Integer] -> Integer
sum2 xs = foldRight xs 0 (+)

product2 :: [Integer] -> Integer
product2 xs = foldRight xs 1 (*)

myLength :: [a] -> Integer
myLength xs = foldRight xs 0 (\_ y -> y + 1)

foldLeft :: [a] -> b -> (a -> b -> b) -> b
foldLeft [] b _ = b
foldLeft (x:xs) b f = foldLeft xs (f x b) f

sum3 :: [Integer] -> Integer
sum3 xs = foldLeft xs 0 (+)

product3 :: [Integer] -> Integer
product3 xs = foldLeft xs 1 (*)

myReverse :: [a] -> [a]
myReverse xs = foldLeft xs [] (:)

append2 :: [a] -> [a] -> [a]
append2 xs ys = foldLeft (myReverse xs) ys (:)

myMap :: [a] -> (a -> b) -> [b]
myMap xs f = myReverse (foldLeft xs [] (\x y -> f x : y))

succAll :: [Integer] -> [Integer]
succAll xs = myMap xs succ

toStringAll :: [Double] -> [String]
toStringAll xs = myMap xs show

myFilter :: [a] -> (a -> Bool) -> [a]
myFilter xs f = myReverse (foldLeft xs [] (\x y -> if f x
                                                      then x : y
                                                      else y))
flatMap :: [a] -> (a -> [b]) -> [b]
flatMap xs f = foldLeft xs [] (\x y -> append2 y (f x))

filter2 :: [a] -> (a -> Bool) -> [a]
filter2 xs f = flatMap xs (\x -> [x | f x])

sumList :: [Integer] -> [Integer] -> [Integer]
sumList [] [] = []
sumList (x:xs) (y:ys) = append2 [(x + y)] (sumList xs ys)

myZipWith :: (a -> a -> a) -> [a] -> [a] -> [a]
myZipWith _ [] [] = []
myZipWith f (x:xs) (y:ys) = append2 [f x y] (myZipWith f xs ys)

myInits :: [a] -> [[a]]
myInits [] = []
myInits xs = append2 [xs] (myInits (myInit xs))

allSubList :: [a] -> [[a]]
allSubList [] = []
allSubList xs = append2 (myInits xs) (allSubList (myTail xs))

hasSubsequence :: Eq a => [a] -> [a] -> Bool
hasSubsequence xs ys = foldLeft (allSubList xs) False (\x result -> if result
                                                                      then result
                                                                      else x == ys)

data Tree a = Leaf a | Branch (Tree a) (Tree a)

nodeCount :: Tree a -> Int
nodeCount (Leaf x) = 1
nodeCount (Branch left right) = (nodeCount left) + (nodeCount right)

maxValue :: (Ord a) => Tree a -> a
maxValue (Leaf x) = x
maxValue (Branch left right) = max (maxValue left) (maxValue right)

treeDepth :: Tree a -> Int
treeDepth (Leaf x) = 0
treeDepth (Branch left right) = (max (treeDepth left) (treeDepth right)) + 1

treeMap :: (b -> b -> b) -> (a -> b) -> Tree a -> b
treeMap bf lf (Leaf x) = lf x
treeMap bf lf (Branch left right) = bf (treeMap bf lf left) (treeMap bf lf right)

nodeCount2 :: Tree a -> Int
nodeCount2 tree = treeMap (\x y -> x + y) (\x -> 1) tree

maxValue2 :: (Ord a) => Tree a -> a
maxValue2 tree = treeMap (\x y -> max x y) (\x -> x) tree

treeDepth2 :: Tree a -> Int
treeDepth2 tree = treeMap (\x y -> (max x y) + 1) (\x -> 0) tree
