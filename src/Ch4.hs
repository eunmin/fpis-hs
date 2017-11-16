module Ch4 (
  Option(Some, None),
  optionMap,
  optionFlatMap,
  optionGetOrElse,
  optionOrElse,
  optionFilter,
  variance,
  optionSequence,
  optionTraverse
) where

data Option a = Some a
              | None
              deriving (Show)

optionMap :: (a -> b) -> Option a -> Option b
optionMap f None = None
optionMap f (Some x) = Some (f x)

optionFlatMap :: (a -> Option b) -> Option a -> Option b
optionFlatMap _ None = None
optionFlatMap f (Some x) = f x

optionGetOrElse :: b -> Option a -> Either a b
optionGetOrElse x None = Right x
optionGetOrElse _ (Some x) = Left x

optionOrElse :: Option b -> Option a -> Either (Option a) (Option b)
optionOrElse x None = Right x
optionOrElse _ (Some x) = Left (Some x)

optionFilter :: (a -> Bool) -> Option a -> Option a
optionFilter _ None = None
optionFilter f (Some x) = if (f x)
                            then Some x
                            else None

mean :: [Double] -> Option Double
mean [] = None
mean xs = Some $ sum xs / fromIntegral (length xs)

variance :: [Double] -> Option Double
variance xs =
  optionFlatMap mean $
    optionFlatMap (\m' -> Some (map (\x -> (x - m') ^ 2) xs)) $
      mean xs

optionMap2 :: Option a -> Option b -> (a -> b -> c) -> Option c
optionMap2 None _ _ = None
optionMap2 _ None _ = None
optionMap2 (Some a) (Some b) f = Some $ f a b

optionSequence :: [Option a] -> Option [a]
optionSequence [] = Some []
optionSequence (None:_) = None
optionSequence (Some x:xs) = optionMap (\xs' -> x : xs') (optionSequence xs)

optionTraverse :: [a] -> (a -> Option b) -> Option [b]
optionTraverse [] _ = Some []
optionTraverse (x:xs) f = optionFlatMap (\x' -> (optionMap (\xs' -> x' : xs') (optionTraverse xs f))) (f x)
