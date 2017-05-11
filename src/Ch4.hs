module Ch4 (
  Option(Some, None),
  optionMap,
  optionFlatMap,
  optionGetOrElse,
  optionOrElse,
  optionFilter
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
