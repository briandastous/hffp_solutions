myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny p [] = False
myAny p (x:xs) = p x || myAny p xs

myElem :: Eq a => a -> [a] -> Bool
myElem a [] = False
myElem a (x:xs) = x == a || myElem a xs

myElem2 :: Eq a => a -> [a] -> Bool
myElem2 a x = any (\b -> b == a) x

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = reverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap unpack (a:as) = unpack a ++ squishMap unpack as

squishAgain :: [[a]] -> [a]
squishAgain x = squishMap id x

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy c (x:[]) = x
myMaximumBy c (x:xs) =
  case (c x oldMax) of
    GT -> x
    _ -> oldMax
  where
    oldMax = myMaximumBy c xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy c (x:[]) = x
myMinimumBy c (x:xs) =
  case (c x oldMin) of
    LT -> x
    _ -> oldMin
  where
    oldMin = myMinimumBy c xs

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
