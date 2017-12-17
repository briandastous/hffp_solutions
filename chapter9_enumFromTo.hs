
eftBool :: Bool -> Bool -> [Bool]
eftBool False False = [False]
eftBool False True = [False, True]
eftBool True False = []
eftBool True True = [True]

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd LT LT = [LT]
eftOrd LT EQ = [LT, EQ]
eftOrd LT GT = [LT, EQ, GT]
eftOrd EQ EQ = [EQ]
eftOrd EQ GT = [EQ, GT]
eftOrd GT GT = [GT]
eftOrd _ _   = []

eftInt :: Int -> Int -> [Int]
eftInt m n
  | m <= n = (m : eftInt (m + 1) n)
  | otherwise = []

eftChar :: Char -> Char -> [Char]
eftChar m n
  | m <= n = (m : eftChar (succ m) n)
  | otherwise = []

eftOrd2 :: (Enum a, Ord a) => a -> a -> [a]
eftOrd2 m n
  | m <= n = (m : eftOrd2 (succ m) n)
  | otherwise = []
