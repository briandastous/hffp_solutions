module Test where

import Data.List

-- Reviewing currying
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy = flip cattyConny
appedCatty = cattyConny "woops"
frappe = flippy "haha"

-- Recursion

-- #2
rSum :: (Eq a, Num a) => a -> a
rSum 1 = 1
rSum n = n + rSum (n - 1)

-- #3
rMult :: Integral a => a -> a -> a
rMult n m = go n m 0
  where
    go n m acc
      | m == 0 = acc
      | otherwise = go n (m - 1) (acc + n)

-- Fixing Divided By
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise = go (n - d) d (count + 1)

data DividedResult a = Result (a, a) | DividedByZero deriving Show

dividedByFixed :: Integral a => a -> a -> DividedResult a
dividedByFixed num denom = go num denom 0
  where
    go n d count
      | d == 0 = DividedByZero
      | (signum n == -1) && (signum d == -1) = normalized
      | signum n /= signum d = negateResult normalized
      | n < d = Result (count, n)
      | otherwise = go (n - d) d (count + 1)
      where
        normalized = go (abs n) (abs d) count
        negateResult (Result (a1, a2)) = Result ((-a1), (-a2))

-- McCarthy 91 function
mc91 :: (Num a, Ord a) => a -> a
mc91 n
  | n > 100 = n - 10
  | otherwise = (mc91 . mc91) (n + 11)

-- Numbers into words

digitToWord :: Int -> String
digitToWord n = case n of
  0 -> "zero"
  1 -> "one"
  2 -> "two"
  3 -> "three"
  4 -> "four"
  5 -> "five"
  6 -> "six"
  7 -> "seven"
  8 -> "eight"
  9 -> "nine"

digits :: Int -> [Int]
digits n
  | n < 10 = [n]
  | otherwise = digits(d) ++ [r]
    where (d, r) = (divMod n 10)

wordNumber :: Int -> String
wordNumber n = concat $ intersperse "-" $ map digitToWord (digits n)
