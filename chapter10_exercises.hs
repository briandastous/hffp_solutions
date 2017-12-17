-- #1
stops  = "pbtdkg"
vowels = "aeiou"

vowelStops :: [Char] -> [Char] -> [(Char, Char)]
vowelStops vs ss = concat (map getPairsForVowel vs)
  where getPairsForVowel v = map (\s -> (v, s)) ss

-- Part a
stopVowelStops :: [Char] -> [Char] -> [(Char, Char, Char)]
stopVowelStops vs ss = concat $ map getTriplesForStop ss
  where
    getTriplesForStop s = map pairToTriple (vowelStops vs ss)
      where pairToTriple (v, s2) = (s, v, s2)

-- Part b
stopVowelStopsNotBeginWithP :: [Char] -> [Char] -> [(Char, Char, Char)]
stopVowelStopsNotBeginWithP vs ss = concat $ map getTriplesForStop (filter (\s -> s /= 'p') ss)
  where
    getTriplesForStop s = map pairToTriple (vowelStops vs ss)
      where pairToTriple (v, s2) = (s, v, s2)

-- Part c
nouns = ["dogs", "balls", "ropes"]
verbs = ["fetch", "chase", "tug"]

verbNouns :: [[Char]] -> [[Char]] -> [([Char], [Char])]
verbNouns vs ns = concat (map getPairsForVerb vs)
  where getPairsForVerb v = map (\n -> (v, n)) ns

nounVerbNouns :: [[Char]] -> [[Char]] -> [([Char], [Char], [Char])]
nounVerbNouns vs ns = concat $ map getTriplesForNoun ns
  where
    getTriplesForNoun n = map pairToTriple (verbNouns vs ns)
      where pairToTriple (v, n2) = (n, v, n2)

-- #2
-- It calculates the average word length of the words in a sentence, rounded down
seekritFunc x = div (sum (map length (words x))) (length (words x))
-- #3
seekritFunc' x = (fromIntegral (sum (map length (words x)))) / (fromIntegral (length (words x)))

-- Rewriting functions using folds

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny p = foldr (\a acc -> acc || (p a)) False

myElem :: Eq a => a -> [a] -> Bool
myElem a = foldr (\a1 acc -> acc || a1 == a) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' a = any (\a' -> a' == a)

myReverse :: [a] -> [a]
myReverse = foldr (\a acc -> acc ++ [a]) []

myMap :: (a -> b) -> [a] -> [b]
myMap p = foldr (\a acc -> (p a : acc)) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = foldr (\a acc -> if p a then (a : acc) else acc) []

squish :: [[a]] -> [a]
squish = foldr (\as acc -> as ++ acc) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr (\a acc -> (f a) ++ acc) []

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f as = foldr (\a acc -> if f a acc == GT then a else acc) (last as) as

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f as = foldr (\a acc -> if f a acc == LT then a else acc) (last as) as
