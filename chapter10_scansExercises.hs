

-- #1
fibs' = take 20 fibs
  where fibs = 1 : scanl (+) 1 fibs

-- #2
fibs'' = takeWhile (\x -> x < 100) fibs
  where fibs = 1 : scanl (+) 1 fibs

-- #3

factorial = scanl (*) 1 [1..]
