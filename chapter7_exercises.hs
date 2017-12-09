-- #1
tensDigit x = d
  where xLast = x `div` 10
        d = xLast `mod` 10

tensDigit' x = d
  where (xLast, _) = x `divMod` 10
        (_, d) = xLast `divMod` 10

hunsD x = d2
  where xLast = x `div` 100
        d2 = xLast `mod` 10

-- #2
foldBoolCase :: a -> a -> Bool -> a
foldBoolCase a1 a2 b =
  case b of
    False -> a1
    True -> a2

foldBoolGuard :: a -> a -> Bool -> a
foldBoolGuard a1 a2 b
  | b == False = a1
  | b == True = a2

-- #3
g :: (a -> b) -> (a, c) -> (b, c)
g aToB (a, c) = (aToB a, c)

-- #4 through #6
roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTrip2 :: (Show a, Read a) => a -> a
roundTrip2 = read . show

roundTrip3 :: (Show a, Read b) => a -> b
roundTrip3 a = read (show a)

main = do
  print ((roundTrip3 4) :: Int)
  print (id 4)
