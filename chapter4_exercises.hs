-- Ex 8
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome a =
  a == reverse(a)

-- Ex 9
myAbs :: Integer -> Integer
myAbs a =
  if (a < 0) then (-a) else a

-- Ex 10
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f t1 t2 = ((snd t1, snd t2), (fst t1, fst t2))

-- Correcting syntax

-- Ex 1
x = (+)
f2 xs = w `x` 1
  where w = length xs
