-- Exercises: Parametricity

-- Ex. 2
f :: a -> a -> a
f a1 a2 = a1

f2 :: a -> a -> a
f2 a1 a2 = a2

-- Ex. 3
-- Only one implementation
g :: a -> b -> b
g a b = b
