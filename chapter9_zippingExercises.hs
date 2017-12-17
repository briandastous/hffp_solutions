-- #1
zip2 :: [a] -> [b] -> [(a,b)];
zip2 _ [] = []
zip2 [] _ = []
zip2 (x:xs) (y:ys) = ((x,y) : zip2 xs ys)

-- #2
zipWith2 :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith2 _ [] _ = []
zipWith2 _ _ [] = []
zipWith2 makeC (a:as) (b:bs) = (makeC a b : zipWith2 makeC as bs)

-- #3
zip2' :: [a] -> [b] -> [(a,b)];
zip2' _ [] = []
zip2' [] _ = []
zip2' x y = zipWith2 (\x y -> (x, y)) x y
