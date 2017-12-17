import Data.Char

-- #2
filterWord w = filter isUpper w

-- #3
capW [] = []
capW (c:cs) = (toUpper c:cs)

-- #4
capW2 [] = []
capW2 (c:cs) = (toUpper c: capW2 cs)

-- #5, #6
getCapFirst s = (toUpper . head) s
getCapFirst2 = (toUpper . head)
