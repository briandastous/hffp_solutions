{-# LANGUAGE NoMonomorphismRestriction #-}

module DetermineTheType where

example = 1

a = (* 9) 6
b = head [(0,"doge"),(1,"kitteh")]
c = head [(0 :: Integer ,"doge"),(1,"kitteh")]
d = if False then True else False
e = length [1, 2, 3, 4, 5]
f = (length [1, 2, 3, 4]) > (length "TACOCAT")

x=5
y=x+5
w = y * 10

x2=5
y2=x+5
z2 y = y * 10

x3=5
y3=x+5
f3=4/y

x4 = "Julie"
y4 = " <3 "
z4 = "Haskell"
f4 = x4 ++ y4 ++ z4
