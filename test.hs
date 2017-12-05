import Data.List
import Data.List.Split

sayHello :: String -> IO ()
sayHello x =
  putStrLn("Hello, " ++ x ++ "!")

testWhere x = z/x + y
  where y = negate x
        z = y * 10

triple x = x * 3

waxOn = x * 5
  where
    z = 7
    y = z + 8
    x = y ^ 2

waxOff x = triple x

thirdLetter :: String -> Char
thirdLetter s = s !! 2

nthLetter n = "Curry is awesome!" !! (n - 1)
