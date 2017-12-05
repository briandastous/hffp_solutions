import Data.List
import Data.List.Split

rvrs :: String -> String
rvrs s = concat (intersperse " " (reverse (splitOn " " s)))

rvrs2 :: String -> String
rvrs2 s = concat . (intersperse " ") . reverse .  (splitOn " ") $ s
