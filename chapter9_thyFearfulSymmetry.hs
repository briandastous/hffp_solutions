takeUntil :: Char -> [Char] -> ([Char], [Char])
takeUntil c s = (takeWhile (/=c) s, dropWhile (/=c) s)

myWords :: [Char] -> [[Char]]
myWords s
  | s' == "" = []
  | otherwise = (word : myWords rest)
  where
    s' = dropWhile (==' ') s -- remove leading spaces
    (word, rest) = takeUntil ' ' s'

myLines :: String -> [String]
myLines "" = [""]
myLines s = (line : myLines rest)
  where
    (line, rest') = takeUntil '\n' s
    -- either we reached the end of the string or we reached a newline
    -- which we want to remove
    rest = case rest' of
      ""      -> ""
      (r:rs)  -> rs
