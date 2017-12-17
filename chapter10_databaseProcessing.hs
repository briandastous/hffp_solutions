import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
            (fromGregorian 1911 5 1)
    (secondsToDiffTime 34123))
   , DbNumber 9001
   , DbString "Hello, world!"
   , DbNumber 9003
   , DbDate (UTCTime
            (fromGregorian 1921 5 1)
            (secondsToDiffTime 34123))
  ]

-- #1
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate db = (map getTime . filter isDate) db
  where isDate item = case item of
          (DbDate _) -> True
          otherwise -> False
        getTime (DbDate t) = t

-- #2
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber db = (map getInteger . filter isNumber) db
  where isNumber item = case item of
          (DbNumber _) -> True
          otherwise -> False
        getInteger (DbNumber i) = i

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent db = (maximum . filterDbDate) db

sumDb :: [DatabaseItem] -> Integer
sumDb db = (sum . filterDbNumber) db

avgDb :: [DatabaseItem] -> Double
avgDb db = ((fromIntegral . sum) nums) / ((fromIntegral . length) nums)
  where nums = filterDbNumber db
