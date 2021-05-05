removeNonUppercase :: [Char] -> [Char]
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z'] ]

factorial :: Int -> Int
factorial n = product [1..n]
-- factorial 50 is -3258495067890909184

factorial' :: Integer -> Integer
factorial' n = product [1..n]
-- factorial2 50 is 30414093201713378043612608166064768844377641568960512000000000000

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

plusLen ns = fromIntegral (length ns) + 3.2
-- length ns + 3.2 はエラーなので fromIntegral で 3.2 と加算ができるようなインスタンスを返す
