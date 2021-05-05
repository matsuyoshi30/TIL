{-# Options -Wall -Werror #-}
lucky :: Int -> String
lucky 7 = "You are lucky"
-- lucky x = "You are not lucky" だと x が未使用というエラーが出たので、以下のように書く
lucky _ = "You are not lucky"

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors a b = ( fst a + fst b, snd a + snd b)

-- 上の例をパターンマッチを使って書く
addVectors' :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- head をパターンマッチを使って定義する例
head :: [a] -> a
head [] = error "No Content"
head (x:_) = x

-- 上のheadと同じ意味のcaseバージョン
head' :: [a] -> a
head' xs = case xs of [] -> error "No Content"
                      (x:_) -> x
-- head は head' の糖衣構文（シンタックスシュガー）

-- as パターンの例
firstLetter :: String -> String
firstLetter "" = "Empty String"
firstLetter xs@(x:_) = "The first letter of " ++ xs ++ " is " ++ [x]

-- ガードの例
bmiTell :: Double -> String
bmiTell bmi
  | bmi <= 18.5 = "underweight"
  | bmi <= 25.0 = "normal"
  | bmi <= 30.0 = "overweight"
  | otherwise = "whale"

max' :: (Ord a) => a -> a -> a
max' x y
  | x < y = y
  | otherwise = x

-- where の例
calcBMI :: Double -> Double -> String
calcBMI weight height
  | bmi <= skinny = "underweight"
  | bmi <= normal = "normal"
  | bmi <= fatty = "overweight"
  | otherwise = "whale"
  where bmi = weight / height ** 2.0
        skinny = 18.5 -- where 以下のインデントは揃えないとコンパイルエラー
        normal = 25.0
        fatty = 30.0

-- where のなかでパターンマッチつかう例
initials :: String -> String -> String
initials firstname lastname = [f] ++ " " ++ [l]
  where (f:_) = firstname
        (l:_) = lastname

-- let の例
cylinder :: Double -> Double -> Double
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ** 2
  in sideArea + 2 * topArea

-- 式の途中でcase式を使う例
describeList :: [a] -> String
describeList ls = "This list is "
                  ++ case ls of [] -> "empty."
                                [_] -> "a singleton list."
                                _ -> "a longer list."
