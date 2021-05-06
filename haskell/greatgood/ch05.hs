{-# Options -Wall -Werror #-}
multiThree :: Int -> Int -> Int -> Int
multiThree x y z = x * y * z
-- 上の例は以下のように書ける
-- multiThree :: Int -> (Int -> (Int -> Int))

-- 同じ関数を2回適用する関数
-- (a -> a) が2回実行する関数(何かを受け取ってそれと同じ型の値を返す)を表す
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = g
  where g x y = f y x -- flip' f x y = f y x でよい

-- 関数とリストを受け取り、リストの各要素に対して関数を適用させたリストを返す
map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs
-- map (+3) [1,2,3,4] は [x+3 | x <- [1,2,3,4]] と同じだが、 map のほうが見やすい

-- 述語とリストを受け取り、リストの要素のうち述語を満たすもののリストを返す
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f x == True = x : filter' f xs
  | otherwise = filter' f xs

largestDivisible3829 :: Integer
largestDivisible3829 = head (filter p [100000,99999..])
  where p x = x `mod` 3829 == 0

-- コラッツ列
chain :: Integer -> [Integer]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | otherwise = n : chain (n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter l (map chain [1..100]))
  where l x = length x > 15

-- ラムダ式で上の例を書き換え (\x -> xxx)
numLongChains' :: Int
numLongChains' = length (filter (\x -> length x > 15) (map chain [1..100]))

-- 再帰を使った sum
sum' :: [Int] -> Int
sum' [] = 0
sum' (x:xs) = x + sum' xs
-- 畳み込みを使った sum
sum'' :: (Num a) => [a] -> a
sum'' xs = foldl (\acc x -> acc + x) 0 xs
-- 関数はカリー化されているのでもっと簡単にかけるよ
-- sum'' = foldl (+) 0

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs
-- map'' f xs = foldl (\acc x -> acc ++ [f x]) [] xs と書けるが、++ は : よりも遅いので普通は右畳み込み
-- また右畳み込みは無限リストにも使える

elem'' :: (Eq a) => a -> [a] -> Bool
elem'' y ys = foldr (\x acc -> if x == y then True else acc) False ys

-- 畳み込みを使った標準ライブリの再実装
reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []
-- reverse' = foldl (flip (:)) [] とも書ける

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

filter'' :: (a -> Bool) -> [a] -> [a]
filter'' p = foldr (\x acc -> if p x then x : acc else acc) []

last' :: [a] -> a
last' = foldl1 (\_ x -> x)

sqrtSum :: Int
sqrtSum = length (takeWhile (< 1000) (scanl1 (+) (map sqrt [1..]))) + 1 -- 冒頭のオプションつけてるとエラー
-- sqrt が Double を返すからだとおもう


oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
-- これは関数合成を使って以下のように書ける
oddSquareSum' :: Integer
oddSquareSum' = sum . takeWhile (<10000) . filter odd $ map (^2) [1..] -- 冒頭のオプションつけてるとエラー
