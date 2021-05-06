{-# Options -Wall -Werror #-}
fac :: Int -> Int
fac n
  | n == 0 = 1
  | n == 1 = 1
  | otherwise = fac (n - 1) + fac (n - 2)

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Empty List"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: Int -> Int -> [Int]
replicate' m n
  | m == 1 = [n] -- 最初基底部忘れてて無限ループ入った
  -- 模範解答は負の値にも対応するために m <= 0 = [n] になっている
  | otherwise = [n] ++ (replicate' (m - 1) n)

take' :: Int -> [Int] -> [Int]
take' n xs
  | n <= 0 = []
  | otherwise = (head xs) : (take' (n - 1) (tail xs))
-- これだけだと引数に空のリストが来たときの考慮がない

-- take' の模範解答
take'' :: Int -> [a] -> [a]
take'' n _
  | n <= 0 = []
take'' _ [] = []
take'' n (x:xs) = x : take'' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool -- 最初型クラスEqを忘れてたので n == x でコンパイルエラー
elem' _ [] = False
elem' n (x:xs)
  | n == x = True
  | otherwise = elem' n xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let lessOrEqual = [a | a <- xs, a <= x]
      larger = [a | a <- xs, a > x]
  in (quicksort lessOrEqual) ++ [x] ++ (quicksort larger)
