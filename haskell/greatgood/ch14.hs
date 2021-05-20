import Data.Monoid
import Data.Ratio
import Data.List (all)
import Control.Monad.Writer
import Control.Monad.State

-- 値と文字列のペアと、普通の値を受け取って値と文字列のペアを返す関数を受け取って、
-- 値と文字列のペアを返す
applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)

-- 別に値とペアにするのは文字列じゃなくてもいい
-- もとの値と計算結果を結合したいので何らかのリストがいい？
applyLog' :: (a, [c]) -> (a -> (b, [c])) -> (b, [c])
applyLog' (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)

-- 結合で便利なもの、、、Monoidのmappend!
applyLog'' :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog'' (x, log) f = let (y, newLog) = f x in (y, log `mappend` newLog)


type Food = String
type Price = Sum Int -- newtype の Sum

addPrice :: Food -> (Food, Price)
addPrice "beans" = ("milk", Sum 20)
addPrice "meat" = ("red wine", Sum 100)
addPrice _ = ("beer", Sum 30)

example1 = addPrice "beans" -- 普通に関数として使えるけど
example2 = ("beans", Sum 10) `applyLog''` addPrice -- モナドもOK


logNumber :: Int -> Writer [String] Int
logNumber n = writer (n, ["Got number: " ++ show n]) -- Writer はコンストラクタがないので writer 関数を使う

multiWithLog :: Writer [String] Int
multiWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  tell ["Gonna multiply with two"] -- Monoid値にだけ追加
  return (a * b)
-- 結果を出力するときは runWriter


gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
  | b == 0 = do
      tell ["Finish with " ++ show a]
      return a
  | otherwise = do
      tell ["Gonna " ++ show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
      gcd' b (a `mod` b)

-- *Main> fst $ runWriter (gcd' 8 3)
-- 1
-- *Main> mapM_ putStrLn $ snd $ runWriter (gcd' 8 3)
-- Gonna 8 mod 3 = 2
-- Gonna 3 mod 2 = 1
-- Gonna 2 mod 1 = 0
-- Finish with 1


addStuff :: Int -> Int
addStuff = do -- 関数はモナドなので do 記法でまとめられる
  a <- (*2)
  b <- (+10)
  return (a+b)
-- addStuff は引数を一つ取る
-- それがまずは *2 と計算され a に束縛される
-- そして同じ引数が +10 と計算され b に束縛される

-- State Monad
-- スタックのモデル化
type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x, xs)

push :: Int -> Stack -> ((), Stack)
push n xs = ((), n:xs)

stackManip :: Stack -> (Int, Stack)
stackManip stack = let
  (_, newStack1) = push 3 stack
  (_, newStack2) = pop newStack1
  in pop newStack2


pop' :: State Stack Int
pop' = state $ \(x:xs) -> (x, xs) -- State はコンストラクタがないので state 関数を使う

push' :: Int -> State Stack ()
push' a = state $ \xs -> ((), a:xs)

stackManip' :: State Stack Int
stackManip' = do
  push' 3
  pop'
  pop'


-- get, put 版
pop2 :: State Stack Int
-- pop2 = do
--   (x:xs) <- get
--   put xs
--   return x
-- GHC 8.6.0 https://gitlab.haskell.org/ghc/ghc/-/wikis/migration/8.6#monadfaildesugaring-by-default
-- do 記法内でエラーが発生しうるパターンマッチをする場合は、すべてのパターンを列挙しないとダメ
pop2 = do
  xl <- get
  case xl of
    (x:xs) -> do
      put xs
      return x
    _ -> error "Pattern matching error." -- 例外発生時にメッセージが返却される

push2 :: Int -> State Stack ()
push2 x = do
  xs <- get
  put (x:xs)


-- ch13 の綱渡りの例を Either で書き直す
type CntBirds = Int
type Pole = (CntBirds, CntBirds)

x -: f = f x

landLeft :: CntBirds -> Pole -> Either String Pole
landLeft n (left, right)
  | abs ((left + n) - right) < 4 = Right (left + n, right)
  | otherwise = Left "Left side is overweight"

landRight :: CntBirds -> Pole -> Either String Pole
landRight n (left, right)
  | abs ((right + n) - left) < 4 = Right (left, right + n)
  | otherwise = Left "Right side is overweight"

-- *Main> (0, 0) -: landLeft 2 >>= landRight 1 >>= landRight 2
-- Right (2,3)
-- *Main> (0, 0) -: landLeft 2 >>= landRight 1 >>= landLeft 5 >>= landRight 2
-- Left "Left side is overweight"

-- 与えられたリストの冪集合を取得する関数
powerset :: [a] -> [[a]]
powerset xs = filterM (\x -> [True, False]) xs
-- 非決定な値としてのリストは、何になったらいいか分からないので同時にそのすべてになろうとしている
-- *Main Control.Monad> powerset [1,2,3]
-- [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]

bigSmalls :: Int -> Int -> Maybe Int
bigSmalls acc x
  | x > 9 = Nothing
  | otherwise = Just (acc + x)

-- 安全な逆ポーランド記法計算関数
readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x, "")] -> Just x
                                _ -> Nothing

foldingFunc :: [Double] -> String -> Maybe [Double]
foldingFunc (x:y:ys) "+" = return ((y+x):ys)
foldingFunc (x:y:ys) "-" = return ((y-x):ys)
foldingFunc (x:y:ys) "*" = return ((y*x):ys)
foldingFunc (x:y:ys) "/" = return ((y/x):ys)
foldingFunc (x:y:ys) "^" = return ((y**x):ys)
foldingFunc (y:ys) "ln" = return ((log y):ys)
foldingFunc xs numberString = liftM (:xs) (readMaybe numberString)

solveRPN :: String -> Maybe Double
solveRPN st = do
  [result] <- foldM foldingFunc [] (words st)
  return result

-- *Main Control.Monad> solveRPN "1 2 * 4 +"
-- Just 6.0
-- *Main Control.Monad> solveRPN "1 2 * 4"
-- Nothing

-- モナドを作る
-- コインのオモテウラと確率のタプルを考える
newtype Prob a = Prob { getProb :: [(a, Rational)] } deriving Show

instance Functor Prob where
  fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs
  -- パターンマッチを使って newtype の中から値を取り出して関数を適用する

instance Applicative Prob where
  pure = return
  (Prob fs) <*> (Prob xs) = Prob $ mul `map` fs <*> xs
    where mul (f,x) (a,y) = (f a,x*y)

-- >>= の実装を検討するために、確率リストの確率リストをどうやって join で平にするかを考える
thisSituation :: Prob (Prob Char)
thisSituation = Prob
  [(Prob [('a', 1%2), ('b', 1%2)], 1%4),
   (Prob [('c', 1%2), ('d', 1%2)], 3%4)
  ]

-- 上でみた確率リストを平らにするのを実装
flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concat $ map multiAll xs
  where multiAll (Prob innerxs, r) = map (\(x,p) -> (x,p*r)) innerxs

-- return と >>= がわかったので、モナドのインスタンスにする
instance Monad Prob where
  return x = Prob [(x, 1%1)]
  m >>= f = flatten (fmap f m)


-- コイン投げ
-- 2枚は普通、1枚は10%の確率でしか表がでない
data Coin = Heads | Tails deriving (Show, Eq)

coin :: Prob Coin
coin = Prob [(Heads, 1%2), (Tails, 1%2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads, 1%10), (Tails, 9%10)]

flipThree :: Prob Bool
flipThree = do
  x <- coin
  y <- coin
  z <- loadedCoin
  return (all (==Tails) [x,y,z])

-- *Main> getProb flipThree
-- [(False,1 % 40),(False,9 % 40),(False,1 % 40),(False,9 % 40),(False,1 % 40),(False,9 % 40),(False,1 % 40),(True,9 % 40)]

-- 9/40 の確率で真
