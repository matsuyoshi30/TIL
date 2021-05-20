import qualified Data.Map as Map
data Shape = Circle Float Float Float |
             Rectangle Float Float Float Float
             deriving (Show) -- これがないと Circle 10 20 10 ではプロンプトに表示がされない

area :: Shape -> Float
area (Circle _ _ r) = pi * r * r
area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

data Point = Point Float Float deriving (Show)
data Shape' = Circle' Point Float | Rectangle' Point Point deriving (Show)

area' :: Shape' -> Float
area' (Circle' _ r) = pi * r * r
area' (Rectangle' (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- 図形を動かす関数
nudge :: Shape' -> Float -> Float -> Shape'
nudge (Circle' (Point x y) r) a b = Circle' (Point (x+a) (y+b)) r
nudge (Rectangle' (Point x1 y1) (Point x2 y2)) a b = Rectangle' (Point (x1+a) (y1+b)) (Point (x2+a) (y2+b))

-- 値コンストラクタが多くの引数を保つ場合はレコード構文を使うことで可視性、利便性を上げる
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String } deriving (Show)
-- フィールドが自明でない場合は上のようにフィールドに名前をつけたほうがいい（全部埋めないとダメ）

-- 三次元ベクトル
-- Int, Integer, Double でも使えるように型コンストラクタにしている
data Vector a = Vector a a a deriving (Show)

vplus :: (Num a) => Vector a -> Vector a -> Vector a -- 加算
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

dotFrod :: (Num a) => Vector a -> Vector a -> a -- 内積
(Vector i j k) `dotFrod` (Vector l m n) = i*l + j*m + k*n

vmult :: (Num a) => Vector a -> a -> Vector a -- スカラー倍
(Vector i j k) `vmult` m = Vector (i*m) (j*m) (k*m)

data Person' = Person' { firstName' :: String
                       , lastName' :: String
                       , age' :: Int } deriving (Eq, Show, Read)

-- 列挙型
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving (Show, Eq, Ord, Read, Bounded, Enum)
-- Bounded のインスタンスなので minBound, maxBound が使える
-- Enum のインスタンスなので succ, pred や範囲指定が使える

-- 型シノニムで前章ででたやつをわかりやすくする例
type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]
phoneBook :: PhoneBook
phoneBook =
  [("betty", "243-2342")
  ,("case", "440-3293")
  ,("david", "523-4350")
  ,("wendy", "340-4025")
  ]

isPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
isPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

-- Either の例
data LockerState = Taken | Free deriving (Show, Eq) -- ロッカーが埋まっているか空いているか

type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
  Nothing -> Left $ "Locker " ++ show lockerNumber ++ " doesn't exist!" -- 該当の番号のロッカーがない
  Just (state, code) -> if state /= Taken
                           then Right code -- 該当の番号のロッカーが空いていた
                        else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"
                             -- 該当の番号のロッカーが埋まっていた
-- Eitherを使うことでロッカーの暗証番号取得失敗の原因が2つ記述できる

lockers :: LockerMap
lockers = Map.fromList
  [(100,(Taken, "ZD39I"))
  ,(101,(Free, "JAH3I"))
  ,(103,(Free, "IQSA9"))
  ,(105,(Free, "QOTSA"))
  ,(109,(Taken, "893JJ"))
  ,(110,(Taken, "99292"))
  ]

-- 再帰的なデータ型
-- 自分自身の型のフィールドを持つデータ型
-- data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

-- 記号文字を使ってデータ型を定義（中置関数にするときにはコロンから始めなければならない）
infixr 5 :-: -- infixr で記号文字の結合順位を定義
data List' a = Empty' | a :-: (List' a) deriving (Show, Read, Eq, Ord)

infixr 5 ^++
(^++) :: List' a -> List' a -> List' a
Empty' ^++ ys = ys
(x :-: xs) ^++ ys = x :-: (xs ^++ ys)

-- 二分探索木
-- 空の木、もしくは値と2つの木をもつ
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

insertNode :: (Ord a) => a -> Tree a -> Tree a
insertNode x EmptyTree = singleton x
insertNode x (Node y left right)
  | x == y = Node y left right
  | x < y = Node y (insertNode x left) right
  | x > y = Node y left (insertNode y right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node y left right)
  | x == y = True
  | x < y = treeElem x left
  | x > y = treeElem x right

-- 型クラス、インスタンスの例
data TrafficLight = Blue | Red | Green -- Eq を自動導出しないで以下で手動でインスタンス作成

instance Eq TrafficLight where
  Blue == Blue = True
  Red == Red = True
  Green == Green = True
  _ == _ = False
-- 型クラス Eq が以下の通りに定義されている（相互再帰）
-- class Eq a where
--   (==) :: a -> a -> Bool
--   (/=) :: a -> a -> Bool
--   x == y = not (x /= y)
--   x /= y = not (x == y)
-- TrafficLight では == のみ実装することでインスタンスを作成＝最小完全定義を満たす
-- 型クラス Eq がメソッドの型のみ定義している場合は /= もインスタンス内に明記する必要がある

instance Show TrafficLight where
  show Blue = "Blue light"
  show Red = "Red light"
  show Green = "Green light"

-- 多相型をインスタンス化する例
-- instance (Eq m) => Eq (Maybe m) where
--   Just x == Just y = x == y
--   Nothing == Nothing = True
--   _ == _ = False

-- Yes, No の型クラス
class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id -- id は引数を一つ受け取ってそれをかえす標準ライブラリの関数

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False

instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _ = True

instance YesNo TrafficLight where
  yesno Red = False
  yesno _ = True

yesNoIf :: (YesNo a) => a -> b -> b -> b
yesNoIf yesnoVal yesResult noResult =
  if yesno yesnoVal
  then yesResult
  else noResult

-- Tree も Functor
instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)
