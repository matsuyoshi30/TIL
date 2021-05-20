import Data.Monoid
import qualified Data.Foldable as F

-- 型引数を2つ取るタプルをファンクターのインスタンスにする例
-- fmap をタプルに適用して第一要素を変更したいケース
newtype Pair b a = Pair { getPair :: (a, b) }
instance Functor (Pair c) where
  fmap f (Pair (x, y)) = Pair (f x, y)
  -- パターンマッチを適用して第一要素に fmap を当てている
  -- Pair コンストラクタ(newtypeで定義したやつ)を使って、タプルを Pair b a に変換している
  -- 上の fmap の型は fmap :: (a -> b) -> Pair c a -> Pair c b

-- data でもできる（newtype のほうが高速で、newtype が使えるので上の方が良い）
data Pair' b a = Pair' (a, b)
instance Functor (Pair' c) where
  fmap f (Pair' (x, y)) = Pair' (f x, y)


-- newtype と遅延評価
data CoolBool = CoolBool { getBool :: Bool }

helloMe :: CoolBool -> String
helloMe (CoolBool _) = "hello"

-- *Main> helloMe $ CoolBool True
-- "hello"
-- *Main> helloMe $ CoolBool False
-- "hello"
-- *Main> helloMe undefined
-- "*** Exception: Prelude.undefined
-- CallStack (from HasCallStack):
--   error, called at libraries/base/GHC/Err.hs:79:14 in base:GHC.Err
--   undefined, called at <interactive>:16:9 in interactive:Ghci3

-- data キーワードで定義された型は複数の値コンストラクタを持つかもしれず、
-- helloMe 関数に与えられた引数が CoolBool _ に合致するかどうか確かめるには、どの値コンストラクタが使われたのかわかるまで、引数の評価を進める
-- undefined は評価されると例外を発生させる

newtype CoolBool' = CoolBool' { getBool' :: Bool }
helloMe' :: CoolBool' -> String
helloMe' (CoolBool' _) = "hello"

-- *Main> helloMe' undefined
-- "hello"

-- newtype キーワードは値コンストラクタを一つしか作れない
-- helloMe' 関数に与えられた引数は、 CoolBool' _ に合致することが自明なので、評価する必要がない＝されない

-- 2つの文字列を受け取り長さを比較してOrderingを返す、長さが同じ場合は辞書順を返す関数を考える
-- ナイーブな実装
lengthCompare :: String -> String -> Ordering
lengthCompare x y = let a = length x `compare` length y
                        b = x `compare` y
                    in if a == EQ then b else a
-- Orderingがモノイドであることを使う
lengthCompare' :: String -> String -> Ordering
lengthCompare' x y = (length x `compare` length y) `mappend` (x `compare` y)

-- Foldable の例
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)
-- ある型コンストラクタをFoldableのインスタンスにするには、foldMap関数を実装するのがよい
-- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
-- 「Foldableにしたいコンテナ(a)の中身を取ってモノイドを返す」関数、a型の値を含むFoldable構造を受け取り、結合された単一のモノイドを返す
instance F.Foldable Tree where
  foldMap f EmptyTree = mempty
  foldMap f (Node x l r) = F.foldMap f l `mappend`
                           f x `mappend`
                           F.foldMap f r
-- 値をモノイドに変換する関数はfoldMapに引数fとして与えられる
-- foldMapの実装に必要なのは、変換関数は所与のものとして、どこに適用して結果のモノイドをどう結合するのかを決めてやること

testTree = Node 5
             (Node 3
               (Node 1 EmptyTree EmptyTree)
               (Node 6 EmptyTree EmptyTree)
             )
             (Node 9
               (Node 8 EmptyTree EmptyTree)
               (Node 10 EmptyTree EmptyTree)
             )

-- Foldableな構造を単一のモノイド値に畳みたいときには実際役に立つ
-- *Main> getAny $ F.foldMap (\x -> Any $ x == 3) testTree
-- True
-- F.foldMap (\x -> Any $ x == 3) testTree は、数をとってAnyに包まれたBoolというモノイド値を返す関数を、testTree の各要素に適用して単一のモノイド値に畳み込んでいる
