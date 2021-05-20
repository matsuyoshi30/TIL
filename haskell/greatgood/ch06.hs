{-# Options -Wall -Werror #-}
import qualified Data.List as ML -- わかりやすいようにしているだけで qualified とかは不要
import Data.Char
import qualified Data.Map as Map -- Data.Map は Prelude と競合する関数とかあるのでこの形でインポートする

numUniques :: (Eq a) => [a] -> Int
numUniques  = length . ML.nub

-- 文章内に含まれる単語の出現回数を数える
wordNums :: String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . ML.group . ML.sort . ML.words
-- 関数合成は右から見る
-- まずは ML.words で文章から空白区切りの単語をリスト化
-- ML.sort でリストをソート
-- ML.group でソート済のリストをグルーピング
-- map を使ってグルーピングした各単語を String, Int のペアに変換

-- あるリストneedleが別のリストhaystackに含まれているかどうかを判定する
isIn :: (Eq a) => [a] -> [a] -> Bool
needle `isIn` haystack = ML.any (needle `ML.isPrefixOf`) (ML.tails haystack)
-- ML.tails はリストに対して一回ずつ tail した結果の要素を返す
-- ML.any は述語とリストを受け取り要素のどれかが述語を満たすかどうかを判定する
-- => 末尾から一個ずつ削除されたリストを要素に持つリストに対して ML.isPrefixOf で needle と先頭から一致するかを調べている

-- シーザー暗号的な
encode :: Int -> String -> String
encode offset msg = map (\x -> chr $ ord x + offset) msg

decode :: Int -> String -> String
decode offset msg = map (\x -> chr $ ord x - offset) msg

-- *Main> decode 3 $ encode 3 "abcde"
-- "abcde"

-- 数値の各桁の和を計算する
digitSum :: Int -> Int
digitSum = sum . map digitToInt . show
-- show で与えられた数値を文字化
-- Data.List の digitToInt で文字化した数値をまた数値にする
-- 上の処理を map を用いて各桁に対して処理
-- sum で map の結果のリストを合計

findTo40 :: Maybe Int
findTo40 = ML.find (\x -> digitSum x == 40) [1..]

findTo :: Int -> Maybe Int
findTo n = ML.find (\x -> digitSum x == n) [1..]

-- キーと、連想リストとしてのペアのリストを与えられたとき、キーに合致する値を返す関数
findKey :: (Eq k) => k -> [(k,v)] -> v
findKey k xs = snd . head . filter (\(key,_) -> key == k) $ xs
-- 上の例だとリストに見つけたいキーのペアがなかったときに空リストが head に渡ってランタイムエラーになる
findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' _ [] = Nothing
findKey' k (x:xs) -- パターンマッチで ((k,v):xs) と書けるよ
  | k == fst x = Just (snd x)
  | otherwise = findKey' k xs
-- 畳み込み使ったバージョン（こっちのほうが再帰よりイイ）
findKey'' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey'' k xs = foldr (\(key,v) acc -> if k == key then Just v else acc) Nothing xs

-- 上で見てきた連想リストは Data.Map 使う
-- Map.fromList <list> で連想リストをMapに変換して処理する（重複したキーがあると後ろのやつが使われる）

phonebook :: Map.Map String String -- 型宣言必要
phonebook = Map.fromList $
  [("betty", "444-1230")
  ,("patsy", "134-3405")
  ,("wendy", "942-2402")
  ]

string2digit :: String -> [Int]
string2digit = map digitToInt . filter isDigit

-- 重複がある連想リストのケース
-- 重複がある連想リストをMapにするときは fromListWith を使う
-- 重複がある連想リストをどう扱うかを決める（以下の例では局所関数addで結合している）
phoneBookToMap :: (Ord k) => [(k,String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith add xs
  where add number1 number2 = number1 ++ ", " ++ number2

phoneBookToMap' :: (Ord k) => [(k,a)] -> Map.Map k [a]
phoneBookToMap' xs = Map.fromListWith (++) $ map (\(k,v) -> (k,[v])) xs
-- fromListWith の前にクロージャでペアの要素をリストの単一要素化して ++ が使えるようにしている

-- phonebook' = -- 先頭の消さないと未使用変数でおこられる
--   [("betty", "444-1230")
--   ,("patsy", "134-3405")
--   ,("patsy", "134-3945")
--   ,("wendy", "942-2412")
--   ,("wendy", "942-2402")
--   ]
