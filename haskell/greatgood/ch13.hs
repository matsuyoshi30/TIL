import Control.Monad

-- 以下は >>=
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just a) f = f a

-- つなわたり
type CntBirds = Int
type Pole = (CntBirds, CntBirds)

landLeft :: CntBirds -> Pole -> Pole
landLeft n (left, right) = (left + n, right)

landRight :: CntBirds -> Pole -> Pole
landRight n (left, right) = (left, right + n)

x -: f = f x

-- 以下のように、ステップをチェーン的にかける
-- *Main> (0, 0) -: landLeft 2 -: landRight 1 -: landRight 2
-- (2,3)

-- landLeft, landRight に失敗の表現をもたせる
landLeft' :: CntBirds -> Pole -> Maybe Pole
landLeft' n (left, right)
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise = Nothing

landRight' :: CntBirds -> Pole -> Maybe Pole
landRight' n (left, right)
  | abs ((right + n) - left) < 4 = Just (left, right + n)
  | otherwise = Nothing

-- 戻り値の型がMaybe Poleになったのでチェーン的にかけなくなった
-- => ここでバインド >>= を使う

-- *Main> (0, 0) -: landLeft' 2 >>= landRight' 1 >>= landRight' 2
-- Just (2,3)

-- 文脈を持った値を、普通の値をとって文脈を持った値を返す関数にわたすことができる

-- ステップの途中で失敗の文脈が発生した場合もちゃんと捕捉できる
-- *Main> (0, 0) -: landLeft 2 -: landRight 6 -: landLeft (-1) -: landRight (-2)
-- (1,4)
-- *Main> (0, 0) -: landLeft' 2 >>= landRight' 6 >>= landLeft' (-1) >>= landRight' (-2)
-- Nothing

-- 複数のモナド値を糊付けできる do 記法
-- do 使わないケース
foo :: Maybe String
foo = Just 3  >>= (\x ->
                      Just "!" >>= (\y ->
                                      Just (show x ++ y)))
-- do 使う
foo' :: Maybe String
foo' = do
  x <- Just 3 -- モナドの結果を <- で調べる
  y <- Just "!"
  Just (show x ++ y)

-- 綱渡りのケースを do 記法で
routine :: Maybe Pole
routine = do
  start <- return (0,0)
  first <- landLeft' 2 start
  -- Nothing -- 途中にはさめる
  second <- landRight' 2 first
  landLeft' 1 second

-- [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n, ch) の do 記法かきかえ
listOfTuple :: [(Int, Char)]
listOfTuple = do
  n <- [1,2]
  ch <- ['a','b']
  return (n,ch)
-- [(n,ch) | n <- [1,2], ch <- ['a','b']] とにてる
-- => リスト内包表記はモナドの糖衣構文（シンタックスシュガー）

-- リスト内包表記でフィルタを使うのは guard 関数適用と同じ
-- [ x | x <- [1..50], '7' `elem` show x ]
sevensOnly :: [Int]
sevensOnly = do
  x <- [1..50]
  guard ('7' `elem` show x) -- False なら失敗、Trueならダミーの値を返す = 計算を続けてよいかの判断
  return x


-- 与えられた位置にあるナイトが特定の位置まで何手で移動できるか問題
type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
  (c', r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1),
               (c+1,r+2),(c+1,r-2),(c-1,r+2),(c-1,r-2)]
  guard (c' `elem` [1..8] && r' `elem` [1..8]) -- 盤面内のものをフィルタ
  return (c', r')

-- モナドなしVer
moveKnight' :: KnightPos -> [KnightPos]
moveKnight' (c, r) = filter onBoard
  [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1),
   (c+1,r+2),(c+1,r-2),(c-1,r+2),(c-1,r-2)]
  where onBoard (c, r) = c `elem` [1..8] && r `elem` [1..8] -- 盤面内のものをフィルタ

-- ある位置から3手で移動できる場所を列挙する関数
in3 :: KnightPos -> [KnightPos]
in3 start = do
  first <- moveKnight start
  second <- moveKnight first
  moveKnight second
-- こうも書ける
-- let in3 = return start >>= moveKnight >>= moveKnight >>= moveKnight

-- 2つの位置を受け取り、3手でナイトがそこまで移動できるかどうかを判定する関数
canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start

-- ex) 始点と終点を与えるとどういうルートをとるか返す関数を考える
-- 与えられた位置にあるナイトが1手で移動できる場所を列挙し、2次元リストで返す
moveKnightRoutes :: [KnightPos] -> [[KnightPos]]
moveKnightRoutes [(c,r)] = do -- 引数のリストの要素が単一の場合、その地点から移動できる場所を探す
  (c',r') <- [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1),
               (c+1,r+2),(c+1,r-2),(c-1,r+2),(c-1,r-2)]
  guard (c' `elem` [1..8] && r' `elem` [1..8]) -- 盤面内のものをフィルタ
  return [(c',r'), (c,r)]
moveKnightRoutes (x:xs) = map (++xs) (moveKnightRoutes [x])
  -- 引数のリストの要素が複数の場合、それぞれの要素について移動できる場所を探して（再帰）、それをスタート地点と結合してリストにする

-- ある地点から3手で移動できるルートのリスト（2次元リスト）を列挙する
allRoute3 :: KnightPos -> [[KnightPos]]
allRoute3 x = do
  first <- moveKnightRoutes [x]
  second <- moveKnightRoutes first
  moveKnightRoutes second

route3 :: KnightPos -> KnightPos -> [[KnightPos]]
route3 start end = do
  allRoutes <- allRoute3 start
  guard (head allRoutes == end)
  return (reverse allRoutes)
