import Data.List

-- reverse polish notation
solveRPN :: String -> Double
solveRPN = head . foldl foldingfunc [] . words
  where foldingfunc (x:y:ys) "+" = (y+x):ys
        foldingfunc (x:y:ys) "-" = (y-x):ys
        foldingfunc (x:y:ys) "*" = (y*x):ys
        foldingfunc (x:y:ys) "/" = (y/x):ys
        foldingfunc (x:y:ys) "^" = (y**x):ys
        foldingfunc (y:ys) "ln" = (log y):ys
        foldingfunc xs numberString = read numberString:xs
-- 任意の浮動小数点数のRPN式を計算できて、かつ演算子の拡張が容易なうえに、たったの10行！


-- 最短経路問題

-- 以下のような経路について最短距離を考える
-- A -- x1 min -- A1 -- x2 min -- A2 -- x3 min -- A3 -- x4 min -- A4
--                |               |               |
--                z1 min          z2 min          z3 min
--                |               |               |
-- B -- y1 min -- B1 -- y2 min -- B2 -- y3 min -- B3 -- y4 min -- B4

-- 与えられた経路の全体像ははコの字型のセクションのリストだととらえて、以下のようにデータ型を定義する
data Section = Section { getA :: Int, getB :: Int , getC :: Int} deriving (Show)
type RoadSystem = [Section]

hearthToLondon :: RoadSystem
hearthToLondon = [ Section 50 10 30
                 , Section 5 90 20
                 , Section 40 2 25
                 , Section 10 8 0
                 ]

data Label = A | B | C deriving (Show) -- 経路のラベル
type Path = [(Label, Int)]

-- 今回は A, B のそれぞれの地点までの最短経路と次のセクション（コの字型のやつ）を受け取り、A、B の次の地点までの最短経路を導出する関数を次のように定義
roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
  let timeA = sum (map snd pathA) -- Aまでかかった時間
      timeB = sum (map snd pathB) -- Bまでかかった時間
      forwardTimeToA = timeA + a   -- A -> A' までかかる時間
      crossTimeToA = timeB + b + c -- B -> B' -> A' までかかる時間
      forwardTimeToB = timeB + b   -- B -> B' までかかる時間
      crossTimeToB = timeA + a + c -- A -> A' -> B' までかかる時間
      newPathA = if forwardTimeToA < crossTimeToA
                    then (A, a):pathA
                 else (C, c):(B, b):pathB
      newPathB = if forwardTimeToB < crossTimeToB
                    then (B, b):pathB
                 else (C, c):(A, a):pathA
  in (newPathA, newPathB)

optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
  let (bestAPath, bestBPath) = foldl roadStep ([],[]) roadSystem
  in if sum (map snd bestAPath) < sum (map snd bestBPath)
        then reverse bestAPath
     else reverse bestBPath

-- 与えられる全体経路を入力から得るようにする
groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

main = do
  contents <- getContents
  let threes = groupsOf 3 (map read $ lines contents)
      roadSystem = map (\[a,b,c] -> Section a b c) threes
      path = optimalPath roadSystem
      pathString = concat $ map (show . fst) path
      pathTime = sum $ map snd path
  putStrLn $ "The best path to take is: " ++ pathString
  putStrLn $ "Time taken: " ++ show pathTime
