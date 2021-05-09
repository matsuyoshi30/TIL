myAction :: IO -> String
myAction = do
  a <- getLine
  b <- getLine
  return $ a ++ b

-- IO はアプリカティブなので、上の例をアプリカティブスタイルでかける
myAction' :: IO -> String
myAction' = (++) <$> getLine <*> getLine

-- 関数もアプリカティブ
-- instance Applicative ((->) r where
--   pure x = (\_ -> x) -- 引数を無視してxを返す
--   f <*> g = \x -> f x (g x)

-- アプリカティブ値のリストを受け取り、リストを戻り値として持つ一つのアプリカティブ値をに変換する関数
sequenseA :: (Applicative f) => [f a] -> f [a]
sequenseA [] = pure []
sequenseA (x:xs) = (:) <$> x <*> sequenseA xs

-- 畳み込みでも実装できる
--   （リストの要素を操作しながら何らかの結果を集計するときは畳み込みが使える）
-- sequenseA = foldr (liftA2 (:)) (pure [])
