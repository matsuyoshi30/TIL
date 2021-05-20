import Control.Monad

-- forM はリストとそのリストに適用する関数を受け取る
-- ラムダ式とdo構文を組み合わせて使うことが多い
main = do
  colors <- forM [1,2,3,4] $ \a -> do
    putStrLn $ "Which color do you associate with the number " ++ show a ++ " ?"
    color <- getLine
    return color
  putStrLn "The colors that you associate with 1, 2, 3 and 4 are:"
  mapM putStrLn colors
