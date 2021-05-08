import System.IO

-- main = do
--   handle <- openFile "baabaa.txt" ReadMode
--   contents <- hGetContents handle
--   putStr contents
--   hClose handle -- ちゃんと閉じよう

-- handle を受け取ってIOアクションを返すラムダ式とよく使われる
main = do
  withFile "baabaa.txt" ReadMode $ \handle -> do
    contents <- hGetContents handle
    putStr contents
