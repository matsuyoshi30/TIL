main = do
  line <- getLine
  if null line
    then return () -- 純粋な値からIOアクションを作るだけ。プログラムが終了するわけではない
    else do
    putStrLn $ reverseWords line
    main

reverseWords :: String -> String
reverseWords = unwords . map reverse . words
