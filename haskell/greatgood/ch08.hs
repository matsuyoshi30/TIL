main = do
  putStrLn "Hello, what is your name?"
  name <- getLine -- name = getLine では name にIOアクションを束縛しただけで結果を取り出せていない
  putStrLn $ "Hey, " ++ name ++ "!"
