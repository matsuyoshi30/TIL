-- main = do
--   contents <- getContents
--   putStrLn (shortLessOnly contents)
main = interact shortLessOnly

shortLessOnly :: String -> String
shortLessOnly = unlines . filter (\line -> length line < 10) . lines
