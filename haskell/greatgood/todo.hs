import System.Environment
import System.Directory
import System.IO
import Data.List
import Control.Exception

dispatch :: String -> [String] -> IO ()
dispatch "add" = add
dispatch "view" = view
dispatch "remove" = remove
dispatch command = doesntExist command

doesntExist :: String -> [String] -> IO ()
doesntExist command _ =
  putStrLn $ "The " ++ command ++ " command doesn't exist"

main = do
  (command:argList) <- getArgs -- サブコマンド、コマンドライン引数のパターンマッチ
  dispatch command argList

add :: [String] -> IO ()
add [filename, todoItem] = appendFile filename (todoItem ++ "\n")
add _ = putStrLn "The add command takes exactly two arguments"

view :: [String] -> IO ()
view [filename] = do
  contents <- readFile filename
  let todoTasks = lines contents
      numberTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
  putStr $ unlines numberTasks
view _ = putStrLn "The view command takes exactly one argument"

remove :: [String] -> IO ()
remove [filename, numberString] = do
  contents <- readFile filename
  let todoTasks = lines contents
      numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
  putStrLn "These are your TODO items:"
  mapM_ putStrLn numberedTasks
  let number = read numberString
      newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
  bracketOnError (openTempFile "." "temp") -- tempfile を処理しているときの異常発生時の対応
    (\(tempName, tempHandle) -> do
        hClose tempHandle
        removeFile tempName)
    (\(tempName, tempHandle) -> do
        hPutStr tempHandle newTodoItems
        hClose tempHandle
        removeFile filename
        renameFile tempName filename)
remove _ = putStrLn "The remove command takes exactly two arguments"
