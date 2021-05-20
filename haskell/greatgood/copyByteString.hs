import System.Environment
import System.Directory
import System.IO
import Control.Monad
import Control.Exception
import qualified Data.ByteString.Lazy as B

main = do
  (filename1:filename2:_) <- getArgs
  copy filename1 filename2

copy src dst = do
  contents <- B.readFile src
  bracketOnError (openTempFile "." "temp")
    (\(tempName, tempHandle) -> do
        hClose tempHandle
        removeFile tempName)
    (\(tempName, tempHandle) -> do
        B.hPutStr tempHandle contents
        hClose tempHandle
        renameFile tempName dst)
