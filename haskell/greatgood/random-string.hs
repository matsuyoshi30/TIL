import System.Random

main = do
  gen <- genStdGen
  putStrLn $ take 20 (randomRs ('a', 'z') gen)
