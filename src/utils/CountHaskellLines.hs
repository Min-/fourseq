-- 12/10/2014
-- Min Zhang

-- count Haskell lines in the directory

import System.Environment (getArgs)
import System.Directory (setCurrentDirectory, getDirectoryContents)
import Control.Monad (forM)

main = do
  putStrLn "\nCountHaskellLines [folder]\n"
  workingFolder <- fmap head getArgs
  setCurrentDirectory workingFolder
  fileNames <- getDirectoryContents workingFolder >>= return . filter (\x-> (take 3 . reverse) x == "sh.")
  result <- forM fileNames (\f->countLines f)
  putStrLn $ "The total Haskell files are: " ++ (show $ length result)  
  putStrLn $ "Total lines: " ++ (show $ sum result)

countLines f = do
  file <- readFile f
  return $ length $ lines file
