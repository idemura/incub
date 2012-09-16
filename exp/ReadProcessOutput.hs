import System.Process
import System.Directory
import System.FilePath
import Data.Char
import Data.List
import Control.Monad (mapM, foldM, filterM)

isFile :: FilePath -> IO Bool
isFile f = do
    b <- doesDirectoryExist f
    return $ not b && (takeExtension f) `elem` [".exe", ".com", ".bat", ".cmd"]
  
checkNameConflict :: String -> IO String
checkNameConflict f = do
    let fname = takeBaseName f
    (_, s, _) <- readProcessWithExitCode "where" [fname] []
    return $ if null s then [] else s

getConflicts :: FilePath -> IO [FilePath]
getConflicts p = do
    getDirectoryContents p >>= filterM isFile >>= mapM checkNameConflict >>= filterM (return . not . null)
  
main = do
    conflicts <- getConflicts "C:\\MinGW\\msys\\1.0\\bin"
    mapM_ putStr conflicts
    putStrLn (show (length conflicts) ++ " conflict(s).")
