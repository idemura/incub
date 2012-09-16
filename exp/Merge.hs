import System (getArgs)
import System.IO
import System.Directory
import System.FilePath.Glob
import Control.Monad (mapM_)

fappend :: String -> String -> IO ()
fappend fout fi = do
  putStrLn (fi ++ "...")
  readFile fi >>= appendFile fout

matchWildcard :: [String] -> [String] -> [String]
matchWildcard fs wc = concat [filter (match $ compile w) fs | w <- wc]

main = do
  args <- getArgs
  if length args < 2 then
    putStrLn "Usage: merge <out> <input-wildcard>+\n"
  else do
    fs <- getDirectoryContents "."
    mapM_ (fappend (head args)) (matchWildcard fs (tail args))
