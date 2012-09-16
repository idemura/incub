import Data.List
import System.Directory
import System.FilePath
import System.Process
import System.Exit
import Control.Monad (filterM)

dirList :: String -> [String]
dirList dir = reverse . scanl1 (\fullp d -> fullp </> d) . splitDirectories $ dir

runIn :: String -> [String] -> String -> CreateProcess
runIn exe args path = pi {cwd = Just path}
  where
    pi = proc exe args
    
build :: String -> IO ExitCode
build path = do
  (_, _, _, h) <- createProcess $ runIn "cabal" ["build"] path
  waitForProcess h
  
getCabal :: String -> IO String
getCabal path = do
  fs <- getDirectoryContents path
  let content = let fn p = p /= "." && p /= ".." in filter fn fs
  files <- filterM (doesFileExist . (path </>)) content
  case filter ((== ".cabal") . takeExtension) files of
    [] -> return []
    (f : _) -> return f

findBuild :: [String] -> IO ExitCode
findBuild [] = do
  putStrLn "Cabal script not found."
  return $ ExitFailure (-1)  
findBuild (path : ps) = do
  cabal <- getCabal path
  if not $ null $ cabal then
    build path
  else
    findBuild ps

main :: IO ()
main = do
  cd <- getCurrentDirectory
  exitWith =<< (findBuild $ dirList cd)
  