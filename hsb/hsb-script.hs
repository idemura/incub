import Data.List
import System.Directory
import System.FilePath
import System.Process
import System.Exit

script = "build.bat"

dirList :: String -> [String]
dirList dir = reverse . scanl1 (\fullp d -> fullp </> d) . splitDirectories $ dir

runIn :: String -> [String] -> String -> CreateProcess
runIn exe args path = pi {cwd = Just path}
  where
    pi = proc (path </> exe) args
    
build :: String -> IO ExitCode
build path = do 
  (_, _, _, h) <- createProcess $ runIn script [] path
  waitForProcess h

findBuild :: [String] -> IO ExitCode
findBuild [] = do
  putStrLn ("'" ++ script ++ "' not found")
  return $ ExitFailure (-1)
  
findBuild (p : ps) = do
  let scriptPath = p </> script
  hasScript <- doesFileExist scriptPath
  if hasScript then
    build p
  else
    findBuild ps

main :: IO ()
main = do
  cd <- getCurrentDirectory
  exitWith =<< (findBuild $ dirList cd)
  