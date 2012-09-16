{-# LANGUAGE CPP #-}

import Data.List
import System.Directory
import System.FilePath
import System.Process
import System.Exit

#ifdef mingw32_HOST_OS
win32 = True
#else
win32 = False
#endif

run :: String -> String -> IO ExitCode
run dir exe = do 
    let pi = let exepath = dir </> exe in shell $ intercalate " " $ if win32 then ["start", exepath] else [exepath]
    (_, _, _, h) <- createProcess $ pi {cwd = Just dir}
    waitForProcess h

getCabal :: IO (Maybe String)
getCabal = go =<< getCurrentDirectory
  where
    go p = getDirectoryContents p >>= \x ->
      case filter ((".cabal" ==) . snd . splitExtension) x of
        [cabal] -> return $ Just $ p </> cabal
        _ -> let levelUp = takeDirectory p in
          if levelUp == p
            then return Nothing
            else go levelUp

getExeName :: String -> IO String
getExeName f = do
    return $ joinPath ["dist", "build", f, if win32 then f <.> "exe" else f]

main :: IO ()
main = getCabal >>= \x -> exitWith =<< case x of
      Nothing -> return $ ExitFailure (-1)
      Just cabal -> do 
        exe <- getExeName (takeBaseName cabal)
        run (dropFileName cabal) exe
