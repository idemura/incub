{-# LANGUAGE OverloadedStrings, PatternGuards #-}

module Main (main) where

import Data.List
import Control.Monad
import Control.Applicative
import System.IO (stderr, stdout, hPutStrLn)
import Translate

putStdOut, putStdErr :: String -> IO ()
putStdOut = hPutStrLn stdout
putStdErr = hPutStrLn stderr

main :: IO ()
main = do
    let f = "Test.st"
    testParse f

  where
    printErr f (ln, msg) = putStdErr $ concat ["Error (", f, "@", show ln, ") : ", msg]
