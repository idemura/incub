{-# LANGUAGE OverloadedStrings, PatternGuards #-}

module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as S
import Data.List
import Control.Monad.Trans
import Control.Monad
import Control.Applicative
import System.Environment (getArgs)
import Base
import Snap
import Snap.Util.FileServe
import Pages

echo :: Snap ()
echo = do
    param <- getParam "param"
    writeBS $ maybe "Must specify echo/param in URL" id param

main :: IO ()
main = do
    initialCfg <- commandLineConfig emptyConfig
    let cfg = setAccessLog ConfigNoLog $ initialCfg
    httpServe cfg handlers
  where
    handlers :: Snap ()
    handlers = 
        ifTop (runApp index)
        <|> route routeMap
        <|> dir "res" (serveDirectory "res")
      where
        routeMap =
          [ ("newpost", runApp newPost)
          , ("login", runApp login)
          , ("loginCheck", runApp loginCheck)
          , ("logout", runApp logout)
          , ("echo/:param", echo)
          , ("wallet", runApp wallet)
          ]

