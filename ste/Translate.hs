{-# LANGUAGE OverloadedStrings, PatternGuards, RecordWildCards, NamedFieldPuns, BangPatterns #-}

module Translate
  ( testParse
  ) where

import qualified Data.ByteString.Lazy.Char8 as L
import Data.List
import Control.Monad
import Control.Monad.State
import Control.Applicative
import Debug.Trace

type CodePos = (Int, Int)

data ParserS =
  ParserS
  {
   psCode :: String,
   psFile :: String,
   psPos :: CodePos
  }

data ParseRes a =
  ParseError CodePos String |
  ParseRight a

data Token =
  TokenString String |
  TokenTag String

instance Show ParserS where
  show ParserS{..} = let
    (l, i) = psPos
    in concat ["ParserS: ", psFile, "@", show l, ":", show i]

strShow :: String -> String
strShow s = tail . init . show $ s

instance Show Token where
  show t = case t of
    TokenString str -> "String " ++ strShow str
    TokenTag tag    -> "<" ++ strShow tag ++ ">"

move :: ParserS -> ParserS
move s@ParserS{psCode, psPos = (l, i)} = case psCode of
  c : cs | c == '\n' -> s{psCode = cs, psPos = (l + 1, 1)}
         | otherwise -> s{psCode = cs, psPos = (l, i + 1)}
  [] -> s

newParser :: String -> String -> ParserS
newParser text file = ParserS text file (1, 1)

testParse :: String -> IO ()
testParse file = do
    text <- readFile file
    let s = newParser text file
    print "hello"
