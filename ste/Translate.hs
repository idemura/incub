{-# LANGUAGE OverloadedStrings, PatternGuards, RecordWildCards, NamedFieldPuns #-}

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
type TranslateError = (CodePos, String)

data ParserS =
     ParserS {
       psCode :: String,
       psFile :: String,
       psPos :: CodePos,
       psErrors :: [TranslateError]
       psTokens :: [Token]
     } 

type ParserM = State ParserS

type ParserF a = ParserS -> (ParserS, a)

data Token = 
     TokenText String
   | TokenTag String

instance Show ParserS where
  show ParserS{..} =
      concat [ "Position: ", showPos psPos, errors psErrors ]
    where
      showPos (line, col) = concat [psFile, "@", show line, ":", show col]
      errStr (pos, msg) = ["  ", showPos pos, " - ", msg, "\n"]
      errors [] = "No errors."
      errors xs = concat $ "Errors:\n" : foldr ((:) . errStr) [] xs

strShow :: String -> String
strShow s = tail . init . show $ s

instance Show Token where
  show t = case t of
             TokenText text -> "Text " ++ strShow text
             TokenTag tag -> "<" ++ strShow tag ++ ">"

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst f (x, y) = (f x, y)

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (x, y) = (x, f y)

move :: Int -> ParserS -> ParserS
move 0 s = s
move n s@ParserS{psCode = c : cs, psPos = (line, col)}
  | c == '\n' = move (n - 1) s{psCode = cs, psPos = (line + 1, 1)}
  | otherwise = move (n - 1) s{psCode = cs, psPos = (line, col + 1)}
move _ s@ParserS{psCode = []} = s

addError :: String -> ParserS -> ParserS
addError msg s@ParserS{..} = s{psErrors = (psLine, msg) : psErrors}

comment :: ParserS -> ParserS
comment s@ParserS{psCode = '{' : '-' : _} = comment $ go $ move 2 s
  where
    go s@ParserS{psCode = '-' : '}' : _} = move 2 s
    go s@ParserS{psCode = '{' : '-' : _} = go $ go $ move 2 s
    go s@ParserS{psCode = []} = addError "{- without closing -}" s
    go s = go $ move 1 s
comment s = s

--<?> :: ParserF -> ParserF 

tokens :: ParserS -> (ParserS, [Token])
tokens s = readTag readChar $ comment s
  where
    addChar :: Char -> [Token] -> [Token]
    addChar c (TokenText cs : ts) = TokenText (c : cs) : ts
    addChar c ts = TokenText [c] : ts

    readChar :: ParserF [Token]
    readChar s@ParserS{psCode = c : _} = mapSnd (addChar c) (tokens $ move 1 s)
    readChar s = (s, [])

    readTag :: ParserF [Token] -> ParserF [Token]
    readTag f s'@ParserS{psCode = '{' : _} = cons $ go $ move 1 s
      where
        go s@ParserS{psCode = c : _}
            | c == '{' = (s, "")
            | c /= '}' = mapSnd (c :) $ go $ move 1 s
        go s = (s, "")

        cons (s@ParserS{psCode = '}' : _}, val) = push (parseTag val) (move 1 s)
        cons _ = (addError "Tag {} not closed" s', [])
    readTag f s = f s

push :: Token -> ParserF [Token]
push t s = mapSnd (t :) (tokens s)

parseTag :: String -> Token
parseTag s = TokenTag s

testParse :: String -> IO ()
testParse file = do
    text <- readFile file
    let s = ParserS text file 1 []
    let (s', ts) = tokens s
    case s' of
      ParserS{psErrors = [], psCode = []} -> mapM_ (putStrLn . show) ts
      s -> print s
    
