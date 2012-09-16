{-# LANGUAGE ScopedTypeVariables #-}

import Data.List
import Debug.Trace (trace, traceShow)

data DLList a = 
    DLNull
  | DLNode
      { prev :: DLList a
      , x :: a
      , next :: DLList a
      }
  deriving (Show)
  
showDLList :: Show a => DLList a -> String
showDLList DLNull = "Null"
showDLList (DLNode _ x _) = show x

walkDLList :: (DLList a -> DLList a) -> DLList a -> [a]
walkDLList _ DLNull = []
walkDLList f n@(DLNode _ x _)  = x : walkDLList f (f n)

makeDLList :: [a] -> (DLList a, DLList a)
makeDLList xs = let (first, last) = step DLNull xs in (first, last)
  where
    step prev [] = (DLNull, prev)
    step prev (x : xs) = let
        this = DLNode prev x next
        (next, last) = step this xs
      in
        (this, last)
    
testList :: [Int] -> IO ()
testList l = let
    (first, last) = makeDLList l
    byNext = walkDLList next first
    byPrev = walkDLList prev last
  in do
    putStrLn $ "Testing: " ++ show l
    print byNext
    print byPrev

main = do
    testList []
    testList [1, 2, 3, 4]
