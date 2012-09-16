import Data.List
import System.IO
import Debug.Trace (trace)

rmNestedMultiline :: String -> String
rmNestedMultiline ('/' : '*' : cs) = rm cs
  where
    rm :: String -> String
    rm ('/' : '*' : cs) = rm (rm cs)
    rm ('*' : '/' : cs) = cs
    rm (c : cs) = rm cs
    rm [] = error "Unterminated comment"

rmToEoln :: String -> String
rmToEoln s = snd $ span (/= '\n') s

removeComments :: String -> String
removeComments c = reverse $ rm c []
  where
    rm :: String -> String -> String
    rm s@('/' : '/' : cs) out = rm (rmToEoln cs) ('\n' : out)
    rm s@('/' : '*' : cs) out = rm (rmNestedMultiline s) (' ' : out)
    rm (c : cs) out = rm cs (c : out)
    rm [] out = out

rmFinalNull :: [[a]] -> [[a]]
rmFinalNull [[]] = []
rmFinalNull (xs : xss) = xs : rmFinalNull xss

main = do
  s <- readFile "test/pp.txt"
  putStrLn $ removeComments s
  print $ rmFinalNull [[1, 2, 3], [4, 5], []]
  
