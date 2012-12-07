import Data.List

wildcardMatch :: String -> String -> Bool
wildcardMatch [] [] = True
wildcardMatch ('?' : pat) (c : cs) = wildcardMatch pat cs
wildcardMatch pat@('*' : pat_tail) s = wildcardMatch pat_tail s || wildcardMatch pat (if null s then [] else tail s)
wildcardMatch (p : pat) (c : cs) = if p == c then wildcardMatch pat cs else False
wildcardMatch _ _ = False

main = do
    print $ wildcardMatch "a?d*p*" "a1d345p"
