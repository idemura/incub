import Data.Char
import Data.List
import Data.Function (on)
import Debug.Trace (trace)

data SourcePos = SourcePos Int Int

getLine :: SourcePos -> Int
getLine (SourcePos l _) = l

getColumn :: SourcePos -> Int
getColumn (SourcePos _ c) = c

data Token = TWord SourcePos String | TPunct SourcePos Char | TEof

longest :: Show a => (a -> a -> Ordering) -> [a] -> [a]
longest ord lst = aux lst []
  where
    aux [] s = reverse $ fst $ maximumBy (compare `on` snd) s
    aux (x : xs) s = let
      s' = ([x], 1) : ([(x : l, n + 1) | (l, n) <- s, ord (head l) x == LT] ++ s)
      in aux xs s'

type WeightHeight = (Int, Int)

circus :: [WeightHeight] -> [WeightHeight]
circus g = longest (compare `on` snd) $ sortBy (compare `on` fst) g

main = do
  let group = [(56, 90), (60, 85), (65, 100), (68, 86), (70, 87), (75, 88)]
  print group
  print $ circus group

