import Data.List
import qualified Data.Map as Map

type Table = Map.Map Integer [Integer]

addFactor :: Integer -> [(Integer, Int)] -> [(Integer, Int)]
addFactor n xxs@(f@(x, i) : xs)
  | n > x = f : addFactor n xs
  | n < x = (n, 1) : xxs
  | otherwise = (x, i + 1) : xs
addFactor n [] = [(n, 1)]

primes :: [Integer]
primes = sieve [2..]
  where
    sieve :: [Integer] -> [Integer]
    sieve (x : xs) = x : (sieve $ filter (\n -> n `rem` x /= 0) xs)

getFactor :: Integer -> (Integer, Integer)
getFactor n = step n primes
  where
    step :: Integer -> [Integer] -> (Integer, Integer)
    step n (p : ps) = let (q, r) = quotRem n p
                      in if p > q then (1, n)
                                  else if r == 0 then (p, q) else step n ps

getDivisiors :: [(Integer, Int)] -> [Integer]
getDivisors ((x, i) : xs) = 
getDivisors [] = []

{-
tabgetFactor :: Table -> Integer -> [Integer]
tabgetFactor tab n = case getFactor n of
                       (1, m) -> [m]
                       (p, q) -> let (Just ps) = Map.lookup p tab
                                     (Just qs) = Map.lookup q tab
                                 in merge ps qs

solve :: Int -> Integer
solve k = solve' 0 2 Map.empty
  where
    solve' :: Int -> Integer -> Table -> Integer
    solve' j n tab
      | j == k = n - fromIntegral j
      | otherwise = let factors = tabgetFactor tab n
                        j' = if length factors == k then j + 1 else 0 
                    in solve' j' (n + 1) (Map.insert n factors tab)
-}

main :: IO ()
main = do
    let fs = [(2, 1), (5, 2)]
    print $ addFactor 3 fs
    print $ addFactor 7 fs
    print $ addFactor 2 fs
