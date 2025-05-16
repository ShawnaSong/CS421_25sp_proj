module Main where

import Data.Ratio
import Data.List (unfoldr)
import System.CPUTime
import Text.Printf

-- === rats3: Enumeration via GCD Trace ===

igcd :: (Integer, Integer) -> (Integer, [Bool])
igcd (m,n)
  | m < n     = step False (igcd (m, n - m))
  | m > n     = step True  (igcd (m - n, n))
  | otherwise = (m, [])
  where step b (d, bs) = (d, b : bs)

ungcd :: (Integer, [Bool]) -> (Integer, Integer)
ungcd (d, bs) = foldr undo (d,d) bs
  where
    undo False (m,n) = (m, n + m)
    undo True  (m,n) = (m + n, n)

boolSeqs :: [[Bool]]
boolSeqs = [] : [ b : bs | bs <- boolSeqs, b <- [False, True] ]

mkRat :: (Integer, Integer) -> Rational
mkRat (m,n) = m % n

rats3 :: [Rational]
rats3 = map (mkRat . curry ungcd 1) boolSeqs

-- === rats4: Stern-Brocot Tree ===

data Tree a = Node a (Tree a) (Tree a)

unfoldTree :: (b -> (a, b, b)) -> b -> Tree a
unfoldTree f seed = Node x (unfoldTree f l) (unfoldTree f r)
  where (x, l, r) = f seed

bf :: Tree a -> [a]
bf t = concat (levels t)
  where
    levels (Node x l r) = [x] : zipWith (++) (levels l) (levels r)

rats4 :: [Rational]
rats4 = bf (unfoldTree step ((0,1), (1,0)))
  where
    step (l@(m1,n1), r@(m2,n2)) =
      let m = m1 + m2
          n = n1 + n2
      in (m % n, (l, (m,n)), ((m,n), r))

-- === rats6: Calkin-Wilf Tree ===

rats6 :: [Rational]
rats6 = bf (unfoldTree step (1,1))
  where step (m,n) = (m % n, (m, m+n), (m+n, n))

-- === rats7: Constant-Cost Iterator ===

rats7 :: [Rational]
rats7 = iterate next 1

next :: Rational -> Rational
next x = recip (fromInteger n + 1 - f)
  where (n, f) = properFraction x

-- === Print first 20 results ===

-- Format rational as "m/n" instead of "m % n"
formatRational :: Rational -> String
formatRational r = show (numerator r) ++ "/" ++ show (denominator r)

-- Print 20 values horizontally, 10 per row
printList :: String -> [Rational] -> IO ()
printList title xs = do
  putStrLn $ "\n=== " ++ title ++ " ==="
  let rows = chunksOf 10 (map formatRational (take 20 xs))
  mapM_ (putStrLn . unwords) rows

-- Helper to chunk list into pieces of n elements
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- === Benchmarking ===

measure :: String -> [Rational] -> IO ()
measure label xs = do
  start <- getCPUTime
  let vals = take 1000000 xs
  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10^12) :: Double
  printf "%-20s: %.6f seconds\n" label diff

main :: IO ()
main = do
  printList "rats3 (GCD trace method)" rats3
  printList "rats4 (Stern-Brocot tree)" rats4
  printList "rats6 (Calkin-Wilf tree)" rats6
  printList "rats7 (Constant-cost iterator)" rats7
  putStrLn "Benchmarking 100,000 elements of each method:"
  measure "rats3 (GCD trace)" rats3
  measure "rats4 (Stern-Brocot)" rats4
  measure "rats6 (Calkin-Wilf)" rats6
  measure "rats7 (Iterator)" rats7

