import Data.Ratio
import Data.List (intercalate)
import System.Environment (getArgs)
import Text.Read (readMaybe)

-- Generic binary tree type
data Tree a = Node a (Tree a) (Tree a)

-- Unfold a tree from a seed
unfoldTree :: (b -> (a, b, b)) -> b -> Tree a
unfoldTree f seed = Node x (unfoldTree f l) (unfoldTree f r)
  where (x, l, r) = f seed

-- Traverse tree by level
levels :: Tree a -> [[a]]
levels (Node x l r) = [x] : zipWith (++) (levels l) (levels r)

-- Calkin-Wilf tree construction
calkinWilfTree :: Tree Rational
calkinWilfTree = unfoldTree step (1,1)
  where step (m,n) = (m % n, (m, m+n), (m+n, n))

-- Stern-Brocot tree construction
sternBrocotTree :: Tree Rational
sternBrocotTree = unfoldTree step ((0,1), (1,0))
  where step (l@(m1,n1), r@(m2,n2)) =
          let m = m1 + m2; n = n1 + n2
          in (m % n, (l, (m,n)), ((m,n), r))

-- Format rational as string
showR :: Rational -> String
showR r = show (numerator r) ++ "/" ++ show (denominator r)

-- Pretty-print a tree's first N levels
printTree :: String -> Tree Rational -> Int -> IO ()
printTree label t depth = do
  putStrLn $ "\\n=== " ++ label ++ " ==="
  let lvl = take depth $ levels t
      maxWidth = 2 ^ (depth - 1)
      formatRow rs = replicate (maxWidth - length rs * 2) ' ' ++ intercalate "   " (map showR rs)
  mapM_ (putStrLn . formatRow) lvl

-- Read depth from command line
main :: IO ()
main = do
  args <- getArgs
  let depth = case args of
                (n:_) -> maybe 4 id (readMaybe n)
                _     -> 4
  printTree "Stern-Brocot Tree (rats4)" sternBrocotTree depth