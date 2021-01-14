module Main where

import Data.Tree
import Data.List
import System.Random
import Data.Bifunctor
import Data.Map.Strict ((!))
import Control.Arrow ((&&&))
import qualified Data.Map.Strict as M

data Symb = Add | Sub | Mul | Div | Exp | Log | IfEq0 | Var | Const
          deriving (Enum, Bounded, Ord, Eq, Show)

instance Random Symb where
  random g         = first toEnum $ randomR (0,length [Add ..]) g
  randomR (a, b) g = first toEnum $ randomR (fromEnum a, fromEnum b) g

arity = M.fromList $ zip [Add .. Const] [2,2,2,2,1,1,3,0,0]

forM xs f     = mapM (const f) xs
sampleTerm    = randomRIO (Var, Const)
sampleNonTerm = randomRIO (Add, IfEq0)
sampleAny     = randomRIO (Add, Const)
length'       = fromIntegral . length
n_term        = length' [Var .. Const] :: Double
n_symbols     = length' [Add .. Const]

full :: Int -> IO (Tree Symb)
full max_depth = do node     <- if max_depth == 0
                                   then sampleTerm
                                   else sampleNonTerm
                    children <- forM [1..arity ! node] (full (max_depth - 1)) 
                    return (Node node children)

grow :: Int -> IO (Tree Symb)
grow max_depth = do let ratio = n_term / n_symbols
                    r    <- randomRIO (0, 1) 
                    node <- if max_depth == 0 || r < ratio
                              then sampleTerm
                              else sampleNonTerm
                    children <- forM [1..arity ! node] (grow (max_depth - 1))
                    return (Node node children)

ramped :: Int -> Int -> Int -> IO [Tree Symb]
ramped min_depth max_depth 0 = return []
ramped min_depth max_depth n_pop = 
  do let range     = max_depth - min_depth + 1
         n         = n_pop `div` range
         half      = n `div` 2
         remainder = n `rem` 2
     treesFull <- forM [1..half] (full min_depth)
     treesGrow <- forM [1..half+remainder] (grow min_depth)
     trees     <- ramped (min_depth+1) max_depth (n_pop - n)
     return (treesFull <> treesGrow <> trees)

mutation f tree = do
  let n = numberOfNodes tree
  point <- randomRIO (0, n-1)
  changeAt point f tree

crossover tree1 tree2 = do
  let n1 = numberOfNodes tree1
      n2 = numberOfNodes tree2
  p1 <- randomRIO (0, n1-1)
  p2 <- randomRIO (0, n2-1)
  let branch1 = getBranchAt p1 tree1
      branch2 = getBranchAt p2 tree2
  child1 <- changeAt p1 (const (return branch2)) tree1 
  child2 <- changeAt p2 (const (return branch1)) tree2 
  return (child1, child2)

getBranchAt :: Int -> Tree a -> Tree a
getBranchAt 0 t           = t
getBranchAt 1 t           = t
getBranchAt p (Node n ts) = let (n',p') = whichChildren (p-1) ts
                            in  getBranchAt p' n'

whichChildren p (t:ts)
  | n < p     = whichChildren (p-n) ts
  | otherwise = (t,p)
  where
    n = numberOfNodes t

changeAt 0 f n = f n
changeAt 1 f n = f n
changeAt p f (Node n ts) = do
  ts' <- changeChildren (p-1) f ts
  return (Node n ts')

changeChildren p f []          = return []
changeChildren p f (t:ts)
  | n < p     = do ts' <- changeChildren (p-n) f ts
                   return (t:ts')
  | otherwise = do t' <- changeAt p f t
                   return (t':ts)
  where
    n = numberOfNodes t

sampleUno  = randomRIO (Exp, Log)
sampleDuo  = randomRIO (Add, Div)
sampleTrio = return IfEq0

changeNode (Node n ts) = 
  do n' <- case arity ! n of
             0 -> sampleTerm
             1 -> sampleUno
             2 -> sampleDuo
             3 -> sampleTrio
     return (Node n' ts)

swapChildren (Node n ts) =
  do let tss = permutations ts
     ix <- randomRIO (0, length tss - 1)
     return (Node n (tss !! ix))

shrink (Node n []) = return (Node n [])
shrink (Node n ts) = 
  do ix <- randomRIO (0, length ts - 1)
     return (ts !! ix)

depth :: Tree a -> Int
depth (Node n []) = 0
depth (Node n ts) = 1 + maximum (map depth ts)

numberOfNodes :: Tree a -> Int
numberOfNodes (Node n []) = 1
numberOfNodes (Node n ts) = 1 + sum (map numberOfNodes ts)

count :: [Int] -> [(Int, Int)]
count = map (head &&& length) . group . sort

printTree tree = putStrLn $ drawTree (fmap show tree)

main :: IO ()
main = do
  tree <- full 3
  printTree tree
  tree' <- mutation shrink tree
  printTree tree'
  (c1,c2) <- crossover tree tree'
  printTree c1
  printTree c2
  pop <- ramped 2 4 1000
  print $ count (map depth pop)
