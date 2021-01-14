module Main where

import System.Random
import Control.Monad
import Data.Bool
import Data.List

-- * Utilities
randomIndividual :: IO [Int]
randomIndividual = replicateM 5 (randomRIO (0,1))

randomPopulation :: Int -> IO [[Int]]
randomPopulation nPop = replicateM nPop randomIndividual

merge :: [(a,a)] -> [a]
merge [] = []
merge ((x1,x2):xs) = x1:x2:merge xs

forEach :: Monad f => [a1] -> (a1 -> f (a2, a2)) -> f [a2]
forEach xs f = merge <$> mapM f xs

-- * Problem dependent

decode :: Num a => [a] -> a
decode [b1, b2, b3, b4, b5] = 16*b1 + 8*b2 + 4*b3 + 2*b4 + b5

fitness :: (Integral a, Num b) => [a] -> b
fitness bits = fromIntegral (decode bits ^ 2)

popFitness :: [[Int]] -> IO [Double]
popFitness pop = return (sigmaScale 2 (map fitness pop))

-- * Parent selection 
probability :: [[Int]] -> [Double] -> [([Int], Double)]
probability pop fs = let tot   = sum fs
                         probs = map (/tot) fs
                     in  zip pop (cumsum probs)

cumsum :: [Double] -> [Double]                  
cumsum = scanl1 (+)

sigmaScale :: (Floating a, Ord a) => a -> [a] -> [a]
sigmaScale c fs = map scale fs
  where
    scale fi = max 0.0 (fi - (fbar - c*fsigma))
    fbar     = sum fs / n
    fsigma   = sqrt (sum (map dev fs) / n)
    dev fi   = (fi - fbar)^2
    n        = fromIntegral (length fs)

rank :: [Double] -> [Double]
rank = map fst . sortOn snd . zip [1..] . map fst . sortOn snd . zip [0..] 

rankLinearScale :: Double -> [Double] -> [Double]
rankLinearScale s ranks = map scale ranks
  where
    c       = (2-s)/mu
    mu      = maximum ranks 
    scale i = c + 2*(i-1)*(s-1)/(mu*(mu-1))

rankExpScale :: [Double] -> [Double]
rankExpScale ranks = let ranks' = map (\i -> 1 - exp (-i)) ranks
                     in  map (/sum ranks') ranks'

roulette :: Int -> [[Int]] -> [Double] -> IO [([Int],[Int])]
roulette n pop fs = do
  let wheel = probability pop fs
  p1 <- replicateM (n `div` 2) (spin wheel)
  p2 <- replicateM (n `div` 2) (spin wheel)
  return (zip p1 p2)

sus :: Int -> [[Int]] -> [Double] -> IO [([Int], [Int])]
sus n pop fs = do
  let wheel = probability pop fs
      n'    = fromIntegral (n `div` 2)
  r1 <- iterate (+(1/n')) <$> randomRIO (0, 1/n')
  r2 <- iterate (+(1/n')) <$> randomRIO (0, 1/n')
  let p1 = map (`grab` wheel) r1
      p2 = map (`grab` wheel) r2
  return (take (n `div` 2) (zip p1 p2))

spin :: [([Int], Double)] -> IO [Int]
spin wheel = do
  r <- randomRIO (0, 1)
  return (grab r wheel)

grab :: Ord b => b -> [(c, b)] -> c
grab r = fst . head . dropWhile ((<=r).snd)

tournament :: Int -> [[Int]] -> [Double] -> IO [([Int],[Int])]
tournament n pop fs = do
  p1 <- replicateM n (fight pop fs)
  p2 <- replicateM n (fight pop fs)
  return (zip p1 p2)

fight :: [[Int]] -> [Double] -> IO [Int]
fight pop fs = do
  ix1 <- randomRIO (0, length pop - 1)
  ix2 <- randomRIO (0, length pop - 1)
  let p1 = pop !! ix1
      p2 = pop !! ix2
  return (bool p1 p2 (fs !! ix1 < fs !! ix2))

-- * Crossover

onePoint :: Double -> ([Int], [Int]) -> IO ([Int], [Int])
onePoint pc (p1, p2) = do
  cx    <- randomRIO (0, 1)
  if cx <= pc
     then do
       point <- randomRIO (0, length p1 - 1)
       let (p1a, p1b) = splitAt point p1
           (p2a, p2b) = splitAt point p2
       return (p1a<>p2b, p2a<>p1b)
     else return (p1, p2)

nPoint :: Double -> Int -> ([Int], [Int]) -> IO ([Int], [Int])
nPoint pc n (p1, p2) = do
  cx <- randomRIO (0, 1)
  if cx <= pc
     then do
       points <- sort <$> replicateM n (randomRIO (0, length p1 - 1))
       return (splitAll points (p1, p2))
     else return (p1, p2)

splitAll :: [Int] -> ([Int], [Int]) -> ([Int], [Int])
splitAll [] (p1, p2) = (p1, p2)
splitAll (ix:ixs) (p1, p2) = 
  let (p1a, p1b) = splitAt ix p1
      (p2a, p2b) = splitAt ix p2
  in  splitAll ixs (p1a<>p2b, p2a<>p1b)

uniformCX :: Double -> Double -> ([Int], [Int]) -> IO ([Int], [Int])
uniformCX pc thr (p1, p2) = do
  cx <- randomRIO (0, 1)
  if cx <= pc
     then do 
       rs <- replicateM (length p1) (randomRIO (0, 1))
       let c1 = zipWith3 (\g1 g2 r -> bool g2 g1 (r <= thr)) p1 p2 rs
           c2 = zipWith3 (\g1 g2 r -> bool g1 g2 (r <= thr)) p1 p2 rs
       return (c1,c2)
     else return (p1, p2)

-- * Mutation     

bitFlip :: Double -> [Int] -> IO [Int]
bitFlip pm []     = return []
bitFlip pm (x:xs) = do
  flip <- randomRIO (0, 1)
  ys   <- bitFlip pm xs
  if flip <= pm
     then return ((1-x):ys)
     else return (x:ys)

-- * Replacement
generational :: Int -> [[Int]] -> [Double] -> IO [[Int]]
generational n pop fs = return (take n (reverse pop))

-- * GA
ga :: IO ()
ga = do pop <- randomPopulation nPop
        step 0 pop

step :: Int -> [[Int]] -> IO ()
step n pop
  | terminate = return ()
  | otherwise = do fs       <- popFitness pop
                   parents  <- roulette nChildren pop fs
                   children <- forEach parents (onePoint pc)
                   xmen     <- mapM (bitFlip pm) children
                   fs'      <- popFitness pop
                   pop'     <- generational nPop (pop <> xmen) (fs <> fs')
                   print (maximum $ map fitness pop')
                   step (n+1) pop'
  where terminate = n==gens

-- parameters
pc        = 0.8
pm        = 0.1
nChildren = 4
nPop      = 4
gens      = 100

pop :: [[Int]]
pop = [[1,0,0,1,0], [0,0,0,0,1], [0,1,0,1,0], [1,0,0,0,1]]

-- >>> roulette 4 pop
-- [([1,0,0,0,1],[0,1,0,1,0]),([1,0,0,1,0],[1,0,0,0,1]),([1,0,0,0,1],[1,0,0,1,0]),([1,0,0,0,1],[1,0,0,1,0])]

-- >>> tournament 4 pop
-- [([1,0,0,1,0],[1,0,0,0,1]),([0,1,0,1,0],[1,0,0,1,0]),([1,0,0,1,0],[1,0,0,0,1]),([0,0,0,0,1],[1,0,0,1,0])]

-- >>> onePoint ([1,0,0,0,1],[0,1,0,1,0])
-- ([1,0,0,1,0],[0,1,0,0,1])

main :: IO ()
main = ga
