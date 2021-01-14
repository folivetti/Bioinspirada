module Main where

import System.Random
import Control.Monad
import Data.Bool

type SAT = [Bool] -> Double

spin :: Ord a => [(a, b)] -> a -> b
spin wheel r = snd $ head $ dropWhile ((<=r).fst) wheel

calcProb :: Fractional b => [b] -> [b]
calcProb xs = map (/sum xs) xs

cumsum :: [Double] -> [Double]
cumsum = scanl1 (+)

getBestWith :: Ord c => (a -> c) -> [a] -> c
getBestWith f = maximum . map f

invertAt :: Int -> [Bool] -> [Bool]
invertAt ix xs = let (ys, z:zs) = splitAt ix xs
                 in  ys ++ not z : zs 

change :: [Bool] -> IO [Bool]
change xs = do ix <- randomRIO (0, length xs - 1)
               return (invertAt ix xs)

toFunc :: [[Int]] -> [Bool] -> Double
toFunc xs bs = fromIntegral . sum $ map checkTruth xs 
  where
    checkTruth clause = bool 0 1 $ any get clause
    get ix
      | ix < 0    = not $ bs !! (-ix - 1)
      | otherwise = bs !! (ix - 1)

initialPop :: Int -> IO [[Bool]]
initialPop n = replicateM n randomSolution

randomSolution :: IO [Bool]
randomSolution = replicateM 20 randomIO

select :: SAT -> [[Bool]] -> IO [[Bool]]
select f pop = do let n       = length pop
                      fitness = map f pop
                      prob    = calcProb fitness
                      wheel   = zip (cumsum prob) pop
                  map (spin wheel) <$> replicateM n (randomRIO (0,1)) 


loop :: SAT -> Int -> [[Bool]] -> IO ()
loop f n pop = do let best = getBestWith f pop
                  putStrLn ("Best result in generation " ++ show n ++ ": " ++ show best)
                  p  <- select f pop
                  p' <- mapM change p
                  if n == 1000 || best == 91
                     then return ()
                     else loop f (n+1) p'

ga :: SAT -> IO ()
ga f = do pop <- initialPop 200
          loop f 0 pop

main :: IO ()
main = do dat <- map (map read.words) . lines <$> readFile "uf20-0770.cnf" :: IO [[Int]]
          let f = toFunc dat
          ga f
