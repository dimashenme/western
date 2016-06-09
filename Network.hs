module Main where

import qualified Data.Map as M

import Data.Matrix
import Data.Array
import Data.List hiding (transpose)
import System.Random
import Control.Monad

import Types
import Western

import System.IO

timeout :: Int
timeout = 100

ntLevelSize :: Network -> [Int]
ntLevelSize n = (map nrows $ ntWeights n) ++ [ncols $ last $ ntWeights n]

boolToFloat :: Bool -> Float
boolToFloat False = 0
boolToFloat True = 1

stdNeuron :: Float -> Float
stdNeuron x = 1 / (1 + exp (-x))

evalNetwork :: Network -> [Float] -> [Float] -- evaluate network on a given input
evalNetwork net input = foldl evalColumn input (ntWeights net)
  where evalColumn col mat = map stdNeuron $ toList $ multStd mat (transpose (fromLists [col]))

makeInput :: GameState -> [Float]
makeInput ((x1, y1, b1), (x2, y2, b2)) = [fromIntegral x1, fromIntegral y1, boolToFloat b1,  fromIntegral x2, fromIntegral y2, boolToFloat b2]

decideTurn :: [Float] -> Turn
decideTurn [l, r, u, d, s, f] 
 | f > 0.75 = Fire
 | s > 0.75 = S
 | l > 0.75 && r > 0.75 = S
 | u > 0.75 && d > 0.75 = S
 | l > 0.75 && u > r = U
 | r > 0.75 && r >= u = R
 | u > 0.75 && d > r = D
 | d > 0.75 && r >= d = R
 | u > 0.75 && d > l = D
 | d > 0.75 && l >= d = L
 | u > 0.75 && u > l = U
 | d > 0.75 && l >= u = L
 | r > 0.75 = R
 | l > 0.75 = L
 | u > 0.75 = U
 | d > 0.75 = D

playGame :: Network -> Network -> (Int, Int)
playGame p1 p2 = result $ foldl makeTurn (Just (Left ((1, 1, False), (w, h, False)))) [0..timeout]  
  where 
  ((_,_),(w,h)) = bounds testMap
  makeTurn (Just (Right x)) _ = Just (Right x)
  makeTurn (Just (Left x)) n 
   | n == timeout = Nothing
   | n `rem` 2 == 0 = turn (Left (decideTurn $ evalNetwork p1 $ makeInput x)) testMap x
   | otherwise = turn (Right (decideTurn $ evalNetwork p2 $ makeInput x)) testMap x
  result Nothing = (0, 0)
  result (Just (Right Player1Won)) = (1, 0)
  result (Just (Right Player2Won)) = (0, 1)

playTournament :: [Network] -> [Int]
playTournament s = map (\x -> (sum $ fmap fst (getRow x table)) + (sum $ fmap snd (getCol x table))) [1..l]
  where 
  l = length s
  table = matrix l l (\(i, j) -> playGame (s !! (i-1)) (s !! (j-1)))

takeNBest :: [Network] -> Int -> [Network]
takeNBest s n = take n $ map fst (sortOn ((*(-1)).snd) $ zip s (playTournament s))

netToString :: Network -> String
netToString n = matrices $ ntWeights n
 where 
 matrices [] = ""
 matrices (x:xs) = show (nrows x) ++ " " ++ show (ncols x) ++ "\n" ++ unlines (map getWord (toLists x)) ++ "\n" ++ matrices xs
 getWord s = unwords $ map show s

makeRandomMatrix :: (Int,Int) -> Float -> IO (Matrix Float)
makeRandomMatrix (i, j) magnitude = do
 g <- newStdGen
 return (fromList i j (take (i*j) (randomRs (0, magnitude) g :: [Float])))

makeRandomMatrices :: [Int] -> Float -> IO [Matrix Float]
makeRandomMatrices s magnitude = do
 let dimlist = zip s $ tail s
 matlist <- mapM (\x->makeRandomMatrix x magnitude) dimlist
 return matlist

makeRandomNetwork :: [Int] -> IO Network
makeRandomNetwork s = do
 matlist <- makeRandomMatrices s 1
 return Network {ntWeights = matlist}

perturbNetwork :: Network -> Float -> IO Network 
perturbNetwork n magnitude = do
 let a = ntWeights n
 b <- makeRandomMatrices (ntLevelSize n) magnitude
 return Network {ntWeights = (zipWith (elementwise (+)) a b)}


netSize = [6, 6, 6]
netNum = 100
iterNum = 100
bestNum = 10
modP = 0.02

evolveNet :: [Network] -> IO [Network]
evolveNet nets = do
 let first = takeNBest nets bestNum
 second <- mapM (\x -> perturbNetwork x modP) first
 third <- mapM (\x -> makeRandomNetwork netSize) [1..(netNum - 2*bestNum)]
 return $ first ++ second ++ third

main = do
 first <- mapM (\x -> makeRandomNetwork netSize) [1..netNum]
 x <- foldr (.) id (replicate iterNum (\x -> x >>= evolveNet)) (return first)
 putStr $ netToString $ head x
 test <- mapM (\x -> makeRandomNetwork netSize) [1..9]
 putStr $ show $ playTournament (head x : test) 

--randomNetwork :: [Int] -> Float -> Network -- create random network with levels of size of the first argument and with absolute value of weights of the order of the second argument


