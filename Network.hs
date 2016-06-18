module Main where

import qualified Data.Map as M

import Data.Matrix
import Data.Array
import Data.List hiding (transpose)
import System.Random
import Control.Monad
import Control.Concurrent
import Graphics.Vty
import Graphics.Vty.Image
import Graphics.Vty.Attributes
import Data.Default(def)
import System.IO

import Types
import Western

import System.IO

timeout = 100
netSize = [6, 6]
netNum = 50
iterNum = 50
bestNum = 10
modP = 0.02
thr = 0.6

ntLevelSize :: Network -> [Int]
ntLevelSize n = (map nrows $ ntWeights n) ++ [ncols $ last $ ntWeights n]

boolToFloat :: Bool -> Float
boolToFloat False = 0
boolToFloat True = 1

stdNeuron :: Float -> Float
stdNeuron x = 1 / (1 + exp (-x + 1.5))

evalNetwork :: Network -> [Float] -> [Float] -- evaluate network on a given input
evalNetwork net input = foldl evalColumn input (ntWeights net)
  where evalColumn col mat = map stdNeuron $ toList $ multStd mat (transpose (fromLists [col]))

makeInput :: GameState -> [Float]
makeInput ((x1, y1, b1), (x2, y2, b2)) = [fromIntegral x1 / fromIntegral w, fromIntegral y1 / fromIntegral h, boolToFloat b1,  fromIntegral x2 / fromIntegral w, fromIntegral y2 / fromIntegral h, boolToFloat b2]
  where 
  ((_,_),(w,h)) = bounds testMap


decideTurn :: [Float] -> Turn
decideTurn [l, r, u, d, s, f] 
 | f > thr = Fire
 | s > thr = S
 | l > thr && r > thr = S
 | u > thr && d > thr = S
 | r > thr && u > r = U
 | u > thr && r >= u = R
 | r > thr && d > r = D
 | d > thr && r >= d = R
 | l > thr && d > l = D
 | d > thr && l >= d = L
 | l > thr && u > l = U
 | u > thr && l >= u = L
 | r > thr = R
 | l > thr = L
 | u > thr = U
 | d > thr = D
 | otherwise = S

playGame :: Network -> Network -> (Int, Int)
playGame p1 p2 = result $ foldl makeTurn (Just (Left ((1, 1, False), (w, h, False)))) [0..timeout]  
  where 
  ((_,_),(w,h)) = bounds testMap
  makeTurn (Just (Right x)) _ = Just (Right x)
  makeTurn (Just (Left x)) n 
   | n == timeout = Nothing
   | n `rem` 2 == 0 = turn (Left (decideTurn $ evalNetwork p1 $ makeInput x)) testMap x
   | otherwise = turn (Right (decideTurn $ evalNetwork p2 $ makeInput (snd x, fst x))) testMap x
  result Nothing = (0, 0)
  result (Just (Right Player1Won)) = (1, 0)
  result (Just (Right Player2Won)) = (0, 1)

playNTurns :: Network -> Network -> Int -> [(Maybe (Either GameState Outcome), Int)]
playNTurns p1 p2 n = gameGoesOn (take n $ iterate makeTurn ((Just (Left ((1, 1, False), (w, h, False)))), 0)) 
  where
  ((_,_),(w,h)) = bounds testMap
  makeTurn ((Just (Right x)), m ) = (Just (Right x), m + 1 )
  makeTurn (Just (Left x), m) 
   | m `rem` 2 == 0 = (turn (Left (decideTurn $ evalNetwork p1 $ makeInput x)) testMap x, m + 1)
   | otherwise = (turn (Right (decideTurn $ evalNetwork p2 $ makeInput (snd x, fst x))) testMap x, m + 1)
  gameGoesOn [] = []
  gameGoesOn ((Just (Left x), n) : xs) = ((Just (Left x)), n) : (gameGoesOn xs)
  gameGoesOn ((Just (Right x), n) : xs) = [(Just (Right x), n)]

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
 matrices (x:xs) = (getWord $ toList x) ++ "\n" ++ matrices xs
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



evolveNet :: [Network] -> IO [Network]
evolveNet nets = do
 let first = takeNBest nets bestNum
 second <- mapM (\x -> perturbNetwork x modP) first
 third <- mapM (\x -> makeRandomNetwork netSize) [1..(netNum - 2*bestNum)]
 return $ first ++ second ++ third

--main = testRender

trainNet :: IO Network
trainNet = do
 first <- mapM (\x -> makeRandomNetwork netSize) [1..netNum]
 x <- foldr (.) id (replicate iterNum (\x -> x >>= evolveNet)) (return first)
 return (head x)

drawBattle x y n = do
 let pics = map (\x -> renderAnyGame testMap (fst x)) $ playNTurns x y n
 vty <- mkVty def
 mapM_ (drawAndWait vty) pics
 evt <- nextEvent vty
 shutdown vty
  where 
   drawAndWait scr pic = do
    update scr pic
    threadDelay 100000

writeNetToFile :: Network -> String -> IO ()
writeNetToFile net filename = do
 writeFile filename (netToString net)

readNetFromFile :: [Int] -> String -> IO Network
readNetFromFile sizes filename = do
 mats <- readFile filename
 let nums = map stringsToFloats (lines mats)
 return Network {ntWeights = zipWith fromListMy (zip sizes $ tail sizes) nums}
 where 
  stringsToFloats s = map ( \x -> read x :: Float ) (words s) 
  fromListMy (x,y) s = fromList x y s 

main = do
 x <- trainNet
 y <- trainNet
 writeNetToFile x "net1"
 writeNetToFile y "net2"

--randomNetwork :: [Int] -> Float -> Network -- create random network with levels of size of the first argument and with absolute value of weights of the order of the second argument


