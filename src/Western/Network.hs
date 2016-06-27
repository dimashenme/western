module Western.Network where

import qualified Data.Map as M

import Data.Matrix hiding ((!))
import Data.Array
import Data.List hiding (transpose)
import System.Random
import Control.Monad
import Control.Concurrent
import Data.Default(def)
import System.IO

import Western.Types
import Western.Game

timeout = 100
netSize = [8, 7, 6]
netNum = 50

-- | Number of iterations
iterNum = 100

bestNum = 23
modP = 0.02
thr = 0.6
bulNum = 2 :: Int

ntLevelSize :: Network -> [Int]
ntLevelSize n = map nrows (ntWeights n) ++ [ncols $ last $ ntWeights n]

boolToFloat :: Bool -> Float
boolToFloat False = 0
boolToFloat True = 1

stdNeuron :: Float -> Float
stdNeuron x = 1 / (1 + exp (-x))

evalNetwork :: Network -> [Float] -> [Float] -- evaluate network on a given input
evalNetwork net input = foldl evalColumn input (ntWeights net)
  where evalColumn col mat = map stdNeuron $ toList $ multStd (fromLists [col]) mat

makeInput :: GameState -> [Float]
makeInput ((x1, y1, n1, b1), (x2, y2, n2, b2)) = [fromIntegral x1 / fromIntegral w, fromIntegral y1 / fromIntegral h, fromIntegral n1 / fromIntegral bulNum, boolToFloat b1,  fromIntegral x2 / fromIntegral w, fromIntegral y2 / fromIntegral h, fromIntegral n2 / fromIntegral bulNum, boolToFloat b2]
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
playGame p1 p2 = result $ foldl makeTurn (Just (Left ((1, 1, bulNum, False), (w, h, bulNum, False)))) [0..timeout]  
  where 
  ((_,_),(w,h)) = bounds testMap
  makeTurn (Just (Right x)) _ = Just (Right x)
  makeTurn (Just (Left x)) n 
   | n == timeout = Nothing
   | even n = turn (Left (decideTurn $ evalNetwork p1 $ makeInput x)) testMap x
   | otherwise = turn (Right (decideTurn $ evalNetwork p2 $ makeInput (snd x, fst x))) testMap x
  result Nothing = (0, 0)
  result (Just (Right Player1Won)) = (1, 0)
  result (Just (Right Player2Won)) = (0, 1)

randomCoords :: Map -> IO ((Int, Int, Int), (Int, Int, Int))
randomCoords map = do
 g1 <- newStdGen
 g2 <- newStdGen
 let ((_,_),(w,h)) = bounds map
 let [x1, x2] = take 2 (randomRs (1, w) g1 :: [Int])
 let [y1, y2] = take 2 (randomRs (1, h) g2 :: [Int])
 let b2 = head $ randomRs (0, bulNum) g1 :: Int
 let b1 = head $ randomRs (0, bulNum) g2 :: Int

 if (map ! (x1, y1) /= 'x') && (map ! (x2, y2) /= 'x')
   then return ((x1, y1, b1), (x2, y2, b2))
   else randomCoords map

playRGame :: Network -> Network -> IO (Int, Int)  
playRGame p1 p2 = do
 ((x1, y1, b1), (x2, y2, b2)) <- randomCoords testMap
 return (result $ foldl makeTurn (Just (Left ((x1, y1, b1, False), (x2, y2, b2, False)))) [0..timeout])  
 where 
   makeTurn (Just (Right x)) _ = Just (Right x)
   makeTurn (Just (Left x)) n 
    | n == timeout = Nothing
    | even n = turn (Left (decideTurn $ evalNetwork p1 $ makeInput x)) testMap x
    | otherwise = turn (Right (decideTurn $ evalNetwork p2 $ makeInput (snd x, fst x))) testMap x
   result Nothing = (0, 0)
   result (Just (Right Player1Won)) = (1, 0)
   result (Just (Right Player2Won)) = (0, 1)

playRNTurns :: Network -> Network -> Int -> IO [(Maybe (Either GameState Outcome), Int)]
playRNTurns p1 p2 n = do 
 ((x1, y1, b1), (x2, y2, b2)) <- randomCoords testMap
 return $ gameGoesOn (take n $ iterate makeTurn (Just (Left ((x1, y1, b1, False), (x2, y2, b2, False))), 0))
   where
     makeTurn (Just (Right x), m) = (Just (Right x), m + 1 )
     makeTurn (Just (Left x), m) 
       | even m = (turn (Left (decideTurn $ evalNetwork p1 $ makeInput x)) testMap x, m + 1)
       | otherwise = (turn (Right (decideTurn $ evalNetwork p2 $ makeInput (snd x, fst x))) testMap x, m + 1)
     gameGoesOn [] = []
     gameGoesOn ((Just (Left x), n) : xs) = (Just (Left x), n) : gameGoesOn xs
     gameGoesOn ((Just (Right x), n) : xs) = [(Just (Right x), n)]


playNTurns :: Network -> Network -> Int -> [(Maybe (Either GameState Outcome), Int)]
playNTurns p1 p2 n = gameGoesOn (take n $ iterate makeTurn (Just (Left ((1, 1, bulNum, False), (w, h, bulNum, False))), 0)) 
  where
  ((_,_),(w,h)) = bounds testMap
  makeTurn (Just (Right x), m ) = (Just (Right x), m + 1 )
  makeTurn (Just (Left x), m) 
   | even m = (turn (Left (decideTurn $ evalNetwork p1 $ makeInput x)) testMap x, m + 1)
   | otherwise = (turn (Right (decideTurn $ evalNetwork p2 $ makeInput (snd x, fst x))) testMap x, m + 1)
  gameGoesOn [] = []
  gameGoesOn ((Just (Left x), n) : xs) = (Just (Left x), n) : gameGoesOn xs
  gameGoesOn ((Just (Right x), n) : xs) = [(Just (Right x), n)]

playTournament :: [Network] -> [Int]
playTournament s = map (\x -> (sum $ fmap fst (getRow x table)) + (sum $ fmap snd (getCol x table))) [1..l]
  where 
  l = length s
  table = matrix l l (\(i, j) -> playGame (s !! (i-1)) (s !! (j-1)))

playRTournament :: [Network] -> IO [Int]  
playRTournament s = do
 table <- sequence $ matrix l l (\(i, j) -> playRGame (s !! (i-1)) (s !! (j-1)))
 return $ map (\x -> (sum $ fmap fst (getRow x table)) + (sum $ fmap snd (getCol x table))) [1..l]
  where 
  l = length s

takeNRBest :: [Network] -> Int -> IO [Network]
takeNRBest s n = do
 results <- playRTournament s
 return $ take n $ map fst (sortOn ((*(-1)).snd) $ zip s results)

takeNBest :: [Network] -> Int -> [Network]
takeNBest s n =
 take n $ map fst (sortOn ((*(-1)).snd) $ zip s results)
 where results = playTournament s

netToString :: Network -> String
netToString n = matrices $ ntWeights n
 where 
 matrices [] = ""
 matrices (x:xs) = getWord (toList x) ++ "\n" ++ matrices xs
 getWord s = unwords $ map show s

makeRandomMatrix :: (Int,Int) -> Float -> IO (Matrix Float)
makeRandomMatrix (i, j) magnitude = do
 g <- newStdGen
 return (fromList i j (take (i*j) (randomRs (-magnitude, magnitude) g :: [Float])))

makeRandomMatrices :: [Int] -> Float -> IO [Matrix Float]
makeRandomMatrices s magnitude = do
 let dimlist = zip s $ tail s
 mapM (`makeRandomMatrix` magnitude) dimlist
 
makeRandomNetwork :: [Int] -> IO Network
makeRandomNetwork s = do
 matlist <- makeRandomMatrices s 1
 return Network {ntWeights = matlist}

perturbNetwork :: Network -> Float -> IO Network 
perturbNetwork n magnitude = do
 let a = ntWeights n
 b <- makeRandomMatrices (ntLevelSize n) magnitude
 return Network {ntWeights = zipWith (elementwise (+)) a b}



evolveNet :: [Network] -> IO [Network]
evolveNet nets = do
 let first = takeNBest nets bestNum
 second <- mapM (`perturbNetwork` modP) first
 third <- mapM (\x -> makeRandomNetwork netSize) [1..(netNum - 2*bestNum)]
 return $ first ++ second ++ third

evolveRNet :: [Network] -> IO [Network]
evolveRNet nets = do
 first <- takeNRBest nets bestNum
 second <- mapM (`perturbNetwork` modP) first
 third <- mapM (\x -> makeRandomNetwork netSize) [1..(netNum - 2*bestNum)]
 return $ first ++ second ++ third
--main = testRender

trainNet :: IO Network
trainNet = do
 first <- mapM (\x -> makeRandomNetwork netSize) [1..netNum]
 x <- foldr (.) id (replicate iterNum (>>= evolveNet)) (return first)
 return (head x)


trainRNet :: IO Network
trainRNet = do
 first <- mapM (\x -> makeRandomNetwork netSize) [1..netNum]
 x <- foldr (.) id (replicate iterNum (>>= evolveRNet)) (return first)
 return (head x)


--randomNetwork :: [Int] -> Float -> Network -- create random network with levels of size of the first argument and with absolute value of weights of the order of the second argument


