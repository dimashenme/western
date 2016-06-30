module Western.Network where


import Data.Matrix hiding ((!))
import Data.Array
import Data.List hiding (transpose)

import Control.Monad
import Control.Monad.Random  hiding (fromList)
import Control.Concurrent

import Western.Game



-- | Turn number after which a training game is terminated
timeout = 100

-- | Sizes of layers of networks
netSize = [8, 7, 6]

-- | Number of networks in a training tournament
netNum = 50

-- | Number of iterations for network training
iterNum = 100

bestNum = 23
modP = 0.02
thr = 0.6

-- | Initial number of bullets
bulNum = 2 :: Int



data Network = Network {  ntWeights :: [Matrix Float] }


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

netToString :: Network -> String
netToString n = matrices $ ntWeights n
 where 
 matrices [] = ""
 matrices (x:xs) = getWord (toList x) ++ "\n" ++ matrices xs
 getWord s = unwords $ map show s

-- | Generate two distinct random coordinates for inital position of players
-- the coordinates are those of an empty square
randomCoords :: (RandomGen g) => Map -> Rand g ((Int, Int, Int), (Int, Int, Int))
randomCoords m = do
  let ((_,_),(w,h)) = bounds m
  b2 <- getRandomR (0, bulNum) 
  b1 <- getRandomR (0, bulNum)

  let emptySquares = filter (\(i,j) -> m ! (i,j) /= 'x') (indices m)

  (x1, y1) <- uniform emptySquares
  (x2, y2) <- uniform (delete (x1,y1) emptySquares)

  return  ((x1, y1, b1), (x2, y2, b2))

-- | Make two networks play on a given map
-- @r@ is a flag telling if the initial position should be random or standard
-- returns the score of both players as a tuple
playGame :: (RandomGen g) => Bool -> Map -> Network -> Network -> Rand g (Int, Int)  
playGame random m p1 p2 = do  
  let ((_,_),(w,h)) = bounds m
  ((x1, y1, b1), (x2, y2, b2)) <- if random
                                 then return ((1, 1, bulNum), (w, h, bulNum))
                                 else randomCoords testMap
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

-- | Make two networks play @n@ turns on a given map 
-- @r@ is a flag telling if the initial position should be random or standard
-- returns the final state
playNTurns :: (RandomGen g) => Bool -> Map -> Network -> Network -> Int -> Rand g [(Maybe (Either GameState Outcome), Int)]
playNTurns random m p1 p2 n = do 
  let ((_,_),(w,h)) = bounds m
  ((x1, y1, b1), (x2, y2, b2)) <- if random
                                  then return ((1, 1, bulNum), (w, h, bulNum))
                                  else randomCoords testMap
  return $ gameGoesOn (take n $ iterate makeTurn (Just (Left ((1, 1, bulNum, False), (w, h, bulNum, False))), 0)) 
  where
    makeTurn (Just (Right x), m ) = (Just (Right x), m + 1 )
    makeTurn (Just (Left x), m) 
      | even m = (turn (Left (decideTurn $ evalNetwork p1 $ makeInput x)) testMap x, m + 1)
      | otherwise = (turn (Right (decideTurn $ evalNetwork p2 $ makeInput (snd x, fst x))) testMap x, m + 1)
    gameGoesOn [] = []
    gameGoesOn ((Just (Left x), n) : xs) = (Just (Left x), n) : gameGoesOn xs
    gameGoesOn ((Just (Right x), n) : xs) = [(Just (Right x), n)]

-- | Make a bunch networks play a series of games on a given map
-- against each other
-- @r@ is a flag telling if the initial position should be random or standard
-- returns the score of each network
playTournament :: (RandomGen g) => Bool -> Map -> [Network] -> Rand g [Int]  
playTournament random m s = do
 table <- sequence $ matrix l l (\(i, j) -> playGame random m (s !! (i-1)) (s !! (j-1)))
 return $ map (\x -> (sum $ fmap fst (getRow x table)) + (sum $ fmap snd (getCol x table))) [1..l]
  where 
  l = length s

takeNBest :: (RandomGen g) => Bool -> Map ->  [Network] -> Int -> Rand g [Network]
takeNBest random m s n = do
 results <- playTournament random m s
 return $ take n $ map fst (sortOn ((*(-1)).snd) $ zip s results)

makeRandomMatrix :: (RandomGen g) => (Int,Int) -> Float -> Rand g (Matrix Float)
makeRandomMatrix (i, j) magnitude = do
 rnumbers <- getRandomRs (-magnitude, magnitude) 
 return (fromList i j (take (i*j) rnumbers))

makeRandomMatrices :: (RandomGen g) => [Int] -> Float -> Rand g [Matrix Float]
makeRandomMatrices s magnitude = do
 let dimlist = zip s $ tail s
 mapM (`makeRandomMatrix` magnitude) dimlist
 
makeRandomNetwork :: (RandomGen g) => [Int] -> Rand g Network
makeRandomNetwork s = do
 matlist <- makeRandomMatrices s 1
 return Network {ntWeights = matlist}

perturbNetwork :: (RandomGen g) => Network -> Float -> Rand g Network 
perturbNetwork n magnitude = do
 let a = ntWeights n
 b <- makeRandomMatrices (ntLevelSize n) magnitude
 return Network {ntWeights = zipWith (elementwise (+)) a b}

evolveNet :: (RandomGen g) => Bool -> Map -> [Network] -> Rand g [Network]
evolveNet random m nets = do
 first <- takeNBest random m nets bestNum
 second <- mapM (`perturbNetwork` modP) first
 third <- mapM (\x -> makeRandomNetwork netSize) [1..(netNum - 2*bestNum)]
 return $ first ++ second ++ third

trainNet :: (RandomGen g) => Bool -> Map -> Rand g Network
trainNet random m = do
 first <- mapM (\x -> makeRandomNetwork netSize) [1..netNum]
 x <- foldr (.) id (replicate iterNum (>>= evolveNet random m)) (return first)
 return (head x)

