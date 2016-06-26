module Main where

import Data.Matrix
import Data.Array
import Data.Default(def)
import Control.Concurrent

import Graphics.Vty
import Graphics.Vty.Image
import Graphics.Vty.Attributes


import Western.Network
import Western.Types
import Western.Game

drawBattle :: Network -> Network -> Int -> IO ()
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


trainNetP :: String -> Vty -> IO ()
trainNetP netname vty = do
 first <- mapM (\x -> makeRandomNetwork netSize) [1..netNum]
 (x, _) <- (foldr (.) id (replicate iterNum (evolve))) (return (first, 1))
 writeNetToFile (head x) netname
  where 
   evolve pair = do
    (s, n) <- pair
    s' <- evolveNet s
    update vty $ picForImage $ string (defAttr ` withForeColor ` green) (show n ++ "/" ++ show iterNum ++ " tournaments played for " ++ netname ++ ".")
    return (s', n + 1)


drawRBattle :: Network -> Network -> Int -> IO ()
drawRBattle x y n = do
 game <- playRNTurns x y n
 let pics = map (\x -> renderAnyGame testMap (fst x)) $ game
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

main :: IO ()
main = do
 vty <- mkVty def
 x <- trainNetP "net1" vty  
 evt <- nextEvent vty
 shutdown vty

-- main = do
--  x <- trainNet
--  y <- trainNet
--  writeNetToFile x "net1"
--  writeNetToFile y "net2"

--randomNetwork :: [Int] -> Float -> Network -- create random network with levels of size of the first argument and with absolute value of weights of the order of the second argument


