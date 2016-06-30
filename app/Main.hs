{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Matrix 
import Data.Array
import Data.Default(def)

import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad.STM

import Control.Monad
import Control.Monad.Random

import System.IO

import Graphics.Vty
import Graphics.Vty.Image
import Graphics.Vty.Attributes


import Western.Network
import Western.Game


-- | trains a network and shows progress on
-- the given vty. The training can be interrupted
-- with an `esc` keypress
-- @r@ is a flag of whether to use a random starting position
-- @m@ is the map to use
trainNetAndShowProgress :: Bool -> Map -> String -> Vty -> IO (Maybe Network)
trainNetAndShowProgress r m netname vty = do
 -- initial pool of random networks
 first <- mapM (\_ -> evalRandIO $ makeRandomNetwork netSize) [1..netNum]
 x <- foldM evolveAndShowProgress (Just first) [1..iterNum]
 return $ liftM head x
  where
   evChan = _eventChannel $ inputIface vty
   evolveAndShowProgress :: Maybe [Network]  -> Int -> IO (Maybe [Network])
   evolveAndShowProgress (Just !s) n = do
    s' <- evalRandIO $ evolveNet True m s
    let progress = string (defAttr `withForeColor` green) (show n ++ "/"
                                                             ++ show iterNum
                                                             ++ " tournaments played for "
                                                             ++ netname ++ ".")        
    let esc = string (defAttr `withForeColor` red) "Press `Esc` to quit"
    update vty $ picForImage $ progress `vertJoin` esc
    nokeypress <-  atomically $ isEmptyTChan evChan
    if nokeypress
      then return $  Just s'
      else
      do
        evt <- nextEvent vty
        case evt of
          EvKey KEsc [] -> return Nothing
          _ -> return $ Just s'
   evolveAndShowProgress Nothing _ = return Nothing

drawBattle :: Bool -> Map -> Network -> Network -> Int -> IO ()
drawBattle r m x y n = do
  game <- evalRandIO $ playNTurns r m x y n
  let pics = map (renderGame testMap . fst)  game
  vty <- mkVty def
  mapM_ (drawAndWait vty) pics
  evt <- nextEvent vty
  shutdown vty
  where 
    drawAndWait scr pic = do
      update scr pic
      threadDelay 100000

writeNetToFile :: Network -> String -> IO ()
writeNetToFile net filename = 
 writeFile filename (netToString net)

readNetFromFile :: [Int] -> String -> IO Network
readNetFromFile sizes filename = do
 mats <- readFile filename
 let nums = map stringsToFloats (lines mats)
 return Network {ntWeights = zipWith fromListMy (zip sizes $ tail sizes) nums}
 where 
  stringsToFloats s = map ( \x -> read x :: Float ) (words s) 
  fromListMy (x,y) s = Data.Matrix.fromList x y s 

main :: IO ()
main = do
  vty <- mkVty def
  nw <- trainNetAndShowProgress True testMap "net1" vty

  case nw of
    Just network -> do
      writeNetToFile network "net1"
      nextEvent vty
      return ()
    Nothing -> return ()
  shutdown vty

-- main :: IO ()
-- main = do
--  n1 <- readNetFromFile netSize "net1"
--  n2 <- readNetFromFile netSize "net2"

--  drawBattle True testMap n1 n2 100
