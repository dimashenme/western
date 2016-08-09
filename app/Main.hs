{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Matrix 
import Data.Array
import Data.Default(def)

import Control.Concurrent
import Control.Concurrent.STM.TChan
import Control.Monad.STM

import Control.Monad
import Control.Monad.State.Lazy
import Control.Monad.Random

import Control.Monad.Trans.Maybe

import System.IO

import Graphics.Vty
import Graphics.Vty.Image
import Graphics.Vty.Picture
import Graphics.Vty.Attributes

import Western.Network
import Western.Game
import Western.Render

import Western.Training as T


-- show progress
showProgress :: (RandomGen g) =>
     Int                    -- ^ current tournament
  -> Int                    -- ^ overall # of tournaments
  -> String                 -- ^ network name 
  -> Vty                    -- ^ vty to use
  -> Protocol g (Maybe ())
showProgress n nmax netname vty = liftIO $ do
  let evChan = _eventChannel $ inputIface vty
  let progress = string (defAttr `withForeColor` green) $
        (show n ++ "/"
          ++ show nmax
          ++ " tournaments played for "
          ++ netname ++ ".")
  let esc = string (defAttr `withForeColor` red) $
        "Press `Esc` to quit"
  update vty $ picForImage $ progress `vertJoin` esc
  nokeypress <-  atomically $ isEmptyTChan evChan
  if nokeypress
    then return $  Just ()
    else
    do
      evt <- nextEvent vty
      case evt of
        EvKey KEsc [] -> return Nothing
        _ -> return $ Just ()


train :: (RandomGen g) =>
     Vty
  -> String
  -> Protocol g ()
train vty netname = do
  populate 20  
  result <- runMaybeT $ forM_ [1..100] $ \n -> do
    lift tournament 
    MaybeT $ showProgress n 100 netname vty
  case result of
    Just _ -> do
      liftIO $ putStrLn "\ntrained. Press a key"
      nws <- getNetworks
      write (head nws) netname
      liftIO $ do
        nextEvent vty
        return ()
    Nothing -> liftIO $ do
      putStrLn "\naborted. Press a key"
      nextEvent vty
      return ()

fight  :: (RandomGen g) =>
     Vty
  -> String
  -> String
  -> Protocol g ()
fight vty net1 net2 = undefined
  

drawBattle :: Bool -> Map -> Network -> Network -> Int -> IO ()
drawBattle r m n1 n2 n = do
  !game <- evalRandIO $ playNTurns r m n1 n2 n
  let pics = map (renderGame testMap . fst) game
  vty <- mkVty def
  mapM_ (drawAndWait vty) (zip pics [1..n])
  evt <- nextEvent vty
  shutdown vty
  where 
    drawAndWait vty (!pic,n) = do
      update vty $ addToBottom pic $ translateY 11 $ string defAttr $ "turn number " ++ show n
      threadDelay 300000

main = do
  vty <- mkVty def
  runProtocol (train vty "foo") testMap
  shutdown vty


-- main :: IO ()
-- main = do
--  n1 <- readNetFromFile netSize "net1"
--  n2 <- readNetFromFile netSize "net2"

--  vty <- mkVty def
--  drawBattle True testMap n1 n2 20
--  nextEvent vty
--  shutdown vty

