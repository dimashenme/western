{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Module      : Western.Training
Description : Training protocol 
Copyright   : (c) Kostya Tolmachev, Dima Sustretov
License     : BSD3
Maintainer  : Dima Sustretov dmitri83@hcoop.net
Stability   : experimental
Portability : POSIX

Defines the training protocol monad. It allows to set up network
training parameters and describe the neural network training
process. On the implementation level Protocol is a stack of three
monads: IO, StateT and RandT.
-}
module Western.Training where

import Control.Monad

import Control.Monad.State.Lazy
import Control.Monad.State.Class

import Control.Monad.Random  hiding (fromList)
import Control.Monad.Random.Class

import Graphics.Vty

import Data.Matrix 

import Data.Default

import Western.Network hiding (timeout, bestNum, modP, thr)
import Western.Game

data TrainingParams = TrainingParams {
    timeout :: Integer
  , randomStartPos :: Bool  
  , bestNum :: Integer
  , modP :: Rational
  , threshold :: Rational
  } deriving Show

  
instance Default TrainingParams where
  def = TrainingParams {
      timeout = 100
    , randomStartPos = True
    , bestNum = 20
    , modP = 0.02
    , threshold = 0.6
  }

data TrainingState = TrainingState {
    parameters :: TrainingParams
  , gameMap :: Map
  , networks :: [Network]
}

type TrainingOutcome = [Network]


newtype Protocol g a = Protocol
  (RandT g (StateT TrainingState IO) a)
  deriving (Monad,Functor,Applicative,
            MonadRandom,MonadIO, MonadState TrainingState)


setParameters :: (RandomGen g) => TrainingParams -> Protocol g ()
setParameters p = do
  s <- get
  put s { parameters = p}

getParameters :: (RandomGen g) => Protocol g TrainingParams 
getParameters = do
  s <- get
  return (parameters s)

getNetworks :: (RandomGen g) => Protocol g [Network]
getNetworks = do
  s <- get
  return (networks s)

liftToProtocol :: (RandomGen g) => Rand g a -> Protocol g a
liftToProtocol r = Protocol $ liftRandT (return . runRand r)

-- protocol instructions

-- | Make an initial pool of random networks
populate ::  (RandomGen g) =>
     Int            -- ^ number of random networks to generate
  -> Protocol g ()
populate n = do 
  nws <- liftToProtocol $
    forM [1..n] $ const $ makeRandomNetwork netSize
  modify (\s -> s { networks = nws} )

-- | Run a tournament once  
tournament :: (RandomGen g) => Protocol g ()
tournament =  do
  s <- get
  let map = gameMap $ s
      randomPos = randomStartPos $ parameters  $ s
      nws = networks $ s
  nws' <- liftToProtocol $! evolveNet randomPos map $! nws
  put $ s { networks = nws' } 

-- | Read a network from file
read :: (RandomGen g) =>
  [Int] -> String -> Protocol g Network
read sizes filename  = liftIO $
  readNetFromFile sizes filename

-- | Write a network to file
write :: (RandomGen g) => Network -> String -> Protocol g ()
write network filename  = liftIO $
  writeNetToFile network filename
 
-- running

runProtocolWithGen :: (RandomGen g) => g -> Protocol g a -> Map -> IO a
runProtocolWithGen gen (Protocol p) m = do
  let st = evalRandT p gen
  let initialState = TrainingState{ parameters = def, gameMap = m, networks = [] }
  evalStateT st initialState
  
runProtocol :: Protocol StdGen a -> Map -> IO a
runProtocol p m = do
  gen <- getStdGen
  runProtocolWithGen gen p m

  
-- example operators

rrr :: (RandomGen g) => Rand g Int
rrr = getRandom

doRandPure :: (RandomGen g) => Protocol g Int
doRandPure = liftToProtocol rrr

doRand :: (RandomGen g) => Protocol g Int
doRand = do
  rrrs <- getRandomRs (0,20)
  return $ foldr (+) 0 (take 20 rrrs)

doIO :: (RandomGen g) => Protocol g ()
doIO = do
  liftIO $ putStrLn "aoeuaoeu"

doRandIO :: (RandomGen g) => Protocol g ()
doRandIO = do
  rrrs <- getRandomRs (0,20) :: (RandomGen g) => Protocol g [Int]
  forM_ (take 20 rrrs) (liftIO . putStrLn . show)

-- tests

testProtocol = do
  setParameters def { timeout = 50 }
  getParameters
  
test = do
  nw <- runProtocol testProtocol testMap
  putStrLn $ show nw

