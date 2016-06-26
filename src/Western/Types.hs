module Western.Types (
    Network(..)
  )

where

import qualified Data.Map as M
import Data.List
import Data.Matrix


data Network = Network {  ntWeights :: [Matrix Float] }



