import Test.QuickCheck

import Western.Network
import Western.Game

prop_lineEnds :: (Int, Int) -> (Int, Int) -> Bool 
prop_lineEnds (x1,y1) (x2,y2) =
  let l = line (x1,y1) (x2,y2)
  in
    elem (x1, y1) l && elem (x2, y2) l

main :: IO ()
main = do
  quickCheck prop_lineEnds
  
