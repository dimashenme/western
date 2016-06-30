module Western.Render where

import Data.Array
import Data.Set (Set,member,fromList)

import Graphics.Vty
import Graphics.Vty.Image
import Graphics.Vty.Attributes
import Data.Default(def)

import Western.Game

renderPlayers :: Map -> GameState -> Image
renderPlayers map state =
  let
    ((x1,y1,b1,s1),(x2,y2,b2,s2)) = state
    ((_,_),(w,h)) = bounds map    
    renderLine y = foldl (\l x -> l <|> (visibleChar x y)) emptyImage [1..w]
    attrFov = Attr{attrForeColor = SetTo blue, attrStyle = Default, attrBackColor = Default}
    attrPlayer1 False = Attr{attrForeColor = SetTo green, attrStyle = Default, attrBackColor = Default}
    attrPlayer1 True = Attr{attrForeColor = SetTo red, attrStyle = Default, attrBackColor = Default}
    attrPlayer2 False = Attr{attrForeColor = SetTo green, attrStyle = Default, attrBackColor = Default}
    attrPlayer2 True = Attr{attrForeColor = SetTo red, attrStyle = Default, attrBackColor = Default}

    visibleChar x y 
      | (x == x1) && (y == y1) = char (attrPlayer1 s1) (head $ show b1)
      | (x == x2) && (y == y2) = char (attrPlayer2 s2) (head $ show b2)
      | (member (x,y) (fov map state)) = char attrFov (map!(x,y)) 
      | otherwise = backgroundFill 1 1 
  in   vertCat (fmap renderLine [1..h])
  
renderRunningGame :: Int -> Map -> GameState -> Picture
renderRunningGame turn map state =
  picForLayers [renderPlayers',renderMap]
  where
    renderPlayers' = renderPlayers map state
    renderMap =  vertCat (fmap (string defAttr . renderLine)  [1..h])
    ((x1,y1,b1,s1),(x2,y2,b2,s2)) = state
    ((_,_),(w,h)) = bounds map
    renderLine  y = foldl (\l x-> l ++ [map!(x,y)]) [] [1..w]

renderVictory :: Outcome -> Picture
renderVictory (Player1Won) = picForImage $ string (defAttr ` withForeColor ` green) "Player 1 won"
renderVictory (Player2Won) = picForImage $ string (defAttr ` withForeColor ` green) "Player 2 won"

renderGame :: Map -> Maybe (Either GameState Outcome) -> Picture
renderGame map (Just (Left x)) = renderRunningGame 1 map x
renderGame map (Just (Right x)) = renderVictory x
renderGame map Nothing = picForImage $  string (defAttr `withForeColor` red) "Error"


testRender :: IO ()
testRender = do
  vty <- mkVty def
  let pic = renderRunningGame 1 testMap ((2,2,2,False),(6,4,2,False))
  update vty pic
  evt <- nextEvent vty  
  shutdown vty
 

