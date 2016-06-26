module Western.Game (
  turn, 
  renderGame,
  testRender,
  testMap,
  Map(..),
  GameState(..),
  Outcome(..),
  Turn(..),
  renderAnyGame )
where

import Data.List
import Data.Array
import Data.Set (Set,member,fromList)

import Control.Monad

import Graphics.Vty
import Graphics.Vty.Image
import Graphics.Vty.Attributes
import Data.Default(def)

type Map = Array (Int, Int) Char

-- | Each player is characterized by three bits of information
-- x- and y-coordinate and a bool saying if he is about to fire
type GameState = ((Int, Int, Bool), (Int, Int, Bool))

-- | Game outcome
data Outcome = Player1Won | Player2Won deriving (Show, Eq)

-- | Choices a player can make when taking a turn               
data Turn = L | R | U | D | S | Fire deriving (Show, Eq)


--- test data

testMap :: Map
testMap = listArray ((1,1),(6,5)) $
  concat $ transpose [
    "......"
  , "..x..."
  , ".xx.x."
  , "......"
  , "......" ]  

testState1 = ((1,1,False), (5,5,False)) :: GameState
testState2 = ((4,4,False), (5,5,False)) :: GameState
testState3 = ((4,1,True), (6,3,False)) :: GameState

------


-- |  Print out the dungeon with players
-- @map@ - the map
-- @state@ - game state
printMap :: Map -> GameState -> IO ()
printMap map ((x1,y1,_),(x2,y2,_)) =
  let map' = (map // [((x1,y1), '1'),((x2,y2),'2')])
      ((_,_),(w,h)) = bounds map'
      printLine _ y = putStrLn (foldl (\l x-> l ++ [map'!(x,y)]) [] [1..w]) 
  in foldM printLine () [1..h] 


-- | Transform the output of "turn" so as to make the returned value
-- from the perspective of the first player into the returned value
-- from the perspective of the second player
mirror :: Maybe (Either GameState Outcome) -> Maybe (Either GameState Outcome)
mirror (Just (Left ((x1,y1,s1), (x2,y2,s2)))) = (Just (Left ((x2,y2,s2), (x1,y1,s1))))
mirror (Just (Right Player1Won)) = (Just (Right Player2Won))
mirror (Just (Right Player2Won)) = (Just (Right Player1Won))
mirror Nothing = Nothing

-- | Return the game state after making a turn
-- @turn@ - Left turn is 1st player's turn, Right turn is 2nd player's turn
-- @map@ - the map
-- @state@ - input state
-- returns Just Left state if the state is updated, Just Right outcome if
-- the game is over,  Nothing if the move is invalid (hitting an obstacle)
turn :: Either Turn Turn -> Map -> GameState  -> Maybe (Either GameState Outcome)
turn tt map state =
  case tt of
    Left t ->  playAsFirstPlayer t map state
    Right t -> mirror $ playAsFirstPlayer t map (snd state, fst state)

-- | check if it is possible to move along the vector dx,dy
-- if one ends up outside the map then return False, otherwise True
-- todo: check for obstacles, and hitting the other player
-- @map@ - the map
-- @(dx,dy)@ - the vector
-- @state@ - game state
boundaryCheck :: Map -> (Int,Int) -> GameState -> Maybe GameState
boundaryCheck map (dx,dy) state =
  let
    ((x1,y1,s1),(x2,y2,s2)) = state
    state' = ((x1,y1,False),(x2,y2,False)) 
    ((_,_),(w,h)) = bounds map
    move x1' y1' 
      | (x1' < 1) = Just state'
      | (x1' > w) = Just state'
      | (y1' < 1) = Just state'
      | (y1' > h) = Just state'
      | (x1' == x2) && (y1' == y2 ) = Just state'
      | map ! (x1',y1') == 'x' = Just state'
      | otherwise = Just ( ((x1', y1',False), (x2,y2,False)))
  in  move (x1+dx) (y1+dy) 

--- Brezenheim naive

fromInt :: (Num a, Integral b) => (b,b) -> (a, a)
fromInt (x,y) = (fromIntegral x, fromIntegral y) 

(|.|) :: Num a => (a, a) -> (a, a) -> a 
(x, y) |.| (a , b) = x*a + y*b

(|+|) :: Num a => (a, a) -> (a, a) -> (a, a)
(x, y) |+| (a , b) = (x + a, y + b)

(|-|) :: Num a => (a, a) -> (a, a) -> (a, a)
(x, y) |-| (a , b) = (x - a, y - b)

distance :: Floating a => (a, a) -> (a, a) -> a
distance (x1, y1) (x2, y2) = sqrt $ ((x2 - x1)^2 + (y2 - y1)^2)

(|*|) :: Num a => a -> (a, a) -> (a, a)
a |*| (x, y) = (a*x, a*y)

interval :: Int -> Int -> [Int]
interval x y 
  | x < y = [x..y]
  | otherwise = [y..x]

-- | Return the list of coordinates of squares that
-- are crossed by the line between two points
line :: (Int,Int) -> (Int,Int) -> [(Int,Int)] 
line (x1,y1) (x2,y2) =  [(x1,y1)] ++
  [ (x, y) | x <- interval (x1) (x2),
    y <- interval (y1) (y2),
    ((x /= x1) || (y /= y1)) &&
    (distance (fromInt (x,y)) (projection (x,y))) < 1/2 ]
  where
    projection v =((cosine v) * (dv v) / dline ) |*| fromInt((x2-x1,y2-y1)) |+| fromInt (x1,y1)
    sine v = sqrt $ 1 - (cosine v)^2
    cosine (x,y) = fromIntegral((x-x1,y-y1) |.| (x2-x1,y2-y1))/(s (x,y))
    s v = (dv v) * dline 
    dv (x,y) = (distance (fromInt((x-x1,y-y1))) (0, 0))
    dline = (distance (fromInt(x2-x1,y2-y1)) (0, 0))

-- | try to shoot from (x1,y1) to (x2,y2)
-- if a line between these two points can be drawn so that it
-- doesn't hit any obstacle, return True, otherwise False
-- @(x1,y1)@ - first point
-- @(x2,y2)@ - second point
canShoot :: Map -> (Int,Int) -> (Int,Int) -> Bool
canShoot map p1 p2 =
  foldr (\(x,y) o -> (map ! (x,y) /= 'x') && o) True (line p1 p2)

shootState :: GameState -> GameState
shootState ((x, y, z), (a, b, c)) = ((x, y, True), (a, b, c))  

playAsFirstPlayer :: Turn -> Map -> GameState  -> Maybe (Either GameState Outcome)
playAsFirstPlayer t map st =
  do 
    let (dx,dy,s) = case t of
                      L -> (-1,0,False)
                      R -> (1,0,False)
                      D -> (0,1,False)
                      U -> (0,-1,False)
                      S -> (0,0,False)
                      Fire -> (0,0,True)
    st' <- boundaryCheck map (dx,dy) st
    let ((x1,y1,s1),(x2,y2,s2)) = st
    let ((x1',y1',_),(_,_,_)) = st'
    let shoot
          | s2 && (canShoot map (x1',y1') (x2,y2)) = Right Player2Won
          | s == True = Left (shootState st)
          | otherwise = Left st'
      in return $ shoot 

fov :: Map -> GameState -> Set (Int,Int)
fov map ((x1,y1,s1), (x2,y2,s2)) =
  fromList [ (x,y) | x <- [1..w], y <- [1..h], (canShoot map (x,y) (x1,y1)) || (canShoot map (x,y) (x2,y2)) ]
  where ((_,_),(w,h)) = bounds map

renderPlayers :: Map -> GameState -> Image
renderPlayers map state =
  let
    ((x1,y1,s1),(x2,y2,s2)) = state
    ((_,_),(w,h)) = bounds map    
    renderLine y = foldl (\l x -> l <|> (visibleChar x y)) emptyImage [1..w]
    attrFov = Attr{attrForeColor = SetTo blue, attrStyle = Default, attrBackColor = Default}
    attrPlayer False = Attr{attrForeColor = SetTo green, attrStyle = Default, attrBackColor = Default}
    attrPlayer True = Attr{attrForeColor = SetTo red, attrStyle = Default, attrBackColor = Default}

    visibleChar x y 
      | (x == x1) && (y == y1) = char (attrPlayer s1) '1'
      | (x == x2) && (y == y2) = char (attrPlayer s2) '2'
      | (member (x,y) (fov map state)) = char attrFov (map!(x,y)) 
      | otherwise = backgroundFill 1 1 
  in   vertCat (fmap renderLine [1..h])
  

renderGame :: Int -> Map -> GameState -> Picture
renderGame turn map state =
  picForLayers [renderPlayers',renderMap]
  where
    renderPlayers' = renderPlayers map state
    renderMap =  vertCat (fmap (string defAttr . renderLine)  [1..h])
    ((x1,y1,s1),(x2,y2,s2)) = state
    ((_,_),(w,h)) = bounds map
    renderLine  y = foldl (\l x-> l ++ [map!(x,y)]) [] [1..w]

renderVictory :: Outcome -> Picture
renderVictory (Player1Won) = picForImage $ string (defAttr ` withForeColor ` green) "Player 1 won"
renderVictory (Player2Won) = picForImage $ string (defAttr ` withForeColor ` green) "Player 2 won"

renderAnyGame :: Map -> Maybe (Either GameState Outcome) -> Picture
renderAnyGame map (Just (Left x)) = renderGame 1 map x
renderAnyGame map (Just (Right x)) = renderVictory x
renderAnyGame map Nothing = picForImage $ string (defAttr ` withForeColor ` red) "Error"

testRender = do
  vty <- mkVty def
  let pic = renderGame 1 testMap ((2,2,False),(6,4,False))
  update vty pic
  evt <- nextEvent vty  
  shutdown vty
 
