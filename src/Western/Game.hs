module Western.Game (
  turn, 
  testMap,
  fov,
  line, 
  Map(..),
  GameState(..),
  Outcome(..),
  Turn(..)  )
where

import Data.List
import Data.Array
import Data.Set (Set,member,fromList)

import Control.Monad



type Map = Array (Int, Int) Char

-- | Each player is characterized by four bits of information
-- x- and y-coordinate, number of bullets
-- and a bool saying if he is about to fire 
type GameState = ((Int, Int, Int, Bool), (Int, Int, Int, Bool))

-- | Game outcome
data Outcome = Player1Won | Player2Won deriving (Show, Eq)

-- | Choices a player can make when taking a turn
-- Left, Right, Up, Down, Skip, Fire
data Turn = L | R | U | D | S | Fire deriving (Show, Eq)

-- | Transform the output of "turn" so as to make the returned value
-- from the perspective of the first player into the returned value
-- from the perspective of the second player
mirror :: Maybe (Either GameState Outcome) -> Maybe (Either GameState Outcome)
mirror (Just (Left ((x1,y1,b1,s1), (x2,y2,b2,s2)))) = (Just (Left ((x2,y2,b2,s2), (x1,y1,b1,s1))))
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
-- if one ends up outside the map or hits an obstacl, or hits
-- another player, then return False, otherwise True
-- @map@ - the map
-- @(dx,dy)@ - the vector
-- @state@ - game state
boundaryCheck :: Map -> (Int,Int) -> GameState -> Maybe GameState
boundaryCheck map (dx,dy) state =
  let
    ((x1,y1,b1,s1),(x2,y2,b2,s2)) = state
    state' = ((x1,y1,b1,False),(x2,y2,b2,False)) 
    ((_,_),(w,h)) = bounds map
    move x1' y1' 
      | (x1' < 1) = Just state'
      | (x1' > w) = Just state'
      | (y1' < 1) = Just state'
      | (y1' > h) = Just state'
      | (x1' == x2) && (y1' == y2 ) = Just state'
      | map ! (x1',y1') == 'x' = Just state'
      | otherwise = Just ( ((x1', y1',b1,False), (x2,y2,b2,False)))
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
shootState ((x, y, b1, z), (a, b, b2, c)) = ((x, y, b1-1, True), (a, b, b2, c))  

playAsFirstPlayer :: Turn -> Map -> GameState  -> Maybe (Either GameState Outcome)
playAsFirstPlayer t map st =
  do 
    let ((x1,y1,b1,s1),(x2,y2,b2,s2)) = st
    let (dx,dy,s) = case t of
                      L -> (-1,0,False)
                      R -> (1,0,False)
                      D -> (0,1,False)
                      U -> (0,-1,False)
                      S -> (0,0,False)
                      Fire -> (0,0,(b1 > 0))
    st' <- boundaryCheck map (dx,dy) st
    let ((x1',y1',_,_),(_,_,_,_)) = st'
    let shoot
          | s2 && (canShoot map (x1',y1') (x2,y2)) = Right Player2Won
          | s == True = Left (shootState st)
          | otherwise = Left st'
      in return $ shoot 

fov :: Map -> GameState -> Set (Int,Int)
fov map ((x1,y1,_,s1), (x2,y2,_,s2)) =
  fromList [ (x,y) | x <- [1..w], y <- [1..h], (canShoot map (x,y) (x1,y1)) || (canShoot map (x,y) (x2,y2)) ]
  where ((_,_),(w,h)) = bounds map


--- test data

testMap :: Map
testMap = listArray ((1,1),(7,7)) $
  concat $ transpose [
    "......."
  , ".xx.xx."
  , ".xx.xx."
  , "......."
  , ".xx.xx."
  , ".xx.xx."
  , "......."]  

testState1 = ((1,1,2,False), (5,5,2,False)) :: GameState
testState2 = ((4,4,1,False), (5,5,2,False)) :: GameState
testState3 = ((4,1,1,True), (6,3,1,False)) :: GameState

--- test functions


-- |  Print out the dungeon with players
-- @map@ - the map
-- @state@ - game state
printMap :: Map -> GameState -> IO ()
printMap map ((x1,y1,_,_),(x2,y2,_,_)) =
  let map' = (map // [((x1,y1), '1'),((x2,y2),'2')])
      ((_,_),(w,h)) = bounds map'
      printLine _ y = putStrLn (foldl (\l x-> l ++ [map'!(x,y)]) [] [1..w]) 
  in foldM printLine () [1..h] 
