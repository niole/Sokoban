{-# LANGUAGE OverloadedStrings #-}
import CodeWorld

{--
rules:
        player pushes boxes into storage locations
        player wins when all boxes are stored
        can push box through storage location to other storage location
        cannot push box through other boxes or boxes in storage locations
--}

data Tile = Wall | Ground | Storage | Box | Blank
data Direction = U | D | L | R
data Coord = C Integer Integer

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = background

background :: Picture
background = colored white (solidRectangle 1.0 1.0)

wall :: Picture
wall = colored (gray 0.5) (solidRectangle 1.0 1.0)

ground :: Picture
ground = colored yellow (solidRectangle 1.0 1.0)

storage :: Picture
storage = colored black (solidCircle 0.25 ) & ground

box :: Picture
box = colored brown (solidRectangle 1.0 1.0)

player :: Picture
player = colored red (solidCircle 0.25 ) & ground

maze :: Integer -> Integer -> Tile
maze x y
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

positionBlock :: Coord -> Picture
positionBlock (C x y) = translated (fromIntegral x) (fromIntegral y) (drawTile( maze x y ))

completeCoord :: Integer -> Integer -> Coord
completeCoord x y = C x y

getYs :: Integer -> [Coord]
getYs x = map (completeCoord x) [-10..10]

getAllCoords :: [Coord]
getAllCoords = [-10..10] >>= getYs

pictureOfMaze :: Picture
pictureOfMaze = foldl (&) blank ( map positionBlock getAllCoords )

initialCoord :: Coord
initialCoord = C 0 (-1)

step :: Direction -> Coord -> Coord
step U (C x y) = C x (y+1)
step D (C x y) = C x (y-1)
step L (C x y) = C (x-1) y
step R (C x y) = C (x+1) y
step _ c = c

handleEvent :: Event -> Coord -> Coord
handleEvent (KeyPress key) c
    | key == "Right" = step R c
    | key == "Up"    = step U c
    | key == "Left"  = step L c
    | key == "Down"  = step D c
handleEvent _ c      = c

handleTime :: Double -> Coord -> Coord
handleTime _ c = c

drawGame :: Coord -> Picture
drawGame c = composePictures [(atCoord c player), pictureOfMaze ]

initialBoxes :: [Coord] --finds coords of boxes in maze
initialBoxes = filter (\(C x y) -> isBox (maze x y)) getAllCoords

isBox :: Tile -> Bool
isBox Box = True
isBox _ = False

canMove :: Tile -> Bool
canMove Ground = True
canMove Storage = True
canMove _ = False

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) p = translated (fromIntegral x) (fromIntegral y) p

composePictures :: [Picture] -> Picture
composePictures ps = foldl (&) blank ps

main :: IO()
main = interactionOf initialCoord handleTime handleEvent drawGame
--main = print (initialBoxes)
