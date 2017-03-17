module StepUtil where
import CodeWorld
import DataTypes
import Shapes


atCoord :: Coord -> Picture -> Picture
atCoord (C x y) p = translated (fromIntegral x) (fromIntegral y) p

composePictures :: [Picture] -> Picture
composePictures ps = foldl (&) blank ps

isOk :: Tile -> Bool
isOk  Ground = True
isOk  Storage = True
isOk  _ = False

drawTile :: Tile -> Picture
drawTile Wall    = wall
drawTile Ground  = ground
drawTile Storage = storage
drawTile Box     = box
drawTile Blank   = background

isStorage :: Tile -> Bool
isStorage Storage = True
isStorage _ = False

stepUp :: Coord -> Coord
stepUp (C x y) = (C x (y+1))

stepDown :: Coord -> Coord
stepDown (C x y) = (C x (y-1))

stepLeft :: Coord -> Coord
stepLeft (C x y) = (C (x-1) y)

stepRight :: Coord -> Coord
stepRight (C x y) = (C (x+1) y)

isBox :: Coord -> [Coord] -> Bool
isBox p (c:cs) = if (eqCoords p c) then True else isBox p cs
isBox p [] = False

eqCoords :: Coord -> Coord -> Bool
eqCoords (C x y) (C x1 y1) = x == x1 && y == y1
