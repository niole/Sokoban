import CodeWorld

data Tile = Wall | Ground | Storage | Box | Blank

drawTile :: Tile -> Picture
drawTile Wall = wall
drawTile Ground = ground
drawTile Storage = storage
drawTile Box = box
drawTile Blank = background

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

maze :: Integer -> Integer -> Tile
maze x y
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

positionBlock :: (Integer, Integer) -> Picture
positionBlock (x, y) = translated (fromIntegral x) (fromIntegral y) (drawTile( maze x y ))

completeCoord :: Integer -> Integer -> (Integer, Integer)
completeCoord x y = (x, y)

getYs :: Integer -> [(Integer, Integer)]
getYs x = map (completeCoord x) [-10..10]

getAllCoords :: [(Integer, Integer)]
getAllCoords = [-10..10] >>= getYs

pictureOfMaze :: Picture
pictureOfMaze = foldl (&) blank ( map positionBlock getAllCoords )

main :: IO()
main = drawingOf pictureOfMaze
