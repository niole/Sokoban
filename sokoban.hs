import CodeWorld

drawTile :: Integer -> Picture
drawTile n
  | n == 0    = background
  | n == 1    = wall
  | n == 2    = ground
  | n == 3    = storage
  | otherwise = box

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

maze :: Integer -> Integer -> Integer
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2

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
