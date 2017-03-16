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
data State = State Coord Direction
data SSState world = StartScreen | Running world --polymorphic in terms of world
data Interaction world = Interaction
        world
        (Double -> world -> world)
        (Event -> world -> world)
        (world -> Picture)


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
pictureOfMaze = composePictures ( drawMazeElements getAllCoords )

initialState :: State --hardcoded
initialState = State (C 0 (-1)) D

step :: Direction -> State -> State
step U (State (C x y) d)  = getValidStep U (State (C x y) d)  (C x (y+1))
step D (State (C x y) d) = getValidStep D (State (C x y) d)  (C x (y-1))
step L (State (C x y) d) = getValidStep L (State (C x y) d)  (C (x-1) y)
step R (State (C x y) d) = getValidStep R (State (C x y) d)  (C (x+1) y)

getValidStep :: Direction -> State -> Coord -> State
getValidStep direction oldState (C x y)
        | isOk (maze x y) = State (C x y) direction
        | otherwise = oldState

isOk :: Tile -> Bool
isOk Ground = True
isOk Storage = True
isOk _ = False

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) state
    | key == "Right" = step R state
    | key == "Up"    = step U state
    | key == "Left"  = step L state
    | key == "Down"  = step D state
handleEvent _ state      = state

handleTime :: Double -> Coord -> Coord
handleTime _ c = c

drawGameState :: State -> Picture
drawGameState (State c _) = composePictures [(atCoord c player), pictureOfMaze ]

resetable :: Interaction s -> Interaction s
resetable (Interaction state0 step handle draw)
  = Interaction state0 step handle' draw
  where handle' (KeyPress key) _ | key == "Esc" = state0
        handle' e s = handle e s

withStartScreen :: Interaction s -> Interaction (SSState s)
withStartScreen (Interaction state0 step handle draw)
  = Interaction state0' step' handle' draw'
  where
    state0' = StartScreen

    step' _ StartScreen = StartScreen
    step' t (Running s) = Running (step t s)

    handle' (KeyPress key) StartScreen
         | key == " "                  = Running state0
    handle' _              StartScreen = StartScreen
    handle' e              (Running s) = Running (handle e s)

    draw' StartScreen = startScreen
    draw' (Running s) = draw s

startScreen :: Picture
startScreen = scaled 3 3 (text "welcome to Sokoban")

runInteraction :: Interaction s -> IO ()
runInteraction (Interaction state0 step handle draw)
  = interactionOf state0 step handle draw

initialBoxes :: [Coord] --finds coords of boxes in maze
initialBoxes = filter (\(C x y) -> isBox (maze x y)) getAllCoords

isBox :: Tile -> Bool
isBox Box = True
isBox _ = False

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) p = translated (fromIntegral x) (fromIntegral y) p

composePictures :: [Picture] -> Picture
composePictures ps = foldl (&) blank ps

drawMazeElements :: [Coord] -> [Picture]
drawMazeElements cs = map positionBlock cs

{-- draws current game state, on key press, gets updated state
and passes it along as an Interaction --}
exercise2 :: Interaction State
exercise2 = Interaction initialState (\_ c -> c) handleEvent drawGameState

main :: IO()
main = runInteraction (resetable (withStartScreen exercise2))
--main = interactionOf initialState handleTime handleEvent drawGameState
--main = drawingOf (composePictures (drawMazeElements initialBoxes))
