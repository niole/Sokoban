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
data State = State (Coord, [Coord]) Direction
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

maze :: Coord -> Tile
maze (C x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage
  | otherwise                = Ground

positionBlock :: (Coord -> Tile) -> Coord -> Picture
positionBlock helper (C x y) = translated (fromIntegral x) (fromIntegral y) (drawTile (helper (C x y)) )

completeCoord :: Integer -> Integer -> Coord
completeCoord x y = C x y

getYs :: Integer -> [Coord]
getYs x = map (completeCoord x) [-10..10]

getAllCoords :: [Coord]
getAllCoords = [-10..10] >>= getYs

pictureOfMaze :: [Coord] -> Picture
pictureOfMaze boxes = composePictures ( drawMazeElements boxes getAllCoords )

{--
        hardcoded
--}
initialState :: State
initialState = State (C 0 (-1), initialBoxes) D

{--
        based on new direction, returns a valid game state
--}
step :: Direction -> State -> State
step U (State (p, bs) d) = getValidStep U (State (p, bs) d) (stepUp p)
step D (State (p, bs) d) = getValidStep D (State (p, bs) d) (stepDown p)
step L (State (p, bs) d) = getValidStep L (State (p, bs) d) (stepLeft p)
step R (State (p, bs) d) = getValidStep R (State (p, bs) d) (stepRight p)

stepUp :: Coord -> Coord
stepUp (C x y) = (C x (y+1))

stepDown :: Coord -> Coord
stepDown (C x y) = (C x (y-1))

stepLeft :: Coord -> Coord
stepLeft (C x y) = (C (x-1) y)

stepRight :: Coord -> Coord
stepRight (C x y) = (C (x+1) y)

{--
        Takes proposed direction, old state and proposed
        new player position and returns a valid state
--}
getValidStep :: Direction -> State -> Coord -> State
getValidStep direction (State (c, cs) d) newCoord
        | isOk (maze newCoord) = (State (newCoord, cs) direction)
        | isBox newCoord cs = getValidBoxMove direction (State (c, cs) d) newCoord
        | otherwise = (State (c, cs) d)

{--
        given a direction, old state, and proposed new
        player position, returns a valid state based on whether
        a user can push the box that matches new player position
--}
getValidBoxMove :: Direction -> State -> Coord -> State
getValidBoxMove U oldState playerPos = checkBoxStep stepUp playerPos oldState U
getValidBoxMove D oldState playerPos = checkBoxStep stepDown playerPos oldState D
getValidBoxMove L oldState playerPos = checkBoxStep stepLeft playerPos oldState L
getValidBoxMove R oldState playerPos = checkBoxStep stepRight playerPos oldState R

{--
        takes step function, new player position and old state and
        returns a state that is valid based on whether
        there is a box or a wall
        otherwise returns old state
--}
checkBoxStep :: (Coord -> Coord) -> Coord -> State -> Direction -> State
checkBoxStep stepper p (State (oldP, bs) oldDir) newDir
        | (isOk (maze (stepper p)) && not (isBox (stepper p) bs)) = State (p, updateBoxState stepper p bs) newDir
        | otherwise = State (oldP, bs) oldDir

updateBoxState :: (Coord -> Coord) -> Coord -> [Coord] -> [Coord]
updateBoxState stepper p bs = map (\b -> if (eqCoords p b) then (stepper b) else b) bs

eqCoords :: Coord -> Coord -> Bool
eqCoords (C x y) (C x1 y1) = x == x1 && y == y1

isBox :: Coord -> [Coord] -> Bool
isBox p (c:cs) = if (eqCoords p c) then True else isBox p cs
isBox p [] = False

isOk :: Tile -> Bool
isOk  Ground = True
isOk  Storage = True
isOk  _ = False

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
drawGameState (State (c, bs) _) = composePictures [(atCoord c player), pictureOfMaze bs ]

drawMazeElements :: [Coord] -> [Coord] -> [Picture]
drawMazeElements boxes allCoords = (map (positionBlock (\_ -> Box)) boxes) ++ (map (positionBlock maze) allCoords)

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
startScreen = scaled 3 3 (text "press the spacebar to play Sokoban")

runInteraction :: Interaction s -> IO ()
runInteraction (Interaction state0 step handle draw)
  = interactionOf state0 step handle draw

initialBoxes :: [Coord] --finds coords of boxes in maze
initialBoxes = [C (-1) 0, C 0 0, C 1 0, C 2 0]

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) p = translated (fromIntegral x) (fromIntegral y) p

composePictures :: [Picture] -> Picture
composePictures ps = foldl (&) blank ps

{-- draws current game state, on key press, gets updated state
and passes it along as an Interaction --}
startState :: Interaction State
startState = Interaction initialState (\_ c -> c) handleEvent drawGameState

main :: IO()
main = runInteraction (resetable (withStartScreen startState))
