module DataTypes where
import CodeWorld


data Tile = Wall | Ground | Storage | Box | Blank

data Direction = U | D | L | R
instance Show Direction where
        show d = directionMap d

directionMap :: Direction -> String
directionMap U = "U"
directionMap L = "L"
directionMap R = "R"
directionMap D = "D"

data Coord = C Integer Integer
instance Show Coord where
        show (C x y) = "(C "++ show x ++ ", " ++ show y ++")"

data State = State Coord Direction [Coord]
instance Show State where
  show (State c d bs) = "State (" ++ show c ++ show bs ++ ") " ++ show d

data SSState world = StartScreen | Running world --polymorphic in terms of world
data Interaction world = Interaction
        world
        (Double -> world -> world)
        (Event -> world -> world)
        (world -> Picture)

