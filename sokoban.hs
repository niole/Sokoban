import CodeWorld

drawTile :: Integer -> Picture
drawTile n
        | n == 0 =  wall
        | n == 1 = ground
        | n == 2 = storage
        | n == 3 = box
        | otherwise = ground

wall :: Picture
wall = colored (gray 0.9) (solidRectangle 1.0 1.0)

ground :: Picture
ground = colored yellow (solidRectangle 1.0 1.0)

storage :: Picture
storage = colored black (solidCircle 1.0 )

box :: Picture
box = colored brown (solidRectangle 1.0 1.0)

main :: IO()
--main = drawingOf (fold (&) (map drawTile [0..5])) TODO create fold for CodeWorld
main = drawingOf (drawTile 3)
