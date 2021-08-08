main :: IO ()
main = life glider

life :: Board -> IO ()
life b = do     cls
                showcells b
                wait 500000
                life (nextgen b) --recursive

cls :: IO () -- this clears the screen and pushes the current contents out of view.
cls = putStr "\ESC[2J"

type Pos = (Int,Int)

writeat :: Pos -> String -> IO () -- this displays a string at a given position by using control charatcers to moving the cursor to this position.
writeat p xs = do   goto p
                    putStr xs

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H") -- this moves the cursor ESC[]

width :: Int -- width of board
width = 10

height :: Int --height of board
height = 10

type Board = [Pos]

glider :: Board -- Initial configuration of board that produces a glider.
glider = [(4,2), (2,3), (4,3), (3,4), (4,4)]

showcells :: Board -> IO ()
showcells b = sequence_ [writeat p "O" | p <- b] --sequence_ performs a list of actions in sequence discarding their results

isAlive :: Board -> Pos -> Bool 
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1,y-1), (x,y-1),
                        (x+1,y-1), (x-1,y),
                        (x+1,y), (x-1,y+1),
                        (x,y+1), (x+1,y+1)]
wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width) + 1,
            ((y-1) `mod` height) + 1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b = length . filter (isAlive b) . neighbs

survivors :: Board -> [Pos]
survivors b = [p | p <- b, elem (liveneighbs b p) [2,3]]

births :: Board -> [Pos]
births b = [(x,y) | x <- [1..width],
                    y <- [1..height],
                    isEmpty b (x,y),
                    liveneighbs b (x,y) == 3]

nextgen :: Board -> Board
nextgen b = survivors b ++ births b

wait :: Int -> IO ()
wait n = sequence_ [return () | _<- [1..n]] --sequence_ just ensures we return IO()