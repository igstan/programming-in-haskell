-- Game of Life

width :: Int
width = 5

height :: Int
height = 5

type Pos = (Int, Int)

type Board = [Pos]

glider :: Board
glider = [(4,2), (2,3), (4,3), (3,4), (4,4)]

goto :: Pos -> IO ()
goto (x,y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeAt :: Pos -> String -> IO ()
writeAt pos xs = do goto pos
                    putStr xs

seqn :: [IO a] -> IO ()
seqn []     = return ()
seqn (a:as) = do a
                 seqn as

showCells :: Board -> IO ()
showCells b = seqn [writeAt p "O" | p <- b]

isAlive :: Board -> Pos -> Bool
isAlive b p = elem p b

isEmpty :: Board -> Pos -> Bool
isEmpty board position = not (isAlive board position)

neighbs :: Pos -> [Pos]
neighbs (x,y) = map wrap [(x-1,y-1), (x,y-1),
                          (x+1,y-1), (x-1,y),
                          (x+1,y), (x-1,y+1),
                          (x,y+1), (x+1,y+1)]

wrap :: Pos -> Pos
wrap (x,y) = (((x-1) `mod` width) + 1,
              ((y-1) `mod` height) + 1)

liveNeighbs :: Board -> Pos -> Int
liveNeighbs board = length . filter (isAlive board) . neighbs

survivors :: Board -> [Pos]
survivors board = [p | p <- board, elem (liveNeighbs board p) [2,3]]

-- births :: Board -> [Pos]
-- births board = [(x,y) | x <- [1..width],
--                         y <- [1..height],
--                         isEmpty board (x,y),
--                         liveNeighbs board(x,y) == 3]

births :: Board -> [Pos]
births board = [p | p <- rmdups (concat (map neighbs board)),
                    isEmpty board p,
                    liveNeighbs board p == 3]

rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : rmdups (filter (/= x) xs)

nextgen :: Board -> Board
nextgen board = survivors board ++ births board

clearScreen :: IO ()
clearScreen = putStr "\ESC[2J"

life :: Board -> IO ()
life board = do clearScreen
                showCells board
                wait 500000
                life (nextgen board)

wait :: Int -> IO ()
wait n = seqn [return () | _ <- [1..n]]
