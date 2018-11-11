module Main where

import           Data.List                      ( nub )

import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           Graphics.Gloss.Interface.Pure.Game

import           Collision                      ( isValidMove
                                                , wallBounce
                                                )
import           Dots                           ( eatDots
                                                , initDotsArray
                                                , renderDots
                                                )
import           Enemy                          ( renderBlinky
                                                , renderClyde
                                                , renderInky
                                                , renderPinky
                                                , moveGhost
                                                , ghostWallBounce
                                                , turnGhost
                                                )
import           GameState
import           Maze                           ( mazePicture
                                                , wallIndex
                                                )

-- start location of our PacMan
startLoc :: Vector
startLoc = (300, -470)

startLocAI :: Vector
startLocAI = (170, -230)

-- initialState of the game
initialState :: PacGame
initialState = Game
        { status  = MENU
        , moves   = []
        , lives   = 3
        , dots    = initDotsArray
        , pacman = Ghost {loc = startLoc, vel = (0, 0)}
        , inky    = Ghost {loc = startLocAI, vel = leftVel}
        , pinky   = Ghost {loc = (0, 0), vel = (0, 0)}
        , blinky  = Ghost {loc = (0, 0), vel = (0, 0)}
        , clyde   = Ghost {loc = (0, 0), vel = (0, 0)}
        }

-- renders pacman as a picture
-- translate gets loc from game state
renderPacman :: PacGame -> Picture
renderPacman game = translate x y $ color yellow $ circleSolid blockSize
        where (x, y) = (loc $ pacman game)

-- moves the pacman around with seconds as a parameter
movePacman :: Float -> PacGame -> PacGame
movePacman seconds game = game
        { pacman = Ghost {loc = (x', y'), vel = (vx, vy)}
        }
    where
        (x , y ) = (loc $ pacman game)
        (vx, vy) = (vel $ pacman game)
        x'       = x + vx * seconds
        y'       = y + vy * seconds

-- distance = initdistance + velocity * time
-- checks moves cache and executes it
executeMove :: PacGame -> PacGame
executeMove game
        |
    -- executes only if pacman is stationary or hit a wall
    -- isValidMove checks for any openings
          (vx, vy) == zeroVel || isValidMove (lx, ly) move = case move of
                UP    -> goUp
                DOWN  -> goDown
                LEFT  -> goLeft
                RIGHT -> goRight
                NONE  -> game
        | otherwise = game
    where
        (lx, ly) = (loc $ pacman game)
        (vx, vy) = (vel $ pacman game)
        lx'      = fromIntegral $ round lx
        ly'      = fromIntegral $ round ly
        mvs      = moves game
        move     = if not $ null mvs then head $ mvs else NONE
      -- make sure we don't use head on a empty list
        newMoves m = filter (\move -> move /= m) mvs
        goUp = game { moves   = (newMoves UP)
                    , pacman = Ghost {vel = upVel, loc = (lx', ly')}
                    }
        goDown = game { moves   = (newMoves DOWN)
                      , pacman = Ghost {vel = downVel, loc = (lx', ly')}
                      }
        goLeft = game { moves   = (newMoves LEFT)
                      , pacman = Ghost {vel = leftVel, loc = (lx', ly')}
                      }
        goRight = game { moves   = (newMoves RIGHT)
                       , pacman = Ghost {vel = rightVel, loc = (lx', ly')}
                       }

-- round off lx,ly to keep it as a multiple of blocksize
-- remove the executed move from the moves array
-- standard game states for moving in different directions
-- handles User Input
handleKeys :: Event -> PacGame -> PacGame
handleKeys (EventKey (SpecialKey key) _ _ _) game = case key of
        KeyUp    -> game { moves = nub $ (moves game) ++ [UP] }
        KeyDown  -> game { moves = nub $ (moves game) ++ [DOWN] }
        KeyLeft  -> game { moves = nub $ (moves game) ++ [LEFT] }
        KeyRight -> game { moves = nub $ (moves game) ++ [RIGHT] }
        _        -> game
        -- caches user input to moves array in the game state
        -- nub makes sure pacman doesn't move in the same direction twice
handleKeys _ game = game

renderUI :: PacGame -> Picture
renderUI game = color white $ scale 0.1 0.1 $ Text $ show (avalMoves (ix, iy))
    where
        totalDots  = length (dots game)
        totalLives = (lives game)
        (x , y )   = (loc $ pacman game)
        (vx, vy)   = (vel $ pacman game)
        avalMoves (x, y) = filter (\m -> isValidMove (x, y) m && (moveToVel m) /= (vix,viy)) moves
        moveToVel m = case m of
                UP    -> upVel
                DOWN  -> downVel
                LEFT  -> leftVel
                RIGHT -> rightVel
                _     -> (0, 0)
        moves      = if (vix,viy) == (upVel) || (vix,viy) == downVel
                        then [LEFT, RIGHT]
                    else [UP,DOWN]
        -- inky
        (ix , iy ) = (loc $ inky game)
        (vix, viy) = (vel $ inky game)

isDotsComplete :: PacGame -> PacGame
isDotsComplete game | length (dots game) == 0 = game { status = WON }
                    | otherwise               = game

hitGhost :: PacGame -> PacGame
hitGhost game
        | elem (rx, ry) rOffsets = game
                { lives   = ls - 1
                , pacman = Ghost {loc = startLoc, vel = (0, 0)}
                }
        | otherwise = game
    where
        ls       = lives game
        (x , y ) = (loc $ pacman game)
        (vx, vy) = (vel $ pacman game)
        (rx, ry) = (round x, round y)
        aiLocs =
                [ (loc $ inky game)
                , (loc $ pinky game)
                , (loc $ blinky game)
                , (loc $ clyde game)
                ]
        blockOffset = blockSize / 2
        offsets     = foldr (++) [] $ map f aiLocs
        f (x, y) = if (vx, vy) == upVel || (vx, vy) == downVel
                then [(x,y),(x, y + blockOffset), (x, y - blockOffset)]
                else [(x,y),(x + blockOffset, y), (x - blockOffset, y)]
        rOffsets = map (\(x, y) -> (round x, round y)) offsets

outOfLives :: PacGame -> PacGame
outOfLives game | (lives game) <= 0 = game { status = LOST }
                | otherwise         = game

renderVictory :: Picture
renderVictory = color white $ scale 0.1 0.1 $ Text "You won"

renderDefeat :: Picture
renderDefeat = color white $ scale 0.1 0.1 $ Text "You lost"

mainWindow :: Display
mainWindow = InWindow "Pacman" (1000, 1000) (0, 0)

bgColor :: Color
bgColor = black

-- main render method
render :: PacGame -> Picture
render game
        | status game == WON = renderVictory
        | status game == LOST = renderDefeat
        | otherwise = translate (-300) 300 $ pictures
                [ mazePicture
                , (renderPacman game)
                , (renderInky game)
                , (renderPinky game)
                , (renderBlinky game)
                , (renderClyde game)
                , (renderUI game)
                , (renderDots game)
                ]

main :: IO ()
main = do
        play mainWindow bgColor fps initialState render handleKeys update
    {-- :: Display -> Color -> Int - no of simulation steps per sec
    -> model The initial model i.e init game
    -> (model -> Picture) a function to convert the game to a picture.
    -> (Event -> a -> a) a function to handle input events.
    -> (ViewPort -> Float -> model -> model)
    A function to step the model one iteration. It is passed the current viewport and the amount of time for this simulation step (in seconds)
    -> IO () --}
    where
        update :: Float -> PacGame -> PacGame
        update seconds game =
                movePacman seconds
                        $ moveGhost seconds
                        $ turnGhost
                        $ ghostWallBounce
                        $ wallBounce
                        $ executeMove
                        $ isDotsComplete
                        $ eatDots
                        $ hitGhost
                        $ outOfLives
                        $ game
