module Main where

import           Data.List                      ( nub )

import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           Graphics.Gloss.Interface.Pure.Game


import           Collision                      ( wallBounce
                                                , isValidMove
                                                )
import           Dots                           ( initDotsArray
                                                , renderDots
                                                , eatDots
                                                )
import           Enemy                          ( renderGhosts
                                                , releaseGhost
                                                , moveGhost
                                                , turnGhosts
                                                , hitGhost
                                                )
import           GameState
import           Maze                           ( wallIndex
                                                , mazePicture
                                                )
import           Pacman                         ( renderPacman
                                                , movePacman
                                                , executeMove
                                                , teleportPacman
                                                )

-- initialState of the game
initialState :: PacGame
initialState = Game
        { time       = 0.0
        , status     = MENU
        , moves      = [LEFT]
        , lives      = 3
        , dots       = initDotsArray
        , pacman     = Ghost {name = PACMAN, loc = startLocPac, vel = (0, 0)}
        , inky       = Ghost {name = INKY, loc = startLocInky, vel = (0, 0)}
        , pinky      = Ghost {name = PINKY, loc = startLocPinky, vel = (0, 0)}
        , blinky     = Ghost {name = BLINKY, loc = startLocBlinky, vel = (0, 0)}
        , clyde      = Ghost {name = CLYDE, loc = startLocClyde, vel = (0, 0)}
        , ghostMode  = SCATTER
        , ghostSpeed = 120
        }

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

{-- UPDATE functions to keep track of various game states --}

-- runs the timer, is called fps times for one second
updateTime :: PacGame -> PacGame
updateTime game = game { time = (time game) + 1 / fromIntegral fps }

-- checks if pacman has eaten all the dots
isDotsComplete :: PacGame -> PacGame
isDotsComplete game | length (dots game) == 0 = game { status = WON }
                    | otherwise               = game

-- checks if pacman is out of lives
outOfLives :: PacGame -> PacGame
outOfLives game | (lives game) <= 0 = game { status = LOST }
                | otherwise         = game

-- changes ghostMode for a particular point in time
changeGhostMode :: PacGame -> PacGame
changeGhostMode game | seconds < 7  = game { ghostMode = SCATTER }
                     | seconds < 27 = game { ghostMode = CHASE }
                     | seconds < 32 = game { ghostMode = SCATTER }
                     | seconds < 37 = game { ghostMode = CHASE }
                     | seconds < 42 = game { ghostMode = SCATTER }
                     | otherwise    = game { ghostMode = CHASE }
        where seconds = time game

renderVictory :: Picture
renderVictory =
        translate (-300) 300 $ color white $ scale 0.5 0.5 $ Text "You won"

renderDefeat :: Picture
renderDefeat =
        translate (-300) 300 $ color white $ scale 0.5 0.5 $ Text "You lost"


-- renders the UI for the game
renderUI :: PacGame -> Picture
renderUI game = color white $ scale 0.2 0.2 $ translate (-150) 850 $ pictures
        [l, lvs, tDts, clk, aiMd]
    where
        l    = Text $ "Pacman location (x,y)= " ++ show (loc $ pacman game)
        lvs  = translate 0 (-200) $ Text $ "Lives left: " ++ show (lives game)
        tDts = translate 0 (-400) $ Text $ "Dots left: " ++ show
                (length $ dots game)
        clk  = translate 0 (-600) $ Text $ "Timer: " ++ show (time game)
        aiMd = translate 0 (-800) $ Text $ "Ghost Mode: " ++ show
                (ghostMode game)

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
                , renderPacman game
                , renderGhosts game
                , renderUI game
                , renderDots game
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
                        $ turnGhosts
                        $ teleportPacman
                        $ changeGhostMode
                        $ wallBounce
                        $ executeMove
                        $ isDotsComplete
                        $ eatDots
                        $ hitGhost
                        $ releaseGhost
                        $ outOfLives
                        $ updateTime
                        $ game
