module Pacman where
import           Graphics.Gloss

import           GameState
import           Collision                      ( isValidMove )

{-- UPDATE function to render pacman as a picture --}
renderPacman :: PacGame -> Picture
renderPacman game = translate x y $ color yellow $ circleSolid blockSize
        where (x, y) = (loc $ pacman game)

{-- UPDATE function to move pacman around with --}
movePacman :: Float -> PacGame -> PacGame
movePacman seconds game = game
        { pacman = Ghost {name = PACMAN, loc = (x', y'), vel = (vx, vy)}
        }
    where
        (x , y ) = (loc $ pacman game)
        (vx, vy) = (vel $ pacman game)
        -- distance = initdistance + velocity * time
        x'       = x + vx * seconds
        y'       = y + vy * seconds

{-- UPDATE function to check moves cache and execute the move --}
executeMove :: PacGame -> PacGame
executeMove game
        |
        -- executes only if pacman is stationary or hit a wall
        -- isValidMove checks for any openings
          (vx, vy) == zeroVel || isValidMove (x, y) move = case move of
                UP    -> goUp
                DOWN  -> goDown
                LEFT  -> goLeft
                RIGHT -> goRight
                NONE  -> game
        | otherwise = game
    where
        (x , y ) = (loc $ pacman game)
        (vx, vy) = (vel $ pacman game)
        x'       = fromIntegral $ round x
        y'       = fromIntegral $ round y
        mvs      = moves game
        -- make sure we don't use head on a empty list
        move     = if not $ null mvs then head mvs else NONE
        -- removes the move given as a parameter from moves cache
        newMoves m = filter (\move -> move /= m) mvs
        goUp = game
                { moves  = (newMoves UP)
                , pacman = Ghost
                                   { name = PACMAN
                                   , vel  = upVel pacSpeed
                                   , loc  = (x', y')
                                   }
                }
        goDown = game
                { moves  = (newMoves DOWN)
                , pacman = Ghost
                                   { name = PACMAN
                                   , vel  = downVel pacSpeed
                                   , loc  = (x', y')
                                   }
                }
        goLeft = game
                { moves  = (newMoves LEFT)
                , pacman = Ghost
                                   { name = PACMAN
                                   , vel  = leftVel pacSpeed
                                   , loc  = (x', y')
                                   }
                }
        goRight = game
                { moves  = (newMoves RIGHT)
                , pacman = Ghost
                                   { name = PACMAN
                                   , vel  = rightVel pacSpeed
                                   , loc  = (x', y')
                                   }
                }

{-- UPDATE function to teleport pacman when going on edge --}
teleportPacman :: PacGame -> PacGame
teleportPacman game
        | (rx, ry) == (0, -290) = game
                { pacman = Ghost
                                   { name = PACMAN
                                   , loc  = (600, -290)
                                   , vel  = leftVel pacSpeed
                                   }
                }
        | (rx, ry) == (600, -290) = game
                { pacman = Ghost
                                   { name = PACMAN
                                   , loc  = (0, -290)
                                   , vel  = rightVel pacSpeed
                                   }
                }
        | otherwise = game
    where
        (x , y ) = loc $ pacman game
        (rx, ry) = (round x, round y)
