module Collision where

import           Graphics.Gloss

import           GameState
import           Maze                           ( intWallIndex )

{-- UPDATE function to check if pacman has hit a wall
    uses wallCollsion for detection
    if hit sets pac vel = zeroVel
--}
wallBounce :: PacGame -> PacGame
wallBounce game
        |
        --if a collision is detected stops pacman
          wallCollision (vx, vy) (lx, ly) = game
                { pacman = Ghost {name = PACMAN, vel = (0, 0), loc = (lx', ly')}
                }
        | otherwise = game
    where
        -- pacman
        (lx, ly) = (loc $ pacman game)
        (vx, vy) = (vel $ pacman game)
        -- round off lx,ly to keep it as a multiple of blockSize
        lx'      = fromIntegral $ round lx
        ly'      = fromIntegral $ round ly

-- takes pac vel, pac loc and it detects if it hits a wall
-- intWallIndex if of form (i,j) check i = y, j = x
-- add or subtract blockOffset according to vel to get relevant edge
-- checks if that edge is a wall
wallCollision :: Vector -> Vector -> Bool
wallCollision (vx, vy) (x, y)
        |
        -- when moving right
          (vx, vy) == rightVel pacSpeed
        = elem (ry + rBlockOffset, rx + rNextBlock) intWallIndex
                || elem (ry - rBlockOffset, rx + rNextBlock) intWallIndex
        |
        -- left
          (vx, vy) == leftVel pacSpeed
        = elem (ry + rBlockOffset, rx - rNextBlock) intWallIndex
                || elem (ry - rBlockOffset, rx - rNextBlock) intWallIndex
        |
        -- up
          (vx, vy) == upVel pacSpeed
        = elem (ry + rNextBlock, rx + rBlockOffset) intWallIndex
                || elem (ry + rNextBlock, rx - rBlockOffset) intWallIndex
        |
        -- down
          (vx, vy) == downVel pacSpeed
        = elem (ry - rNextBlock, rx + rBlockOffset) intWallIndex
                || elem (ry - rNextBlock, rx - rBlockOffset) intWallIndex
        |
        -- when stationary make sure wallCollsion is false to make sure pacman can move again
          otherwise
        = False
        where (rx, ry) = (round x, round y)

{-- Checks if a move is valid for the current location
        used by turnGhost and executeMove
--}
isValidMove :: Vector -> Move -> Bool
isValidMove (x, y) move = if condition then notWall else False
    where
        -- makes sure a move occurs only when (x,y) is a multiple of blockSize
        -- don't know why we should do this but the game breaks if not
        condition =
                (mod (round $ x + blockOffset) (round blockSize))
                        == 0
                        && (mod (round $ y + blockOffset) (round blockSize))
                        == 0
        -- if not in wallIndex return true
        notWall  = not $ or $ map check pathways
        -- checks whether the x',y' are in intWallIndex
        check (x', y') = elem ((round y'), (round x')) intWallIndex
        -- get the value of nextBlock and nextNextBlock of x,y
        pathways = case move of
                UP ->
                        [ (x + i, y + j)
                        | i <- [-blockOffset, blockOffset]
                        , j <- [nextBlock, nextNextBlock]
                        ]
                DOWN ->
                        [ (x + i, y - j)
                        | i <- [-blockOffset, blockOffset]
                        , j <- [nextBlock, nextNextBlock]
                        ]
                LEFT ->
                        [ (x - i, y + j)
                        | i <- [nextBlock, nextNextBlock]
                        , j <- [-blockOffset, blockOffset]
                        ]
                RIGHT ->
                        [ (x + i, y + j)
                        | i <- [nextBlock, nextNextBlock]
                        , j <- [-blockOffset, blockOffset]
                        ]
                _ -> [(0, 0)] -- (0,0) is always in the wallIndex
