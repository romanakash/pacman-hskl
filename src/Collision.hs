module Collision where

import           Graphics.Gloss

import           GameState
import           Maze                           ( intWallIndex )

wallBounce :: PacGame -> PacGame
wallBounce game
        |
        --if a collision is detected stops pacman
          wallCollision (vx, vy) (lx, ly) = game
                { pacman = Ghost {vel = (0, 0), loc = (lx', ly')}
                }
        | otherwise = game
    where
        -- pacman
        (lx, ly)   = (loc $ pacman game)
        (vx, vy)   = (vel $ pacman game)
        -- round off lx,ly to keep it as a multiple of blocksize
        lx'        = fromIntegral $ round lx
        ly'        = fromIntegral $ round ly
        -- inky
        (ix , iy ) = (loc $ inky game)
        (vix, viy) = (loc $ inky game)
        ix'        = fromIntegral $ round ix
        iy'        = fromIntegral $ round iy
        -- pinky
        (px , py ) = (loc $ pinky game)
        (vpx, vpy) = (loc $ pinky game)
        px'        = fromIntegral $ round px
        py'        = fromIntegral $ round py
        -- blinky
        (bx , by ) = (loc $ blinky game)
        (vbx, vby) = (loc $ blinky game)
        bx'        = fromIntegral $ round bx
        by'        = fromIntegral $ round by
        -- clyde
        (cx , cy ) = (loc $ clyde game)
        (vcx, vcy) = (loc $ clyde game)
        cx'        = fromIntegral $ round cx
        cy'        = fromIntegral $ round iy

-- takes pacVel, pacLoc and it detects if it hits a wall
wallCollision :: Vector -> Vector -> Bool
wallCollision (vx, vy) (x, y)
        |
    -- when moving
    -- right
          (vx, vy) == rightVel
        = elem (ry + rBlockOffset, rx + rNextBlock) intWallIndex
                || elem (ry - rBlockOffset, rx + rNextBlock) intWallIndex
        |
    -- left
          (vx, vy) == leftVel
        = elem (ry + rBlockOffset, rx - rNextBlock) intWallIndex
                || elem (ry - rBlockOffset, rx - rNextBlock) intWallIndex
        |
    -- up
          (vx, vy) == upVel
        = elem (ry + rNextBlock, rx + rBlockOffset) intWallIndex
                || elem (ry + rNextBlock, rx - rBlockOffset) intWallIndex
        |
    -- down
          (vx, vy) == downVel
        = elem (ry - rNextBlock, rx + rBlockOffset) intWallIndex
                || elem (ry - rNextBlock, rx - rBlockOffset) intWallIndex
        |
    -- when stationary make sure wallCollsion is false to make sure pacman can move again
          otherwise
        = False
        where (rx, ry) = (round x, round y)

-- checks if a valid move is available
isValidMove :: Vector -> Move -> Bool
isValidMove (x, y) move = if condition
-- if not in wallIndex return true
        then not $ or $ map check pathways
        else False
        -- get the value of nextBlock and nextNextBlock of x,y
    where
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
            -- a valid move can only occur when pacLoc x,y is a multiple of blockSize
        condition =
                (mod (round $ x + blockOffset) (round blockSize))
                        == 0
                        && (mod (round $ y + blockOffset) (round blockSize))
                        == 0
            -- checks where the x',y' are in intWallIndex
        check (x', y') = elem ((round y'), (round x')) intWallIndex
