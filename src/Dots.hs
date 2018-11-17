module Dots where

import           Data.List                      ( sort )
import           Graphics.Gloss

import           GameState
import           Maze                           ( wallIndex )

--generates intial dots array for initialGameState
initDotsArray :: [Vector]
-- checks if dot is not near a wallIndex and also noDotsZone as set in GameState
initDotsArray = filter isNotInDotZone $ filter isNotInWallIndex arr
    where
        -- creates an array of dots for every column
        -- multiply by blockSize to get location of dot and add offset to center it
        arr =
                [ (j * blockSize + blockOffset, i * blockSize - blockOffset)
                | i <- [(-1), (-2) .. (-29)]
                , j <- [1, 2 .. 28]
                ]
        -- offsets to operated on a given index to check if it is near a wall Index
        offsets = [blockOffset, (-blockOffset), blockOffset, (-blockOffset)]
        -- generates an array of possible offsets for a dot
        offsetIndex (j, i) =
                [ (i + k, j + m) | k <- offsets, m <- sort offsets ]
        -- checks if possible offsets for an index is not in the wall Index
        isNotInWallIndex (j, i) =
                and $ map (\d -> not $ elem d wallIndex) (offsetIndex (j, i))
        -- checks if given index is not in noDotsZone
        isNotInDotZone (j, i) = not $ or
                [ (xa < j && j < xb) && (yb < i && i < ya)
                | ((xa, xb), (ya, yb)) <- noDotsZone
                ]

-- renders the dots from game state to the screen
renderDots :: PacGame -> Picture
renderDots game = pictures
        [ translate x y $ color dotColor $ rectangleSolid
                  (blockOffset / 2)
                  (blockOffset / 2)
        | (x, y) <- dotsArr
        ]
        where dotsArr = dots game

-- checks if pacman has eaten a dot and removes that dot from the dots array in game state
eatDots :: PacGame -> PacGame
eatDots game | elem (lx', ly') dtsI = game { dots = newDtsF }
             | otherwise            = game
    where
        (lx , ly ) = (loc $ pacman game)
        -- round off for Eq check
        (lx', ly') = (round lx, round ly)
        -- converts dots array from (Float,Float) -> (Int,Int)
        dtsI       = rListOfTuples $ dots game
        -- filters out the eaten dots
        newDtsI    = filter (\(x, y) -> not $ (lx', ly') == (x, y)) dtsI
        -- converts dots array from (Int,Int) -> (Float,Float)
        newDtsF    = map (\(x, y) -> (fromIntegral x, fromIntegral y)) newDtsI
