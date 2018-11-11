module Enemy where
import           Graphics.Gloss
import           System.Random
import System.IO.Unsafe

import           GameState
import           Collision                      ( isValidMove
                                                , wallCollision
                                                )

randomMove :: [Move] -> Move
randomMove []  = NONE
randomMove [m] = m
randomMove mvs
        | r == z = mvs !! 0
        | r == 1 = mvs !! 1
        | r == 2 = mvs !! 2
        | r == 3 = mvs !! 3
        | otherwise = NONE
    where
        f = do unsafePerformIO newStdGen
        g = unsafePerformIO $ getStdGen
        (r,g') = randomR (0,1) $ f
        z :: Int
        z = 0
        lo :: Int
        lo = 0
        hi :: Int
        hi = length mvs - 1

turnGhost :: PacGame -> PacGame
turnGhost game
        | length moves > 0 = game
                { inky = Ghost {vel = (nextVel (ix, iy)), loc = (ix', iy')}
                }
        | otherwise = game
    where
        nextVel (x, y) = moveToVel $ randomMove $ avalMoves (ix, iy)
        avalMoves (x, y) = filter (\m -> isValidMove (x, y) m) moves
        moveToVel m = case m of
            UP    -> upVel
            DOWN  -> downVel
            LEFT  -> leftVel
            RIGHT -> rightVel
            NONE  -> (vix,viy)
        moves      = if (vix,viy) == (upVel) || (vix,viy) == downVel
                        then [LEFT, RIGHT]
                    else [UP,DOWN]
        -- inky
        (ix , iy ) = (loc $ inky game)
        (vix, viy) = (vel $ inky game)
        ix'        = fromIntegral $ round ix
        iy'        = fromIntegral $ round iy

moveGhost :: Float -> PacGame -> PacGame
moveGhost seconds game = game
        { inky  = Ghost {vel = (vix, viy), loc = (ix', iy')}
        , pinky = Ghost {vel = (vpx, vpy), loc = (px', py')}
        }
    where
        -- inky
        (ix , iy ) = (loc $ inky game)
        (vix, viy) = (vel $ inky game)
        ix'        = ix + vix * seconds
        iy'        = iy + viy * seconds
        -- pinky
        (px , py ) = (loc $ pinky game)
        (vpx, vpy) = (vel $ pinky game)
        px'        = px + vpx * seconds
        py'        = py + vpy * seconds
        -- blinky
        (bx , by ) = (loc $ blinky game)
        (vbx, vby) = (vel $ blinky game)
        -- clyde
        (cx , cy ) = (loc $ clyde game)
        (vcx, vcy) = (vel $ clyde game)

ghostWallBounce :: PacGame -> PacGame
ghostWallBounce game
        | wallCollision (vix, viy) (ix, iy) = game
                { inky = Ghost {vel = (0, 0), loc = (ix', iy')}
                }
        | otherwise = game
    where
        -- inky
        (ix , iy ) = (loc $ inky game)
        (vix, viy) = (vel $ inky game)
        ix'        = fromIntegral $ round ix
        iy'        = fromIntegral $ round iy
        -- pinky
        (px , py ) = (loc $ pinky game)
        (vpx, vpy) = (vel $ pinky game)
        px'        = fromIntegral $ round px
        py'        = fromIntegral $ round py
        -- blinky
        (bx , by ) = (loc $ blinky game)
        (vbx, vby) = (vel $ blinky game)
        bx'        = fromIntegral $ round bx
        by'        = fromIntegral $ round by
        -- clyde
        (cx , cy ) = (loc $ clyde game)
        (vcx, vcy) = (vel $ clyde game)
        cx'        = fromIntegral $ round cx
        cy'        = fromIntegral $ round iy

-- renders blinky as a picture
-- translate gets pacLoc from game state
renderInky :: PacGame -> Picture
renderInky game = translate x y $ color blue $ circleSolid blockSize
        where (x, y) = (loc $ inky game)

-- renders blinky as a picture
-- translate gets pacLoc from game state
renderPinky :: PacGame -> Picture
renderPinky game = translate x y
        $ color (makeColor 255 192 203 1) (circleSolid blockSize)
        where (x, y) = (loc $ pinky game)

-- renders blinky as a picture
-- translate gets pacLoc from game state
renderBlinky :: PacGame -> Picture
renderBlinky game = translate x y $ color red $ circleSolid blockSize
        where (x, y) = (loc $ blinky game)

-- renders blinky as a picture
-- translate gets pacLoc from game state
renderClyde :: PacGame -> Picture
renderClyde game = translate x y $ color orange $ circleSolid blockSize
        where (x, y) = (loc $ clyde game)
