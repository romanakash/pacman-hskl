module Enemy where
import           Graphics.Gloss
import           Graphics.Gloss.Data.Vector

import           GameState
import           Collision                      ( isValidMove
                                                , wallCollision
                                                )

ghostTarget :: Ghost -> PacGame -> Vector
ghostTarget ghost game
        | aiMode == SCATTER = case gName of
                INKY   -> fourCorners !! 3
                PINKY  -> fourCorners !! 0
                BLINKY -> fourCorners !! 1
                CLYDE  -> fourCorners !! 2
        | otherwise = case gName of
                -- inky targets two tiles ahead of pacman and w.r.t to blinky
                INKY   -> inkyTarget
                -- pinky targets four tiles ahead of pacman
                PINKY  -> pinkyTarget
                -- blinky targets pacman directly
                BLINKY -> (lx, ly)
                -- clyde targets pacman directly when distance is greater than eight tiles
                -- when distance is lesser he goes to the corner
                CLYDE  -> clydeTarget
    where
        aiMode     = ghostMode game
        (lx, ly)   = (loc $ pacman game)
        (vx, vy)   = (vel $ pacman game)
        gName      = name ghost
        inkyTarget = rotateV (-angle) (loc $ blinky game)
        angle      = angleVV (loc $ blinky game) (twoTiles)
        twoTiles | (vx, vy) == upVel pacSpeed    = (lx, ly + blockSize * 4)
                 | (vx, vy) == downVel pacSpeed  = (lx, ly + blockSize * 4)
                 | (vx, vy) == leftVel pacSpeed  = (lx - blockSize * 4, ly)
                 | (vx, vy) == rightVel pacSpeed = (lx + blockSize * 4, ly)
                 | otherwise                     = (lx, ly)
        fourTiles = blockSize * 8
        pinkyTarget
                | (vx, vy) == upVel pacSpeed    = (lx, ly + fourTiles)
                | (vx, vy) == downVel pacSpeed  = (lx, ly + fourTiles)
                | (vx, vy) == leftVel pacSpeed  = (lx - fourTiles, ly)
                | (vx, vy) == rightVel pacSpeed = (lx + fourTiles, ly)
                | otherwise                     = (lx, ly)
        clydeTarget
                | ((sqrt ((lx - cx) ** 2 + (ly - cy) ** 2))) > blockSize * 16
                = (lx, ly)
                | otherwise = fourCorners !! 2
                where (cx,cy) = (loc $ clyde game)
-- advanced ai right here folks
whichMove :: Vector -> Vector -> [Move] -> Move
-- uv - user vector which is one of the ghosts
-- tv - target vector which is a specific tile
whichMove _          _          []    = NONE
whichMove (uvx, uvy) (tvx, tvy) moves = closestMove
    where
        -- (9999,NONE) will always be maximum
        closestMove = snd $ foldr min (9999, NONE) mD
        -- move paired up with distance to pacman
        mD          = map d mPT
        -- calculates distance between two vectors
        d ((x, y), m) = ((sqrt ((x - tvx) ** 2 + (y - tvy) ** 2)), m)
        -- move paired up with relevant potential tile
        mPT = map f moves
        f move = case move of
                UP    -> ((uvx, uvy + blockSize * 2), UP)
                LEFT  -> ((uvx - blockSize * 2, uvy), LEFT)
                DOWN  -> ((uvx, uvy - blockSize * 2), DOWN)
                RIGHT -> ((uvx + blockSize * 2, uvy), RIGHT)

-- checks available moves for ghosts
-- moves func makes sure ghosts can't reverse
-- if move is available changes ghost velocity
turnGhost :: PacGame -> PacGame
turnGhost game
        | length iAvalMoves > 0 = game
                { inky = Ghost {name = INKY, vel = iNextVel, loc = (ix', iy')}
                }
        | length pAvalMoves > 0 = game
                { pinky = Ghost {name = PINKY, vel = pNextVel, loc = (px', py')}
                }
        | length bAvalMoves > 0 = game
                { blinky = Ghost
                                   { name = BLINKY
                                   , vel  = bNextVel
                                   , loc  = (bx', by')
                                   }
                }
        | length cAvalMoves > 0 = game
                { clyde = Ghost {name = CLYDE, vel = cNextVel, loc = (cx', cy')}
                }
        | otherwise = game
    where
        speed = ghostSpeed game
        nextVel :: Ghost -> Vector
        nextVel ghost = moveToVel $ whichMove (uvx, uvy) (tvx, tvy) $ avalMoves
                ghost
            where
                (uvx, uvy) = loc ghost
                (tvx, tvy) = ghostTarget ghost game
        avalMoves :: Ghost -> [Move]
        avalMoves ghost
            -- makes sure ghost doesn't get access to teleporter
            | (round x,round y) == (150,-290) = [m | m <- mvs, not $ m == LEFT]
            | (round x,round y) == (250,-290) = [m | m <- mvs, not $ m == LEFT]
            | otherwise = mvs
            where
                mvs = filter (\m -> isValidMove (x, y) m) (moves (vx, vy))
                (x , y ) = loc $ ghost
                (vx, vy) = vel $ ghost
        uV = upVel speed
        dV = downVel speed
        lV = leftVel speed
        rV = rightVel speed
        moveToVel m = case m of
                UP    -> uV
                DOWN  -> dV
                LEFT  -> lV
                RIGHT -> rV
                NONE  -> (vix, viy)
        moves (vx, vy) | elem (round vx, round vy) noUpZone = [LEFT, RIGHT]
                       | (vx, vy) == uV = [UP, LEFT, RIGHT]
                       | (vx, vy) == lV = [UP, LEFT, DOWN]
                       | (vx, vy) == dV = [LEFT, DOWN, RIGHT]
                       | (vx, vy) == rV = [UP, DOWN, RIGHT]
                       | otherwise = [UP, LEFT, DOWN, RIGHT]
        -- inky
        (ix , iy ) = (loc $ inky game)
        (vix, viy) = (vel $ inky game)
        ix'        = fromIntegral $ round ix
        iy'        = fromIntegral $ round iy
        iAvalMoves = avalMoves (inky game)
        iNextVel   = nextVel (inky game)
        -- pinky
        (px , py ) = (loc $ pinky game)
        (vpx, vpy) = (vel $ pinky game)
        px'        = fromIntegral $ round px
        py'        = fromIntegral $ round py
        pAvalMoves = avalMoves (pinky game)
        pNextVel   = nextVel (pinky game)
        -- blinky
        (bx , by ) = (loc $ blinky game)
        (vbx, vby) = (vel $ blinky game)
        bx'        = fromIntegral $ round bx
        by'        = fromIntegral $ round by
        bAvalMoves = avalMoves (blinky game)
        bNextVel   = nextVel (blinky game)
        -- clyde
        (cx , cy ) = (loc $ clyde game)
        (vcx, vcy) = (vel $ clyde game)
        cx'        = fromIntegral $ round cx
        cy'        = fromIntegral $ round cy
        cAvalMoves = avalMoves (clyde game)
        cNextVel   = nextVel (clyde game)

moveGhost :: Float -> PacGame -> PacGame
moveGhost seconds game = game
        { inky   = Ghost {name = INKY, vel = (vix, viy), loc = (ix', iy')}
        , pinky  = Ghost {name = PINKY, vel = (vpx, vpy), loc = (px', py')}
        , blinky = Ghost {name = BLINKY, vel = (vbx, vby), loc = (bx', by')}
        , clyde  = Ghost {name = CLYDE, vel = (vcx, vcy), loc = (cx', cy')}
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
        bx'        = bx + vbx * seconds
        by'        = by + vby * seconds
        -- clyde
        (cx , cy ) = (loc $ clyde game)
        (vcx, vcy) = (vel $ clyde game)
        cx'        = cx + vcx * seconds
        cy'        = cy + vcy * seconds

-- renders blinky as a picture
-- translate gets pacLoc from game state
renderInky :: PacGame -> Picture
renderInky game = translate x y $ color cyan $ circleSolid blockSize
        where (x, y) = (loc $ inky game)

-- renders blinky as a picture
-- translate gets pacLoc from game state
renderPinky :: PacGame -> Picture
renderPinky game = translate x y $ color (light rose) $ circleSolid blockSize
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
