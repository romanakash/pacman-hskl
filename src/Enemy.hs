module Enemy where
import           Graphics.Gloss
import           Graphics.Gloss.Data.Vector

import           GameState
import           Collision                      ( isValidMove )
import           Dots                           ( initDotsArray )

{-- render methods for the ghosts
        ghosts have shape circle
        radius of ghosts is the same as blockSize
        translate offsets the ghost according to the loc
--}

-- main render method for the ghosts
renderGhosts :: PacGame -> Picture
renderGhosts game = pictures
        [renderInky game, renderPinky game, renderBlinky game, renderClyde game]

-- rnewTurninky as a picture
renderInky :: PacGame -> Picture
renderInky game = translate x y $ color cyan $ circleSolid blockSize
        where (x, y) = (loc $ inky game)

-- renders blinky as a picture
renderPinky :: PacGame -> Picture
renderPinky game = translate x y $ color (light rose) $ circleSolid blockSize
        where (x, y) = (loc $ pinky game)

-- renders blinky as a picture
renderBlinky :: PacGame -> Picture
renderBlinky game = translate x y $ color red $ circleSolid blockSize
        where (x, y) = (loc $ blinky game)

-- renders blinky as a picture
renderClyde :: PacGame -> Picture
renderClyde game = translate x y $ color orange $ circleSolid blockSize
        where (x, y) = (loc $ clyde game)


{-- UPDATE function to decide whether to release the ghost to play
        sets ghost loc -> ghostEntryLoc, vel -> oneof right, left
--}
releaseGhost :: PacGame -> PacGame
releaseGhost game
        | rSeconds == 0 = game
                { blinky = Ghost
                                   { name = BLINKY
                                   , loc  = ghostEntryLoc
                                   , vel  = rightVel speed
                                   }
                }
        | rSeconds == 2 = game
                { pinky = Ghost
                                  { name = PINKY
                                  , loc  = ghostEntryLoc
                                  , vel  = leftVel speed
                                  }
                }
        | rSeconds == 7 = game
                { clyde = Ghost
                                  { name = CLYDE
                                  , loc  = ghostEntryLoc
                                  , vel  = rightVel speed
                                  }
                }
        | rSeconds == 13 = game
                { inky = Ghost
                                 { name = INKY
                                 , loc  = ghostEntryLoc
                                 , vel  = rightVel speed
                                 }
                }
        | otherwise = game
    where
        rSeconds = round $ time game
        speed    = ghostSpeed game

{-- UPDATE function to moves ghosts
        new loc is based on ghost vel and time passed since last update
        displacement = init displacement + vel * time
        make sure to init Ghost data structure with all fields
--}
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

{-- UPDATE functions to turn ghosts
        make sure to have separate update function for each ghost
--}

turnGhosts :: PacGame -> PacGame
turnGhosts game = turnInky $ turnPinky $ turnBlinky $ turnClyde game

turnInky :: PacGame -> PacGame
turnInky game = game { inky = Ghost {name = INKY, loc = l, vel = v} }
    where
        (l, v) = newTurn ghost game
        ghost  = Ghost
                { name = INKY
                , loc  = (loc $ inky game)
                , vel  = (vel $ inky game)
                }

turnPinky :: PacGame -> PacGame
turnPinky game = game { pinky = Ghost {name = PINKY, loc = l, vel = v} }
    where
        (l, v) = newTurn ghost game
        ghost  = Ghost
                { name = PINKY
                , loc  = (loc $ pinky game)
                , vel  = (vel $ pinky game)
                }

turnBlinky :: PacGame -> PacGame
turnBlinky game = game { blinky = Ghost {name = BLINKY, loc = l, vel = v} }
    where
        (l, v) = newTurn ghost game
        ghost  = Ghost
                { name = BLINKY
                , loc  = (loc $ blinky game)
                , vel  = (vel $ blinky game)
                }

turnClyde :: PacGame -> PacGame
turnClyde game = game { clyde = Ghost {name = CLYDE, loc = l, vel = v} }
    where
        (l, v) = newTurn ghost game
        ghost  = Ghost
                { name = CLYDE
                , loc  = (loc $ clyde game)
                , vel  = (vel $ clyde game)
                }

{-- checks if ghost can turn and returns the new (loc,vel)
        checks available moves for ghosts
        select move according to whichMove
        whichMove selects move which take ghost closer to target
        ghostTarget provides the target fot whichMove
--}
newTurn :: Ghost -> PacGame -> (Vector, Vector)
-- whenever a move is aval nextVel is called
newTurn ghost game | length avalMoves > 0 = ((x', y'), nextVel)
                   | otherwise            = ((x, y), (vx, vy))
    where
        gName    = name ghost
        (x, y)   = loc ghost
        x'       = fromIntegral $ round x
        y'       = fromIntegral $ round y
        (rx, ry) = (round x, round y)
        (vx, vy) = vel ghost
        speed    = ghostSpeed game
        uV       = upVel speed
        dV       = downVel speed
        lV       = leftVel speed
        rV       = rightVel speed
        moveToVel m = case m of
                UP    -> uV
                DOWN  -> dV
                LEFT  -> lV
                RIGHT -> rV
                -- keeps the current direction
                NONE  -> (vx, vy)
        -- decides which direction the ghost should turn
        -- calls avalMoves and passes it to whichMove
        -- calls ghostTarget and passed it to whichMove
        -- finally converts move to relevant velocity
        nextVel :: Vector
        nextVel = moveToVel $ whichMove (ux, uy) (tx, ty) $ avalMoves
            where
                (ux, uy) = loc ghost
                (tx, ty) = ghostTarget ghost game
        -- gets all aval moves for the ghost using isValidMove
        avalMoves :: [Move]
        avalMoves |
                -- if inside ghost box no aval moves
                    (x, y) == getStartLoc gName = []
                  |
                -- makes sure ghost doesn't get access to teleporter left
                    (rx, ry) == (150, -290) = [ m | m <- mvs, not $ m == LEFT ]
                  |
                -- makes sure ghost doesn't get access to teleporter right
                    (rx, ry) == (450, -290) = [ m | m <- mvs, not $ m == RIGHT ]
                  | otherwise = mvs
            where
                -- checks isValidMove for every move
                mvs = filter (\m -> isValidMove (x, y) m) (moves (vx, vy))
                getStartLoc name = case name of
                        INKY   -> startLocInky
                        PINKY  -> startLocPinky
                        BLINKY -> startLocBlinky
                        CLYDE  -> startLocClyde
                -- implements standard pacman rules for a ghost move
                -- makes sure ghosts can't reverse
                moves (vx, vy)
                        | elem (round vx, round vy) noUpZone = [LEFT, RIGHT]
                        | (vx, vy) == uV = [UP, LEFT, RIGHT]
                        | (vx, vy) == lV = [UP, LEFT, DOWN]
                        | (vx, vy) == dV = [LEFT, DOWN, RIGHT]
                        | (vx, vy) == rV = [UP, DOWN, RIGHT]
                        | otherwise = [UP, LEFT, DOWN, RIGHT]

-- returns the target for a particular ghost based on ghostMode
ghostTarget :: Ghost -> PacGame -> Vector
ghostTarget ghost game
        |
        -- scatters the ghost to the four corners
          aiMode == SCATTER = case gName of
                INKY   -> fourCorners !! 3
                PINKY  -> fourCorners !! 0
                BLINKY -> fourCorners !! 1
                CLYDE  -> fourCorners !! 2
        |
        -- targets for ghosts based on standard pacman rules
          otherwise = case gName of
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
        gName      = name ghost
        (lx , ly ) = (loc $ pacman game)
        (vlx, vly) = (vel $ pacman game)
        -- inky
        inkyTarget = rotateV (-angle) (loc $ blinky game)
        angle      = angleVV (loc $ blinky game) (twoTiles)
        twoTiles | (vlx, vly) == upVel pacSpeed    = (lx, ly + blockSize * 4)
                 | (vlx, vly) == downVel pacSpeed  = (lx, ly + blockSize * 4)
                 | (vlx, vly) == leftVel pacSpeed  = (lx - blockSize * 4, ly)
                 | (vlx, vly) == rightVel pacSpeed = (lx + blockSize * 4, ly)
                 | otherwise                       = (lx, ly)
        -- pinky
        fourTiles = blockSize * 8
        pinkyTarget
                | (vlx, vly) == upVel pacSpeed    = (lx, ly + fourTiles)
                | (vlx, vly) == downVel pacSpeed  = (lx, ly + fourTiles)
                | (vlx, vly) == leftVel pacSpeed  = (lx - fourTiles, ly)
                | (vlx, vly) == rightVel pacSpeed = (lx + fourTiles, ly)
                | otherwise                       = (lx, ly)
        -- clyde
        clydeTarget
                |
                -- if distance between pacman and clyde > 8 tiles
                  ((sqrt ((lx - cx) ** 2 + (ly - cy) ** 2))) > blockSize * 16 = ( lx
                                                                                , ly
                                                                                )
                | otherwise = fourCorners !! 2
                where (cx, cy) = (loc $ clyde game)

-- advanced ai right here
whichMove :: Vector -> Vector -> [Move] -> Move
-- if no aval moves return NONE to maintain current ghost vel
whichMove _        _        []    = NONE
whichMove _        _        [m]   = m
-- uv - user vector which is one of the ghosts
-- tv - target vector which is a specific tile
whichMove (ux, uy) (tx, ty) moves = closestMove
    where
        -- return move whose PT has minimum distance to target
        -- (9999,NONE) will always be maximum
        closestMove :: Move
        closestMove = snd $ foldr min (9999, NONE) mD
        -- move paired up with distance to pacman
        mD :: [(Float, Move)]
        mD = map d mPT
        -- calculates distance between two vectors
        d :: (Vector, Move) -> (Float, Move)
        d ((x, y), m) = ((sqrt ((x - tx) ** 2 + (y - ty) ** 2)), m)
        -- move paired up with relevant potential tile
        mPT :: [(Vector, Move)]
        mPT = map f moves
        -- potential tile is future tile if ghost makes that particular move
        f :: Move -> (Vector, Move)
        f move = case move of
                UP    -> ((ux, uy + blockSize * 2), UP)
                LEFT  -> ((ux - blockSize * 2, uy), LEFT)
                DOWN  -> ((ux, uy - blockSize * 2), DOWN)
                RIGHT -> ((ux + blockSize * 2, uy), RIGHT)


{-- UPDATE function to check if a ghost has hit pacman
        resets the gamestate if hit is detected
--}
hitGhost :: PacGame -> PacGame
hitGhost game
        | checkForHit = game
                { time   = 0
                , lives  = ls - 1
                , moves  = [LEFT]
                , pacman = Ghost
                                   { name = PACMAN
                                   , loc  = startLocPac
                                   , vel  = (0, 0)
                                   }
                , inky   = Ghost {name = INKY, loc = startLocInky, vel = (0, 0)}
                , pinky  = Ghost
                                   { name = PINKY
                                   , loc  = startLocPinky
                                   , vel  = (0, 0)
                                   }
                , blinky = Ghost
                                   { name = BLINKY
                                   , loc  = startLocBlinky
                                   , vel  = (0, 0)
                                   }
                , clyde  = Ghost
                                   { name = CLYDE
                                   , loc  = startLocClyde
                                   , vel  = (0, 0)
                                   }
                }
        | otherwise = game
    where
        ls       = lives game
        (x , y ) = (loc $ pacman game)
        (rx, ry) = (round x, round y)
        (vx, vy) = (vel $ pacman game)
        -- locs of all the ghosts
        aiLocs =
                [ (loc $ inky game)
                , (loc $ pinky game)
                , (loc $ blinky game)
                , (loc $ clyde game)
                ]
        -- locs of pacman including the 4 edges
        pacLocOffsets =
                [ (x              , y)
                , (x              , y + blockOffset)
                , (x              , y - blockOffset)
                , (x + blockOffset, y)
                , (x - blockOffset, y)
                ]
        -- make sure to round float to int before checking for Eq
        rAiLocs        = rListOfTuples aiLocs
        rPacLocOffsets = rListOfTuples pacLocOffsets
        -- checks if pac loc or one of its edges == loc any ghost
        checkForHit    = or $ map (\(rx, ry) -> elem (rx, ry) rAiLocs)
                                  rPacLocOffsets
