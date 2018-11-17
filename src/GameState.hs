-- game data structures and common variables are stored here
module GameState where

import           Graphics.Gloss

fps :: Int
fps = 60

pacSpeed :: Float
pacSpeed = 150

-- start location of our characters
-- multiples of blockSize + blockSize / 2
startLocPac :: Vector
startLocPac = (300, -470)

startLocInky :: Vector
startLocInky = (270, -290)

startLocPinky :: Vector
startLocPinky = (270, -290)

startLocBlinky :: Vector
startLocBlinky = (330, -290)

startLocClyde :: Vector
startLocClyde = (330, -290)

ghostEntryLoc :: Vector
ghostEntryLoc = (300,-230)

wallColor :: Color
wallColor = blue

dotColor :: Color
dotColor = light green

transparent :: Color
transparent = makeColor 0 0 0 0

blockSize :: Float
blockSize = 20

blockOffset :: Float
blockOffset = blockSize / 2

rBlockOffset :: Int
rBlockOffset = round blockOffset

nextBlock :: Float
nextBlock = blockSize / 2 + blockSize

rNextBlock :: Int
rNextBlock = round nextBlock

nextNextBlock :: Float
nextNextBlock = nextBlock + nextBlock

rNextNextBlock :: Int
rNextNextBlock = round nextNextBlock

rListOfTuples :: [(Float,Float)] -> [(Int,Int)]
rListOfTuples l =  map (\(fx, fy) -> (round fx, round fy)) l

fourCorners :: [Vector]
fourCorners = [topLeft, topRight, bottomLeft, bottomRight]
    where
        topLeft     = (1 * blockSize, (-1) * blockSize)
        topRight    = (30 * blockSize, (-1) * blockSize)
        bottomLeft  = (1 * blockSize, (-31) * blockSize)
        bottomRight = (30 * blockSize, (-31) * blockSize)


zeroVel :: Vector
zeroVel = (0, 0)

upVel :: Float -> Vector
upVel speed = (0, speed)

downVel :: Float -> Vector
downVel speed = (0, -speed)

leftVel :: Float -> Vector
leftVel speed = (-speed, 0)

rightVel :: Float -> Vector
rightVel speed = (speed, 0)

noDotsZone :: [(Vector, Vector)]
noDotsZone
        = [ ( (0 * blockSize    , 7 * blockSize)
            , ((-10) * blockSize, (-20) * blockSize)
            )
          , ( (23 * blockSize   , 30 * blockSize)
            , ((-10) * blockSize, (-20) * blockSize)
            )
          , ( (8 * blockSize   , 22 * blockSize)
            , ((-9) * blockSize, (-20) * blockSize)
            )
          ]

noUpZone :: [(Int, Int)]
noUpZone = [(270, -230), (330, -230), (270, -470), (330, -470)]

data Move
  = UP
  | LEFT
  | DOWN
  | RIGHT
  | NONE
  deriving (Eq, Show, Ord)

data Status
  = MENU
  | WON
  | LOST
  deriving (Eq, Show)

-- pacman is a type of ghost himself
-- you've become the very thing you swore to destroy
data Ghost = Ghost
  { name :: GhostName
  , loc :: Vector
  , vel :: Vector
  } deriving (Eq, Show)

data GhostName = PACMAN | INKY | PINKY | BLINKY | CLYDE deriving (Eq, Show)

data GhostMode = SCATTER | CHASE | STUNNED deriving (Eq, Show)

-- data structure that represents the state of the game
data PacGame = Game
  { time :: Float
  , status :: Status
  , moves :: [Move]
  , lives :: Int
  , dots :: [Vector]
  , pacman :: Ghost
  , inky :: Ghost
  , pinky :: Ghost
  , blinky :: Ghost
  , clyde :: Ghost
  , ghostMode :: GhostMode
  , ghostSpeed :: Float
  } deriving (Show)
