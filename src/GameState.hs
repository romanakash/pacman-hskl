-- game data structures and common variables are stored here
module GameState where

import Graphics.Gloss

fps :: Int
fps = 60

pacSpeed :: Float
pacSpeed = 100

wallColor :: Color
wallColor = red

dotColor :: Color
dotColor = light green

transparent :: Color
transparent = makeColor 0 0 0 0

blockSize :: Float
blockSize = 20

-- literally magic here
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

zeroVel :: Vector
zeroVel = (0, 0)

upVel :: Vector
upVel = (0, pacSpeed)

downVel :: Vector
downVel = (0, -pacSpeed)

leftVel :: Vector
leftVel = (-pacSpeed, 0)

rightVel :: Vector
rightVel = (pacSpeed, 0)

noDotsZone :: [(Vector, Vector)]
noDotsZone =
  [ ((0 * blockSize, 7 * blockSize), ((-10) * blockSize, (-20) * blockSize))
  , ((23 * blockSize, 30 * blockSize), ((-10) * blockSize, (-20) * blockSize))
  , ((8 * blockSize, 22 * blockSize), ((-9) * blockSize, (-20) * blockSize))
  ]

data Move
  = UP
  | DOWN
  | LEFT
  | RIGHT
  | NONE
  deriving (Eq, Show)

data Status
  = MENU
  | WON
  | LOST
  deriving (Eq, Show)

data Ghost = Ghost
  { loc :: Vector
  , vel :: Vector
  } deriving (Eq, Show)

-- data structure that represents the state of the game
data PacGame = Game
  { status :: Status
  , moves :: [Move]
  , lives :: Int
  , dots :: [Vector]
  , pacman :: Ghost
  , inky :: Ghost
  , pinky :: Ghost
  , blinky :: Ghost
  , clyde :: Ghost
  } deriving (Show)
