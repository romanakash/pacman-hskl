-- for some reason the maze was designed to be rendered in the second quadrant
-- by the time I realized what was happening it was already too late to fix it
-- so a typical cordinate is (x,-y)
-- i = line no of mazeInString, j = char no of mazeInString
-- j == x co-ordinate, -i == y co-ordinate
-- also multi line comments suck
module Maze where
import           Graphics.Gloss

import           GameState

mazeInString :: [String]
mazeInString =
    [   "|||||||||||||||||||||||||||||||",
        "|              |              |",
        "|              |              |",
        "|  ||||  ||||  |  ||||  ||||  |",
        "|  ||||  ||||  |  ||||  ||||  |",
        "|                             |",
        "|                             |",
        "|  ||||  |  |||||||  |  ||||  |",
        "|        |     |     |        |",
        "|        |     |     |        |",
        "|||||||  ||||  |  ||||  |||||||",
        "      |  |           |  |      ",
        "      |  |           |  |      ",
        "|||||||  |  |||||||  |  |||||||",
        "|           |     |           |",
        "|           |     |           |",
        "|||||||  |  |||||||  |  |||||||",
        "      |  |           |  |      ",
        "      |  |           |  |      ",
        "|||||||  |  |||||||  |  |||||||",
        "|              |              |",
        "|              |              |",
        "|  ||||  ||||  |  ||||  ||||  |",
        "|     |                 |     |",
        "|     |                 |     |",
        "||||  |  |  |||||||  |  |  ||||",
        "|        |     |     |        |",
        "|        |     |     |        |",
        "|  ||||||||||  |  ||||||||||  |",
        "|                             |",
        "|                             |",
        "|||||||||||||||||||||||||||||||"
    ]

-- iMazeStr contains array of strings indexed with line no (i,str)
iMazeStr = zip [0..] mazeInString
-- ij contains array of char indexed with line no and char no (i,j, str)
ijMazeStr :: [[(Float, Float, Char)]]
ijMazeStr = map f iMazeStr
    where
        f (i, str) = zipWith (\j chr -> (fromIntegral i, fromIntegral j, chr)) [0..] str

-- converts (i,j,char) -> relevant Picture
charToPic :: (Float, Float, Char) -> Picture
charToPic (i, j, chr) =
    case chr of
        -- use negative index for i to avoid flipping
        -- translate offsets given picture with x,y value
        '|' -> translate (j * (blockSize)) (i*(-blockSize)) wallBlock
        ' ' -> translate (j * (blockSize)) (i*(-blockSize)) $ color white $ rectangleWire blockSize blockSize
        x -> blank -- for spaces and tabs
        where
            wallBlock = color wallColor $ rectangleWire blockSize blockSize

-- array containing only the indices with walls
wallIndex :: [(Float,Float)]
wallIndex = foldr (++) [] [[((-i) * blockSize, j * blockSize) | (i,j,chr) <- str, chr == '|'] | str <- ijMazeStr]
intWallIndex :: [(Int,Int)]
intWallIndex = map (\(x,y) -> (round x, round y)) wallIndex

spaceIndex :: [Vector]
spaceIndex = [(i,j) | (i,j,chr) <- foldedMazeStr, isSpace (i,j)]
    where
        isSpace (i,j) = not $ elem ((-i)*blockSize,j*blockSize) wallIndex
        foldedMazeStr = foldr (++) [] ijMazeStr

-- renderMaze converts [[(i,j,char)]] -> Picture
-- array of char pictures -> line :: Picture using pictures () from gloss
-- array of line pictures -> single picture using pictures () from gloss
renderMaze :: [[(Float, Float, Char)]] -> Picture
renderMaze ijMazeStr = pictures [pictures [charToPic ijchr | ijchr <- line] | line <- ijMazeStr]

mazePicture = renderMaze (ijMazeStr)
