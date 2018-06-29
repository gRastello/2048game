module Modules.Printer ( drawBoard
                       , drawGameOver
                       ) where

-- import libraries
import UI.NCurses
import Data.List
import qualified Data.Text as T

-- import custom modules
import Modules.BoardManipulation (Board(..))

-- white glyph for clearing purposes
whiteSpace = Glyph ' ' []

-- coordinates of each cell
cellAssociationList = [ (0, 0), (0, 6), (0, 12), ( 0, 18)
                      , (3, 0), (3, 6), (3, 12), ( 3, 18)
                      , (6, 0), (6, 6), (6, 12), ( 6, 18)
                      , (9, 0), (9, 6), (9, 12), ( 9, 18)
                      ]

-- | Print the board to the game window
drawBoard :: Board -> Update ()
drawBoard board = do
    clearWindow 12 24
    drawCells (concat board) cellAssociationList

-- clear the current window
clearWindow :: Integer -> Integer -> Update ()
clearWindow row col = if row /= 0 then do
                          moveCursor (pred row) 0
                          drawLineH (Just whiteSpace) col
                          clearWindow (pred row) col
                      else
                          return ()

-- draws the board cell by cell
drawCells :: [Int] -> [(Integer, Integer)] -> Update ()
drawCells [] [] = return ()
drawCells (x:xs) (y:ys) = do
    drawCell y x
    drawCells xs ys

-- draws a single cell
drawCell :: (Integer, Integer) -> Int -> Update ()
drawCell (y, x) cellContent = do
    moveCursor y x
    drawLineH (Just whiteSpace) 6
    moveCursor (y + 1) x
    drawString cellContent'
    moveCursor (y + 2) x
    drawLineH (Just whiteSpace) 6
        where cellContent' = if cellContent /= 0 then whiteBefore ++ s ++ whiteAfter else "      "
              whiteBefore = take ((6 - length s) `div` 2) $ repeat ' '
              whiteAfter  = take (6 - ((6 - length s) `div` 2) - length s) $ repeat ' '
              s = show cellContent

-- | Draws a game over screen
drawGameOver :: Update ()
drawGameOver = do
    clearWindow 12 24
    moveCursor 5 0
    drawString "          GAME"
    moveCursor 6 0
    drawString "          OVER"

