module Modules.BoardManipulation ( Board(..)
                                 , swipeRight
                                 , swipeLeft
                                 , swipeUp
                                 , swipeDown
                                 , addNumber ) where

-- libraries
import Data.List
import System.Random

-- | The 2048 board.
-- It is defined as a [[Int]] but we'll always use a 4x4 board (there's even a
-- 4 hardcoded in the swipeRightRow function).
type Board = [[Int]]

-- | Calculate the board after a right swipe.
-- This function is used as a base for all the other swipes.
swipeRight :: Board -> Board
swipeRight = map swipeRightRow

swipeRightRow :: [Int] -> [Int]
swipeRightRow list = swipeRightRow' list''
    where list'   = list \\ [0, 0, 0, 0] -- removes zeroes from the Row
          list''  = take (4 - length list') (repeat 0) ++ list' -- add zeroes in front of the Row

swipeRightRow' :: [Int] -> [Int]
swipeRightRow' [a, b, c, d]
    | and [a == b, c == d] = [0,   0, a+b, c+d]
    | and [a /= b, c == d] = [0,   a,   b, c+d]
    | b == c               = [0,   a, b+c,   d]
    | and [a == b, c /= d] = [0, a+b,   c,   d]
    | otherwise            = [a,   b,   c,   d]

-- | Rotate the board 90 degree anti-clockwise.
-- This function is used together with swipeRight to define all the other swipes.
rotate :: Board -> Board
rotate [[a], [b], [c], [d]] = [[a, b, c, d]]
rotate [(a:as), (b:bs), (c:cs), (d:ds)] = rotate [as, bs, cs, ds] ++ [[a, b, c, d]]

-- | Calculate the board after a down swipe.
swipeDown :: Board -> Board
swipeDown = rotate . rotate . rotate . swipeRight . rotate

-- | Calculate the board after an up swipe
swipeUp :: Board -> Board
swipeUp = rotate . swipeRight . rotate . rotate . rotate

-- | Calculate the board after a left swipe.
swipeLeft :: Board -> Board
swipeLeft = rotate . rotate . swipeRight . rotate . rotate

-- | Add random number (2 or 4) to the board
addNumber :: Board -> IO Board
addNumber b = do
    n <- newStdGen >>= (return . fst . randomR (0, 15)) :: IO Int
    if concat b !! n == 0
        then do
            newNumber <- newStdGen >>= (return . (*2) . fst . randomR (1, 2)) :: IO Int
            return . unconcat . replaceAtIndex newNumber n . concat $ b
        else addNumber b

-- inverse of concat for a 4x4 board
unconcat :: [a] -> [[a]]
unconcat [] = []
unconcat (a:b:c:d:xs) = [a, b, c, d] : unconcat xs

-- replace an index
replaceAtIndex :: a -> Int -> [a] -> [a]
replaceAtIndex newElem 0 list = newElem : tail list
replaceAtIndex newElem n list = head list : replaceAtIndex newElem (pred n) (tail list)
