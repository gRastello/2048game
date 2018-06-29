{- ┌───┬───┐  2048                          -
 - │ λ │ 2 │  ----                          -
 - ├───┼───┤  version 0.0.0                 -
 - │ 4 │ λ │  maintainer: Gabriele Rastello -
 - └───┴───┘  license: GPLv3                -}

-- libraries
import UI.NCurses
import Control.Monad.IO.Class

-- import custom modules
import Modules.BoardManipulation
import Modules.Printer

-- game board
board = [ [0, 0, 0, 0]
        , [0, 0, 0, 0]
        , [0, 0, 0, 0]
        , [0, 0, 0, 0]]

-- | The main
main :: IO ()
main = runCurses $ do
    -- disable input printing and cursor visibility
    setEcho False
    setCursorMode CursorInvisible

    -- initialize game board
    newBoard <- liftIO . addNumber $ board

    -- initialize the windows
    borderWindow <- newWindow 14 26 0 0
    gameWindow   <- newWindow 12 24 1 1

    updateWindow borderWindow $ do
        drawBorder Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

    updateWindow gameWindow $ do
        drawBoard newBoard

    render

    -- start game loop
    loop gameWindow newBoard

-- | The game loop
loop :: Window -> Board -> Curses ()
loop gameWindow board = do
    -- show game over if the game is really over
    if checkGameOver board 
        then do updateWindow gameWindow drawGameOver >> render
        else return ()

    -- wait for an event
    e <- getEvent gameWindow Nothing

    if or [checkQuit e, checkGameOver board] then return () else do
        -- check for swiping actions and get a new board accordingly
        newBoard <- if (checkSwipe e $ board) == board then
                        return board
                    else
                        liftIO . addNumber . checkSwipe e $ board

        -- update game window
        updateWindow gameWindow $ do
            drawBoard newBoard

        -- render window
        render

        -- loop again
        loop gameWindow newBoard

-- | Check for a game over
checkGameOver :: Board -> Bool
checkGameOver b = and [ swipeUp    b == b
                      , swipeDown  b == b
                      , swipeLeft  b == b
                      , swipeRight b == b
                      ]

-- | Check for a quit command
checkQuit :: Maybe Event -> Bool
checkQuit (Just (EventCharacter 'q')) = True
checkQuit _ = False

-- | Check for swiping commands
checkSwipe :: Maybe Event -> (Board -> Board)
checkSwipe (Just (EventCharacter 'h')) = swipeLeft
checkSwipe (Just (EventCharacter 'j')) = swipeDown
checkSwipe (Just (EventCharacter 'k')) = swipeUp
checkSwipe (Just (EventCharacter 'l')) = swipeRight
checkSwipe (Just (EventCharacter 'r')) = (\_ -> board)
checkSwipe _ = id
