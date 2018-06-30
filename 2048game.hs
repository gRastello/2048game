{- ┌───┬───┐  2048                          -
 - │ λ │ 2 │  ----                          -
 - ├───┼───┤  version 0.2.0                 -
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
    infoWindow   <- newWindow 14 20 0 26

    updateWindow borderWindow $ do
        drawBorder Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
        
    updateWindow infoWindow (drawInfo 0)
        
    updateWindow gameWindow $ do
        drawBoard newBoard

    render

    -- start game loop
    loop gameWindow infoWindow newBoard 0

-- | The game loop
loop :: Window -> Window -> Board -> Int -> Curses ()
loop gameWindow infoWindow board score = do
    -- show game over if the game is really over, show nothing otherwise
    if checkGameOver board 
        then do updateWindow gameWindow drawGameOver >> render
        else return ()


        
    -- wait for an event
    e <- getEvent gameWindow Nothing

    -- go in with the loop only if no quit command or gameover is detected
    if or [checkQuit e, checkGameOver board] then return () else do
        -- check for swiping actions and get a new board accordingly
        newBoard <- if (fst . checkSwipe e $ board) == board then
                        return board
                    else
                        liftIO . addNumber . fst . checkSwipe e $ board

        -- update score
        let newScore = score + (snd . checkSwipe e $ board)

        -- update game window
        updateWindow gameWindow $ do
            drawBoard newBoard
            
        -- updae the info window
        updateWindow infoWindow (drawInfo newScore)
            
        -- render window
        render

        -- loop again
        loop gameWindow infoWindow newBoard newScore

-- | Check for a game over
checkGameOver :: Board -> Bool
checkGameOver b = and [ (fst . swipeUp    $ b) == b
                      , (fst . swipeDown  $ b) == b
                      , (fst . swipeLeft  $ b) == b
                      , (fst . swipeRight $ )b == b
                      ]

-- | Check for a quit command
checkQuit :: Maybe Event -> Bool
checkQuit (Just (EventCharacter 'q')) = True
checkQuit _ = False

-- | Check for commands that modify the board
checkSwipe :: Maybe Event -> (Board -> (Board, Int))
checkSwipe (Just (EventCharacter 'h')) = swipeLeft
checkSwipe (Just (EventCharacter 'j')) = swipeDown
checkSwipe (Just (EventCharacter 'k')) = swipeUp
checkSwipe (Just (EventCharacter 'l')) = swipeRight
checkSwipe (Just (EventCharacter 'r')) = (\_ -> (board, 0)) -- reinitialize the board
checkSwipe _ = (\b -> (b, 0))
