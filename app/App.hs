{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}

module Main where
import Lib
import GHC.Wasm.Prim
import Data.String (IsString(..))
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Control.Monad (when)
import qualified System.Random.SplitMix.Distributions as D

-- Export the actual initialization function
foreign export javascript "main" main :: IO ()

data Button = Button {
    button_text :: String,
    button_x :: Float,
    button_y :: Float,
    button_width :: Float,
    button_height :: Float,
    button_color :: String,
    button_on_click :: JSVal -> IO ()
}

renderButton :: PixiApp -> Button -> IO ()
renderButton app button = do
    button_text <- newText (toJSString button.button_text) (toJSString button.button_color)
    setProperty "eventMode" button_text (stringAsVal "static")
    setAnchor button_text 0.5
    setProperty "x" button_text (floatAsVal button.button_x)
    setProperty "y" button_text (floatAsVal button.button_y)
    addEventListener "pointerdown" button_text =<< jsFuncFromHs_ button.button_on_click
    addChild app button_text

data GameState = GameState {
    gs_score :: Integer,
    gs_mean :: Double,
    gs_stddev :: Double
} deriving (Show, Eq)

updateScore :: IORef GameState -> PixiText -> IO ()
updateScore game_state_ref score_text = do
    game_state@GameState{..} <- readIORef game_state_ref
    sample <- D.sampleIO $
         D.normal gs_mean gs_stddev
    let new_score = gs_score + round sample
    consoleLogShow $ "New score: " ++ show new_score
    setProperty "text" score_text (stringAsVal $ toJSString $ show new_score)
    writeIORef game_state_ref (game_state { gs_score = new_score })

main :: IO ()
main = do
    -- Initialize PIXI application
    app <- newApp
    app <- initAppInTarget app "black" "#canvas-container"
    appendToTarget "#canvas-container" app
    screen <- getProperty "screen" app
    screen_width <- valAsInt <$> getProperty "width" screen
    screen_height <- valAsInt <$> getProperty "height" screen
    game_state_ref <- newIORef GameState {
                             gs_score = 0,
                             gs_mean = 500.0,
                             gs_stddev = 100.0
                         }
    score_text <- newText (toJSString "0") "white"
    setProperty "x" score_text (floatAsVal $ fromIntegral screen_width / 2.0)
    setProperty "y" score_text (floatAsVal $ fromIntegral screen_height / 2.0)
    setAnchor score_text 0.5
    addChild app score_text

    let sample_button = Button {
        button_text = "Sample",
        button_x = fromIntegral screen_width / 2.0,
        button_y = fromIntegral screen_height - 200.0,
        button_width = 100.0,
        button_height = 100.0,
        button_color = "white",
        button_on_click =
             \event ->
                updateScore game_state_ref score_text
    }
    consoleLogShow "rendering button"
    renderButton app sample_button
    return ()
