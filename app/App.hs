{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}

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

    background <- baseTexture "WHITE" >>= newSprite
    setAnchor background 0.5
    setProperty "x" background (floatAsVal button.button_x)
    setProperty "y" background (floatAsVal button.button_y)
    bounds <- callMethod button_text "getLocalBounds"
    t_width <- valAsFloat <$> getProperty "width" bounds
    t_height <- valAsFloat <$> getProperty "height" bounds
    consoleLogShow $ "t_width: " ++ show t_width
    consoleLogShow $ "t_height: " ++ show t_height
    setProperty "width" background (floatAsVal $ t_width * 1.1)
    setProperty "height" background (floatAsVal $ t_height * 1.1)
    setProperty "alpha" background (floatAsVal 0.2)
    setProperty "visible" background (boolAsVal False)

    addEventListener "pointerover" button_text =<< jsFuncFromHs_
         (\_ -> do setProperty "visible" background (boolAsVal True))
    addEventListener "pointerout" button_text =<< jsFuncFromHs_
         (\_ -> do setProperty "visible" background (boolAsVal False))

    

    addChild app background
    addChild app button_text

data GameState = GameState {
    gs_score :: Integer,
    gs_mean :: Double,
    gs_stddev :: Double,
    gs_target :: Integer
} deriving (Show, Eq)

updateScore :: IORef GameState -> PixiText -> PixiText -> IO ()
updateScore game_state_ref score_text target_text = do
    game_state@GameState{..} <- readIORef game_state_ref
    sample <- D.sampleIO $
         D.normal gs_mean gs_stddev
    let new_score = gs_score + round sample
    setProperty "text" score_text (stringAsVal $ toJSString $ show new_score)
    if new_score >= gs_target then do
        consoleLogShow "You win!"
        blipWithFreq 800.0
        let new_target = gs_target * 2
            new_mean = gs_mean * 2
            new_stddev = gs_stddev * 2
            new_score = 0
        setProperty "text" target_text (stringAsVal $ toJSString $ show $ gs_target * 2)
        writeIORef game_state_ref (game_state { gs_target = new_target, gs_mean = new_mean, gs_stddev = new_stddev, gs_score = new_score })
    else
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
                             gs_stddev = 100.0,
                             gs_target = 10_000
                         }
    target_text <- newText (toJSString "10000") "white"
    setProperty "x" target_text (floatAsVal $ fromIntegral screen_width / 2.0)
    setProperty "y" target_text (floatAsVal $ (fromIntegral screen_height / 2.0) - 100.0)
    setAnchor target_text 0.5
    addChild app target_text

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
                updateScore game_state_ref score_text target_text
    }
    consoleLogShow "rendering button"
    renderButton app sample_button
    return ()
