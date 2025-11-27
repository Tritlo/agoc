{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE LambdaCase #-}

module Main where
import Lib (blipWithFreq, histogram_plot, HistogramOptions(..), defaultHistogramOptions, closeWindow, initDiceRenderer, renderDiceFrame, acquireDiceSlot, releaseDiceSlot, getDiceSlotTexture)
import Graphics.PixiJS
import GHC.Wasm.Prim
import Data.String (IsString(..))
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Control.Monad (when, forM_)
import qualified System.Random.SplitMix.Distributions as D
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Aeson (ToJSON(..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.List (partition)
import System.Random (randomRIO)
import Numeric (showHex)

-- Export the actual initialization function
foreign export javascript "main" main :: IO ()

-- *****************************************************************************
-- * Menu System
-- *****************************************************************************

-- | A menu item with text and an action to perform when clicked
data MenuItem = MenuItem {
    menuItem_text :: String,
    menuItem_action :: IO ()
}

-- | A menu is a list of menu items with positioning info
data Menu = Menu {
    menu_items :: [MenuItem],
    menu_x :: Float,
    menu_y :: Float,
    menu_spacing :: Float,  -- vertical spacing between items
    menu_color :: String,
    menu_hoverColor :: String
}

-- | Render a menu and return the container holding it
renderMenu :: Menu -> IO Container
renderMenu menu = do
    container <- newContainer
    setX container menu.menu_x
    setY container menu.menu_y

    forM_ (zip [0..] menu.menu_items) $ \(idx, item) -> do
        text_obj <- newTextWithStyle (toJSString item.menuItem_text) (toJSString menu.menu_color)
        setEventMode text_obj "static"
        setCursor text_obj "pointer"
        setAnchor text_obj 0.5 0.5
        setX text_obj 0.0
        setY text_obj (fromIntegral idx * menu.menu_spacing)

        -- Click handler
        on "pointerdown" text_obj =<< jsFuncFromHs_ (\_ -> item.menuItem_action)

        -- Hover effect - change color
        on "pointerover" text_obj =<< jsFuncFromHs_
             (\_ -> setPropertyKey ["style", "fill"] (toJSVal text_obj)
                        (stringAsVal $ toJSString menu.menu_hoverColor))
        on "pointerout" text_obj =<< jsFuncFromHs_
             (\_ -> setPropertyKey ["style", "fill"] (toJSVal text_obj)
                        (stringAsVal $ toJSString menu.menu_color))

        addChild container text_obj

    return container

-- *****************************************************************************
-- * Screen Management
-- *****************************************************************************

-- | Different screens in the game
data Screen = StartScreen | OptionsScreen | GameScreen
    deriving (Eq, Show)

-- | Global app state for screen management
data AppState = AppState {
    app_currentScreen :: Screen,
    app_screenContainer :: Container  -- The container holding current screen contents
}

-- | Clear the current screen container
clearScreen :: Container -> IO ()
clearScreen container = do
    numChildren <- getNumChildren container
    -- Remove children from end to start to avoid index issues
    forM_ (reverse [0..numChildren-1]) $ \_ -> do
        removeChildAt container 0
    return ()

-- *****************************************************************************
-- * Button (legacy, kept for game screen)
-- *****************************************************************************

data Button = Button {
    button_text :: String,
    button_x :: Float,
    button_y :: Float,
    button_width :: Float,
    button_height :: Float,
    button_color :: String,
    button_on_click :: JSVal -> IO ()
}

renderButton :: Container -> Button -> IO ()
renderButton container button = do
    button_text <- newTextWithStyle (toJSString button.button_text) (toJSString button.button_color)
    setEventMode button_text "static"
    setAnchor button_text 0.5 0.5
    setX button_text button.button_x
    setY button_text button.button_y
    on "pointerdown" button_text =<< jsFuncFromHs_ button.button_on_click

    on "pointerover" button_text =<< jsFuncFromHs_
         (\_ -> do setPropertyKey ["style", "fill"] (toJSVal button_text) (stringAsVal "blue"))
    on "pointerout" button_text =<< jsFuncFromHs_
         (\_ -> do setPropertyKey ["style", "fill"] (toJSVal button_text) (stringAsVal "black"))

    _ <- addChild container button_text
    return ()

data GameState = GameState {
    gs_score :: Integer,
    gs_dists :: Seq Dist,
    gs_target :: Integer
} deriving (Show, Eq)


data Dist = Uniform {low :: Double, high :: Double}
          | Exponential {lambda :: Double}
          | Gamma {k :: Double, theta :: Double}
          | Normal {mean :: Double, stddev :: Double}
  deriving (Eq, Ord)

instance Show Dist where
    show (Uniform a b) = "𝒰([" ++ show a ++ ", " ++ show b ++ "])"
    show (Exponential lambda) = "Exp(λ=" ++ show lambda ++ ")"
    show (Gamma k theta) = "Γ(k=" ++ show k ++ ", θ=" ++ show theta ++ ")"
    show (Normal mean stddev) = "𝒩(μ=" ++ show mean ++ ", σ²=" ++ show stddev ++ ")"


validateDist :: Dist -> Bool
validateDist (Uniform a b) = a < b
validateDist (Exponential lambda) = lambda > 0
validateDist (Gamma k theta) = k > 0 && theta > 0
validateDist (Normal mean stddev) = stddev > 0


sampleDist :: Monad m => Dist -> D.GenT m Double
sampleDist (Uniform low high) = D.uniformR low high
sampleDist (Exponential theta) = D.exponential theta
sampleDist (Gamma k theta) = D.gamma k theta
sampleDist (Normal mean stddev) = D.normal mean stddev

sampleDists :: Monad m => Seq Dist -> D.GenT m Double
sampleDists dists = do
    case Seq.viewl dists of
        Seq.EmptyL -> return 0.0
        first Seq.:< rest -> do
            x <- sampleDist first
            xs <- sampleDists rest
            return (x + xs)


showDists :: Seq Dist -> String
showDists dists = case Seq.viewl dists of
    Seq.EmptyL -> ""
    first Seq.:< rest -> show first ++ foldMap (\dist -> " + " ++ show dist) rest


poisson :: Monad m => Double -> D.GenT m Int
poisson lambda = go 0 0
  where go !t !k = do
          t' <- (t +) <$> D.exponential lambda
          if t' > 1
          then return k
          else go t' (k + 1)

newtype Histogram = Histogram [(Int, Double)]

instance ToJSON Histogram where
    toJSON (Histogram bins) = toJSON bins

histogram :: Int -> Int -> Seq Dist -> IO Histogram
histogram num_samples bins dists = do
    samples <- D.samplesIO num_samples $ sampleDists dists
    let (h_min, h_max) = (minimum samples, maximum samples)
    let bin_width = (h_max - h_min) / fromIntegral bins
    let bins' :: Double -> Double -> [Double] -> [(Int, Double)]
        bins' !acc !h_cutoff [] = []
        bins' !acc !h_cutoff !samples =
            let (lt, rest) = partition (<= h_cutoff) samples
                len_lt = length lt
                acc' = acc + (fromIntegral len_lt)
                h_cutoff' = h_cutoff + bin_width
            in  (len_lt, h_cutoff) : bins' acc' h_cutoff' rest
        bins = bins' 0.0 (h_min  + bin_width) samples
    return (Histogram bins)



-- | Todo: here we should simplify the distributions by combining like terms,
-- e.g. N(500,100) + N(500,100) -> N(1000,200)
simplifyDists :: Seq Dist -> Seq Dist
simplifyDists dists = dists

histogram_options :: HistogramOptions
histogram_options = defaultHistogramOptions {
    ho_width = 300,
    ho_height = 150,
    ho_fillColor = "black",
    ho_backgroundColor = "white",
    ho_marginLeft = 35,
    ho_marginRight = 10,
    ho_marginTop = 10,
    ho_marginBottom = 25
}

-- | Helper to convert Int to 2-digit hex string
toHex :: Int -> String
toHex n = let h = showHex n "" in if length h == 1 then '0':h else h

-- | Play a spinning dice animation with randomized trajectory
-- Uses time-based animation (2 seconds total) with exact final face landing
-- Each animation gets its own slot in the spritesheet for concurrent animations
playDiceAnimation :: JSVal -> Application -> Container -> Int -> Int -> IO () -> IO ()
playDiceAnimation diceRenderer _app container screenW screenH onComplete = do
    -- Acquire a slot for this animation
    slotIndex <- acquireDiceSlot diceRenderer

    -- Generate random final face (1-6)
    finalFace <- randomRIO (1, 6) :: IO Int

    -- Random bounce parameters for variety
    bounceFreq <- randomRIO (30.0, 50.0) :: IO Float
    bounceAmp <- randomRIO (6.0, 12.0) :: IO Float

    -- Random start position (around sample button)
    startXOffset <- randomRIO (-50.0, 50.0) :: IO Float
    startYOffset <- randomRIO (-30.0, 30.0) :: IO Float

    -- Random end position (around center)
    endXOffset <- randomRIO (-60.0, 60.0) :: IO Float
    endYOffset <- randomRIO (-40.0, 40.0) :: IO Float

    -- Random number of full rotations per axis (1-6)
    numRotsX <- randomRIO (1, 6) :: IO Int
    numRotsY <- randomRIO (1, 6) :: IO Int
    numRotsZ <- randomRIO (1, 6) :: IO Int

    -- Muted colors: moderate contrast between channels
    colorType <- randomRIO (0, 5) :: IO Int
    (r, g, b) <- case colorType of
        0 -> do  -- Muted Red
            r' <- randomRIO (180, 220) :: IO Int
            g' <- randomRIO (100, 140) :: IO Int
            b' <- randomRIO (100, 140) :: IO Int
            return (r', g', b')
        1 -> do  -- Muted Green
            r' <- randomRIO (100, 140) :: IO Int
            g' <- randomRIO (180, 220) :: IO Int
            b' <- randomRIO (100, 140) :: IO Int
            return (r', g', b')
        2 -> do  -- Muted Blue
            r' <- randomRIO (100, 140) :: IO Int
            g' <- randomRIO (100, 140) :: IO Int
            b' <- randomRIO (180, 220) :: IO Int
            return (r', g', b')
        3 -> do  -- Muted Yellow (red + green)
            r' <- randomRIO (180, 220) :: IO Int
            g' <- randomRIO (180, 220) :: IO Int
            b' <- randomRIO (100, 140) :: IO Int
            return (r', g', b')
        4 -> do  -- Muted Magenta (red + blue)
            r' <- randomRIO (180, 220) :: IO Int
            g' <- randomRIO (100, 140) :: IO Int
            b' <- randomRIO (180, 220) :: IO Int
            return (r', g', b')
        _ -> do  -- Muted Cyan (green + blue)
            r' <- randomRIO (100, 140) :: IO Int
            g' <- randomRIO (180, 220) :: IO Int
            b' <- randomRIO (180, 220) :: IO Int
            return (r', g', b')
    let diceColor = fromString $ "0x" ++ toHex r ++ toHex g ++ toHex b

    -- Fixed timing (2 seconds total)
    let rollDuration = 1.5 :: Float    -- seconds
        totalDuration = 2.0 :: Float   -- seconds (includes 0.5s hold)

    let centerX = fromIntegral screenW / 2
        centerY = fromIntegral screenH / 2
        buttonY = fromIntegral screenH - 150

        startX = centerX + startXOffset
        startY = buttonY + startYOffset
        endX = centerX + endXOffset
        endY = centerY + endYOffset

        -- Target rotation to land on specific face
        -- BoxGeometry face order: +X(3), -X(4), +Y(1), -Y(6), +Z(2), -Z(5)
        -- At rotation (0,0,0): face 1 is on +Y (top), face 6 on -Y (bottom)
        -- Camera is at (0, 5, 0) looking at origin, so +Y faces camera
        -- We want the target face to be on +Y after rotation
        (faceRotX, faceRotY, faceRotZ) = case finalFace of
            1 -> (0, 0, 0)           -- Face 1 already on +Y
            6 -> (pi, 0, 0)          -- Flip around X to bring -Y to +Y
            2 -> (-pi/2, 0, 0)       -- Face 2 on +Z, rotate -90° around X
            5 -> (pi/2, 0, 0)        -- Face 5 on -Z, rotate +90° around X
            3 -> (0, 0, pi/2)        -- Face 3 on +X, rotate +90° around Z
            _ -> (0, 0, -pi/2)       -- Face 4 on -X, rotate -90° around Z

        -- Total rotation = full rotations + face offset
        -- This guarantees we land exactly on the target face
        totalRotX = fromIntegral numRotsX * 2 * pi + faceRotX
        totalRotY = fromIntegral numRotsY * 2 * pi + faceRotY
        totalRotZ = fromIntegral numRotsZ * 2 * pi + faceRotZ

    -- Get a texture for this slot and create sprite
    slotTextureVal <- getDiceSlotTexture diceRenderer slotIndex
    let slotTexture = fromJSVal slotTextureVal :: Texture
    diceSprite <- newSpriteFromTexture slotTexture
    setX diceSprite startX
    setY diceSprite startY
    setAnchor diceSprite 0.5 0.5
    _ <- addChild container diceSprite

    -- Do initial render to the slot
    renderDiceFrame diceRenderer slotIndex 0 0 0 diceColor 6

    -- Create dedicated ticker for this animation
    animTicker <- newTicker
    setMaxFPS animTicker 60

    -- Track elapsed time
    elapsedRef <- newIORef (0.0 :: Float)

    -- Add callback using time-based animation
    tickerCallback <- jsFuncFromHs_ $ \tickerVal -> do
        let ticker = fromJSVal tickerVal :: Ticker

        -- Get delta time from ticker (in ms, convert to seconds)
        deltaMs <- getDeltaMS ticker
        let deltaS = deltaMs / 1000.0

        elapsed <- readIORef elapsedRef
        let newElapsed = elapsed + deltaS
        writeIORef elapsedRef newElapsed

        -- t is progress through roll phase (0 to 1), clamped at 1
        let t = min 1.0 (newElapsed / rollDuration) :: Float

            -- Simple linear interpolation: at t=1.0, we're exactly at totalRot
            -- Use ease-out for visual appeal (fast start, slow end)
            tEased = if t >= 1.0 then 1.0 else 1.0 - (1.0 - t) ** 3

            -- Current rotation = progress * total rotation
            rotX = tEased * totalRotX
            rotY = tEased * totalRotY
            rotZ = tEased * totalRotZ

            -- Position: ease-out quadratic for movement
            posEased = 1.0 - (1.0 - t) ** 2
            currentX = startX + (endX - startX) * posEased
            currentY = startY + (endY - startY) * posEased

            -- Bounce near end of roll (not during hold)
            bounce = if t > 0.85 && t < 1.0
                     then sin ((t - 0.85) * bounceFreq) * bounceAmp * (1.0 - t) * 6.67
                     else 0

        -- Update dice in this slot
        renderDiceFrame diceRenderer slotIndex rotX rotY rotZ diceColor 6
        setX diceSprite currentX
        setY diceSprite (currentY + bounce)

        -- End after total duration (2 seconds)
        when (newElapsed >= totalDuration) $ do
            stop ticker
            _ <- removeChild container diceSprite
            releaseDiceSlot diceRenderer slotIndex
            onComplete

    add animTicker tickerCallback
    start animTicker

updateScore :: IORef GameState -> Text -> Text -> Text -> Sprite -> IO ()
updateScore game_state_ref score_text target_text distr_text histogram_sprite = do
    game_state@GameState{..} <- readIORef game_state_ref
    sample <- D.sampleIO $ sampleDists gs_dists

    let new_score = gs_score + round sample
    setText score_text (toJSString $ "Score: " ++ show new_score)
    if new_score >= gs_target then do
        blipWithFreq 800.0
        let new_target = gs_target * 2
        let new_dists = gs_dists :|> Normal 500.0 100.0
        setText target_text (toJSString $ "Target: " ++ show new_target)
        setText distr_text (toJSString $ "X ~ " ++ showDists new_dists)

        Histogram histogram <- histogram 10_000 100 new_dists
        histogram_data <- parseJSON (toJSString $ BSC.unpack $ Aeson.encode histogram)
        histogram_texture_jsval <- histogram_plot histogram_data histogram_options
        setProperty "texture" (toJSVal histogram_sprite) histogram_texture_jsval

        writeIORef game_state_ref (game_state { gs_score = 0, gs_target = new_target, gs_dists = new_dists })
    else
        writeIORef game_state_ref (game_state { gs_score = new_score })

-- *****************************************************************************
-- * Screen Rendering Functions
-- *****************************************************************************

-- | Render the game screen (the main gameplay)
renderGameScreen :: JSVal -> Application -> Container -> Int -> Int -> IORef GameState
                 -> (IO ())  -- ^ Action to show pause menu
                 -> IO ()
renderGameScreen diceRenderer app screenContainer screen_width screen_height game_state_ref showPauseMenu = do
    clearScreen screenContainer

    GameState{..} <- readIORef game_state_ref

    -- Histogram at the top (smaller now)
    Histogram hist <- histogram 10_000 100 gs_dists
    histogram_data <- parseJSON (toJSString $ BSC.unpack $ Aeson.encode hist)
    histogram_texture_jsval <- histogram_plot histogram_data histogram_options
    let histogram_texture = fromJSVal histogram_texture_jsval :: Texture
    histogram_sprite <- newSpriteFromTexture histogram_texture
    setX histogram_sprite (fromIntegral screen_width / 2.0)
    setY histogram_sprite 100.0
    setAnchor histogram_sprite 0.5 0.5
    addChild screenContainer histogram_sprite

    -- Game info in the middle
    target_text <- newTextWithStyle (toJSString $ "Target: " ++ show gs_target) "black"
    setX target_text (fromIntegral screen_width / 2.0)
    setY target_text (fromIntegral screen_height / 2.0 - 60.0)
    setAnchor target_text 0.5 0.5
    addChild screenContainer target_text

    score_text <- newTextWithStyle (toJSString $ "Score: " ++ show gs_score) "black"
    setX score_text (fromIntegral screen_width / 2.0)
    setY score_text (fromIntegral screen_height / 2.0)
    setAnchor score_text 0.5 0.5
    addChild screenContainer score_text

    distr_text <- newTextWithStyle (toJSString $ "X ∼ " ++ showDists gs_dists) "black"
    setX distr_text (fromIntegral screen_width / 2.0)
    setY distr_text (fromIntegral screen_height / 2.0 + 60.0)
    setAnchor distr_text 0.5 0.5
    addChild screenContainer distr_text

    -- Buttons at the bottom
    let sample_button = Button {
        button_text = "Sample",
        button_x = fromIntegral screen_width / 2.0,
        button_y = fromIntegral screen_height - 150.0,
        button_width = 100.0,
        button_height = 100.0,
        button_color = "black",
        button_on_click =
             \_ -> do
                let onDiceComplete =
                        updateScore game_state_ref score_text target_text distr_text histogram_sprite
                playDiceAnimation diceRenderer app screenContainer screen_width screen_height onDiceComplete
    }
    renderButton screenContainer sample_button

    let menu_button = Button {
        button_text = "Menu",
        button_x = fromIntegral screen_width / 2.0,
        button_y = fromIntegral screen_height - 80.0,
        button_width = 100.0,
        button_height = 50.0,
        button_color = "black",
        button_on_click = \_ -> showPauseMenu
    }
    renderButton screenContainer menu_button

-- | Render the pause menu screen
renderPauseMenu :: Application -> Container -> Int -> Int
                -> (IO ())  -- ^ Action to continue game
                -> (IO ())  -- ^ Action to quit to start screen
                -> IO ()
renderPauseMenu app screenContainer screen_width screen_height continueGame quitGame = do
    clearScreen screenContainer

    -- Title
    title <- newTextWithStyle (toJSString "Paused") "black"
    setX title (fromIntegral screen_width / 2.0)
    setY title 150.0
    setAnchor title 0.5 0.5
    addChild screenContainer title

    -- Menu
    let pauseMenu = Menu {
        menu_items = [
            MenuItem { menuItem_text = "Continue", menuItem_action = continueGame },
            MenuItem { menuItem_text = "Quit", menuItem_action = quitGame }
        ],
        menu_x = fromIntegral screen_width / 2.0,
        menu_y = fromIntegral screen_height / 2.0,
        menu_spacing = 60.0,
        menu_color = "black",
        menu_hoverColor = "blue"
    }

    menuContainer <- renderMenu pauseMenu
    _ <- addChild screenContainer menuContainer
    return ()

-- | Render the start screen with menu
renderStartScreen :: Application -> Container -> Int -> Int
                  -> (IO ())  -- ^ Action to show game screen
                  -> (IO ())  -- ^ Action to show options screen
                  -> IO ()
renderStartScreen app screenContainer screen_width screen_height showGame showOptions = do
    clearScreen screenContainer

    -- Title
    title <- newTextWithStyle (toJSString "A Game of Chance") "black"
    setX title (fromIntegral screen_width / 2.0)
    setY title 150.0
    setAnchor title 0.5 0.5
    addChild screenContainer title

    -- Menu
    let startMenu = Menu {
        menu_items = [
            MenuItem { menuItem_text = "Start Game", menuItem_action = showGame },
            MenuItem { menuItem_text = "Options", menuItem_action = showOptions },
            MenuItem { menuItem_text = "Exit", menuItem_action = closeWindow }
        ],
        menu_x = fromIntegral screen_width / 2.0,
        menu_y = fromIntegral screen_height / 2.0,
        menu_spacing = 60.0,
        menu_color = "black",
        menu_hoverColor = "blue"
    }

    menuContainer <- renderMenu startMenu
    _ <- addChild screenContainer menuContainer
    return ()

-- | Render the options screen
renderOptionsScreen :: Application -> Container -> Int -> Int
                    -> (IO ())  -- ^ Action to go back to start screen
                    -> IO ()
renderOptionsScreen app screenContainer screen_width screen_height goBack = do
    clearScreen screenContainer

    -- Title
    title <- newTextWithStyle (toJSString "Options") "black"
    setX title (fromIntegral screen_width / 2.0)
    setY title 150.0
    setAnchor title 0.5 0.5
    addChild screenContainer title

    -- Placeholder text
    placeholder <- newTextWithStyle (toJSString "(No options yet)") "gray"
    setX placeholder (fromIntegral screen_width / 2.0)
    setY placeholder (fromIntegral screen_height / 2.0 - 50.0)
    setAnchor placeholder 0.5 0.5
    addChild screenContainer placeholder

    -- Back menu
    let backMenu = Menu {
        menu_items = [
            MenuItem { menuItem_text = "Back", menuItem_action = goBack }
        ],
        menu_x = fromIntegral screen_width / 2.0,
        menu_y = fromIntegral screen_height / 2.0 + 50.0,
        menu_spacing = 60.0,
        menu_color = "black",
        menu_hoverColor = "blue"
    }

    menuContainer <- renderMenu backMenu
    _ <- addChild screenContainer menuContainer
    return ()

-- | Initial game state
initialGameState :: GameState
initialGameState = GameState {
    gs_score = 0,
    gs_dists = Seq.fromList [Normal 500.0 100.0],
    gs_target = 10_000
}

main :: IO ()
main = do
    -- Initialize PIXI application
    app <- newApp >>= flip initApp "white"
    appendCanvas app
    screen <- getScreen app
    screen_width <- round <$> getRectWidth screen
    screen_height <- round <$> getRectHeight screen

    -- Create screen container that will hold all screen content
    screenContainer <- newContainer
    stage <- getStage app
    addChild stage screenContainer

    -- Initialize game state
    game_state_ref <- newIORef initialGameState

    -- Initialize persistent dice renderer (single WebGL context for all dice animations)
    diceRenderer <- initDiceRenderer 128

    -- Define screen transition functions using mutual recursion via IORefs
    showStartScreenRef <- newIORef (return () :: IO ())
    showOptionsScreenRef <- newIORef (return () :: IO ())
    showGameScreenRef <- newIORef (return () :: IO ())
    showPauseMenuRef <- newIORef (return () :: IO ())

    let showStartScreen = do
            action <- readIORef showStartScreenRef
            action

    let showOptionsScreen = do
            action <- readIORef showOptionsScreenRef
            action

    let showGameScreen = do
            action <- readIORef showGameScreenRef
            action

    let showPauseMenu = do
            action <- readIORef showPauseMenuRef
            action

    -- Reset game state and go to start screen
    let quitToStart = do
            writeIORef game_state_ref initialGameState
            showStartScreen

    -- Set up the actual screen rendering functions
    writeIORef showStartScreenRef $
        renderStartScreen app screenContainer screen_width screen_height
            showGameScreen showOptionsScreen

    writeIORef showOptionsScreenRef $
        renderOptionsScreen app screenContainer screen_width screen_height
            showStartScreen

    writeIORef showGameScreenRef $
        renderGameScreen diceRenderer app screenContainer screen_width screen_height game_state_ref
            showPauseMenu

    writeIORef showPauseMenuRef $
        renderPauseMenu app screenContainer screen_width screen_height
            showGameScreen quitToStart

    -- Start at the start screen
    showStartScreen
