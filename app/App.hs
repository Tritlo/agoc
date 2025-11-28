{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE LambdaCase #-}

module Main where
import Lib (blipWithFreq, playDiceRollSound, closeWindow, generateDiceSpritesheet, getAnimationFrames, newAnimatedSpriteFromJSArray)
import Graphics.PixiJS
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Control.Monad (when, forM_, void)
import System.Random (randomRIO)

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

    forM_ (zip [0 :: Int ..] menu.menu_items) $ \(idx, item) -> do
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

    void $ addChild container button_text
    return ()

data GameState = GameState {
    gs_score :: Int,
    gs_target :: Int,
    gs_numDice :: Int  -- Number of dice to roll (increases when target is reached)
} deriving (Show, Eq)


-- | Vibrant color palette for dice tinting
vibrantColors :: [Int]
vibrantColors =
    [ 0xFF4444  -- Bright Red
    , 0x44FF44  -- Bright Green
    , 0x4444FF  -- Bright Blue
    , 0xFFFF44  -- Bright Yellow
    , 0xFF44FF  -- Bright Magenta
    , 0x44FFFF  -- Bright Cyan
    , 0xFFA500  -- Orange
    , 0x9932CC  -- Purple
    ]

-- | Play a spinning dice animation using pre-generated spritesheet
-- Uses AnimatedSprite with tinting for color variety
playDiceAnimation :: JSVal -> Application -> Container -> Int -> Int -> Int -> IO () -> IO ()
playDiceAnimation spritesheetCtx _app container screenW screenH finalFace onComplete = do
    -- Random bounce parameters for variety
    bounceFreq <- randomRIO (30.0, 50.0) :: IO Float
    bounceAmp <- randomRIO (6.0, 12.0) :: IO Float

    -- Random start position (around roll button)
    startXOffset <- randomRIO (-50.0, 50.0) :: IO Float
    startYOffset <- randomRIO (-30.0, 30.0) :: IO Float

    -- Random end position (in top half of screen)
    let topHalfHeight = fromIntegral screenH / 2
        halfWidth = fromIntegral screenW / 2
    endXOffset <- randomRIO (-halfWidth * 0.7, halfWidth * 0.7) :: IO Float
    endYOffset <- randomRIO (-topHalfHeight * 0.4, topHalfHeight * 0.4) :: IO Float

    -- Pick random variant (0-2) for visual variety
    variant <- randomRIO (0, 2) :: IO Int

    -- Pick random vibrant color
    colorIndex <- randomRIO (0, length vibrantColors - 1) :: IO Int
    let tintColor = vibrantColors !! colorIndex

    -- Fixed timing
    let rollDuration = 1.5 :: Float    -- seconds (matches spritesheet: 30 frames at 20 FPS)
        totalDuration = 2.0 :: Float   -- seconds (includes 0.5s hold)

    let centerX = fromIntegral screenW / 2
        topHalfY = fromIntegral screenH / 4  -- Center of top half
        buttonY = fromIntegral screenH - 150

        startX = centerX + startXOffset
        startY = buttonY + startYOffset
        endX = centerX + endXOffset
        endY = topHalfY + endYOffset

    -- Get animation frames for this face and variant
    framesArray <- getAnimationFrames spritesheetCtx finalFace variant

    -- Create AnimatedSprite from the frames array
    animSpriteVal <- newAnimatedSpriteFromJSArray framesArray
    let animSprite = fromJSVal animSpriteVal :: Sprite  -- Use Sprite for common operations

    -- Set up the sprite
    setX animSprite startX
    setY animSprite startY
    setAnchor animSprite 0.5 0.5
    setScale animSprite 2.0 2.0  -- Scale up 64px to 128px display size
    setTint animSprite tintColor

    -- Configure animation: 30 frames over 1.5s = 20 FPS animation speed
    -- AnimatedSprite.animationSpeed is frames per tick at 60 FPS
    -- We want 20 FPS playback: 20/60 = 0.333
    let animSpeed = 20.0 / 60.0 :: Float
    setAnimationSpeed (fromJSVal animSpriteVal :: AnimatedSprite) animSpeed
    setLoop (fromJSVal animSpriteVal :: AnimatedSprite) False

    void $ addChild container animSprite

    -- Start the spritesheet animation
    playAnimatedSprite (fromJSVal animSpriteVal :: AnimatedSprite)

    -- Create ticker for position animation and completion detection
    animTicker <- newTicker
    setMaxFPS animTicker 60  -- Fixed 60 FPS for smooth animation

    -- Track elapsed time
    elapsedRef <- newIORef (0.0 :: Float)

    -- Add callback for position/bounce animation
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

            -- Position: ease-out quadratic for movement
            posEased = 1.0 - (1.0 - t) ** 2
            currentX = startX + (endX - startX) * posEased
            currentY = startY + (endY - startY) * posEased

            -- Bounce near end of roll (not during hold)
            bounce = if t > 0.85 && t < 1.0
                     then sin ((t - 0.85) * bounceFreq) * bounceAmp * (1.0 - t) * 6.67
                     else 0

        -- Update position
        setX animSprite currentX
        setY animSprite (currentY + bounce)

        -- End after total duration (2 seconds)
        when (newElapsed >= totalDuration) $ do
            stop ticker
            stopAnimatedSprite (fromJSVal animSpriteVal :: AnimatedSprite)
            void $ removeChild container animSprite
            onComplete

    add animTicker tickerCallback
    start animTicker

-- | Roll the current number of dice (gs_numDice) simultaneously
rollAllDice :: JSVal -> Application -> Container -> Int -> Int
            -> IORef GameState -> Text -> Text -> IO ()
rollAllDice spritesheetCtx app container screenW screenH game_state_ref score_text target_text = do
    GameState{..} <- readIORef game_state_ref
    playDiceRollSound gs_numDice
    -- Track how many dice have completed
    completedRef <- newIORef (0 :: Int)
    -- Roll all dice simultaneously
    forM_ [1..gs_numDice] $ \_ -> do
        finalFace <- randomRIO (1, 6) :: IO Int
        let onComplete = do
                -- Add this die's result to score
                updateScore game_state_ref score_text target_text finalFace
                -- Track completion
                completed <- readIORef completedRef
                writeIORef completedRef (completed + 1)
        playDiceAnimation spritesheetCtx app container screenW screenH finalFace onComplete

-- | Update score after a dice roll. Increments numDice when target is reached.
updateScore :: IORef GameState -> Text -> Text -> Int -> IO ()
updateScore game_state_ref score_text target_text diceResult = do
    game_state@GameState{..} <- readIORef game_state_ref

    let new_score = gs_score + diceResult
    setText score_text (toJSString $ "Score: " ++ show new_score)
    if new_score >= gs_target then do
        blipWithFreq 800.0
        let new_target = gs_target + 10 + gs_numDice * 5  -- Increase target moderately
        let new_numDice = gs_numDice + 1
        setText target_text (toJSString $ "Target: " ++ show new_target)
        writeIORef game_state_ref (game_state { gs_score = 0, gs_target = new_target, gs_numDice = new_numDice })
    else
        writeIORef game_state_ref (game_state { gs_score = new_score })

-- *****************************************************************************
-- * Screen Rendering Functions
-- *****************************************************************************

-- | Render the game screen (the main gameplay)
renderGameScreen :: JSVal -> Application -> Container -> Int -> Int -> IORef GameState
                 -> (IO ())  -- ^ Action to show pause menu
                 -> IO ()
renderGameScreen spritesheetCtx app screenContainer screen_width screen_height game_state_ref showPauseMenu = do
    clearScreen screenContainer

    GameState{..} <- readIORef game_state_ref

    -- Game info in the middle
    target_text <- newTextWithStyle (toJSString $ "Target: " ++ show gs_target) "black"
    setX target_text (fromIntegral screen_width / 2.0)
    setY target_text (fromIntegral screen_height / 2.0 - 60.0)
    setAnchor target_text 0.5 0.5
    void $ addChild screenContainer target_text

    score_text <- newTextWithStyle (toJSString $ "Score: " ++ show gs_score) "black"
    setX score_text (fromIntegral screen_width / 2.0)
    setY score_text (fromIntegral screen_height / 2.0)
    setAnchor score_text 0.5 0.5
    void $ addChild screenContainer score_text

    -- Buttons at the bottom
    let sample_button = Button {
        button_text = "Roll",
        button_x = fromIntegral screen_width / 2.0,
        button_y = fromIntegral screen_height - 150.0,
        button_width = 100.0,
        button_height = 100.0,
        button_color = "black",
        button_on_click = \_ -> rollAllDice spritesheetCtx app screenContainer screen_width screen_height game_state_ref score_text target_text
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
renderPauseMenu _app screenContainer screen_width screen_height continueGame quitGame = do
    clearScreen screenContainer

    -- Title
    title <- newTextWithStyle (toJSString "Paused") "black"
    setX title (fromIntegral screen_width / 2.0)
    setY title 150.0
    setAnchor title 0.5 0.5
    void $ addChild screenContainer title

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
    void $ addChild screenContainer menuContainer
    return ()

-- | Render the start screen with menu
renderStartScreen :: Application -> Container -> Int -> Int
                  -> (IO ())  -- ^ Action to show game screen
                  -> (IO ())  -- ^ Action to show options screen
                  -> IO ()
renderStartScreen _app screenContainer screen_width screen_height showGame showOptions = do
    clearScreen screenContainer

    -- Title
    title <- newTextWithStyle (toJSString "A Game of Chance") "black"
    setX title (fromIntegral screen_width / 2.0)
    setY title 150.0
    setAnchor title 0.5 0.5
    void $ addChild screenContainer title

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
    void $ addChild screenContainer menuContainer
    return ()

-- | Render the options screen
renderOptionsScreen :: Application -> Container -> Int -> Int
                    -> (IO ())  -- ^ Action to go back to start screen
                    -> IO ()
renderOptionsScreen _app screenContainer screen_width screen_height goBack = do
    clearScreen screenContainer

    -- Title
    title <- newTextWithStyle (toJSString "Options") "black"
    setX title (fromIntegral screen_width / 2.0)
    setY title 150.0
    setAnchor title 0.5 0.5
    void $ addChild screenContainer title

    -- Placeholder text (no settings currently)
    noSettings <- newTextWithStyle (toJSString "(No settings available)") "gray"
    setX noSettings (fromIntegral screen_width / 2.0)
    setY noSettings (fromIntegral screen_height / 2.0 - 40.0)
    setAnchor noSettings 0.5 0.5
    void $ addChild screenContainer noSettings

    -- Back menu
    let backMenu = Menu {
        menu_items = [
            MenuItem { menuItem_text = "Back", menuItem_action = goBack }
        ],
        menu_x = fromIntegral screen_width / 2.0,
        menu_y = fromIntegral screen_height / 2.0 + 40.0,
        menu_spacing = 60.0,
        menu_color = "black",
        menu_hoverColor = "blue"
    }

    menuContainer <- renderMenu backMenu
    void $ addChild screenContainer menuContainer
    return ()

-- | Initial game state
initialGameState :: GameState
initialGameState = GameState {
    gs_score = 0,
    gs_target = 15,   -- Reasonable target for ~4 dice rolls on average
    gs_numDice = 1
}

-- | Render a loading screen
renderLoadingScreen :: Container -> Int -> Int -> IO ()
renderLoadingScreen screenContainer screen_width screen_height = do
    clearScreen screenContainer

    -- Loading text
    loadingText <- newTextWithStyle (toJSString "Loading...") "black"
    setX loadingText (fromIntegral screen_width / 2.0)
    setY loadingText (fromIntegral screen_height / 2.0)
    setAnchor loadingText 0.5 0.5
    void $ addChild screenContainer loadingText

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
    void $ addChild stage screenContainer

    -- Show loading screen immediately
    renderLoadingScreen screenContainer screen_width screen_height

    -- Generate dice spritesheet (async operation)
    spritesheetCtx <- generateDiceSpritesheet

    -- Initialize game state
    game_state_ref <- newIORef initialGameState

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
        renderGameScreen spritesheetCtx app screenContainer screen_width screen_height game_state_ref
            showPauseMenu

    writeIORef showPauseMenuRef $
        renderPauseMenu app screenContainer screen_width screen_height
            showGameScreen quitToStart

    -- Transition from loading to start screen
    showStartScreen
