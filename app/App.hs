{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE LambdaCase #-}

module Main where
import Lib (blipWithFreq, playDiceRollSound, closeWindow, generateDiceSpritesheet, getAnimationFrames, newAnimatedSpriteFromJSArray)
import Graphics.PixiJS
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef, IORef)
import Control.Monad (when, forM_, void)
import System.Random (randomRIO)
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)

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

-- | Type of die: additive contributes to X, multiplicative to Y in X*Y scoring
data DiceType = Additive | Multiplicative
    deriving (Show, Eq, Ord)

-- | Result of a single die roll, tracking type and value
data DieResult = DieResult {
    dr_type   :: DiceType,
    dr_value  :: Int,
    dr_sprite :: JSVal
}

-- | Current roll state for tracking dice during animation
data RollState = RollState {
    rs_pending     :: Int,          -- Number of dice still animating
    rs_newResults  :: [DieResult],  -- Results from current batch (for combo detection)
    rs_prevResults :: [DieResult],  -- Results from previous batches (not checked for combos)
    rs_isComboRoll :: Bool          -- Whether this is a combo bonus roll
}

-- | Main game state
data GameState = GameState {
    gs_score              :: Int,       -- Current accumulated score
    gs_target             :: Int,       -- Target to reach
    gs_additiveDice       :: Int,       -- Number of additive dice
    gs_multiplicativeDice :: Int,       -- Number of multiplicative dice
    gs_persistentSprites  :: [JSVal],   -- Sprites from last roll (to clear on next roll)
    gs_rollState          :: Maybe RollState,  -- Active roll tracking (Nothing when idle)
    gs_round              :: Int        -- Current round number
}

-- | Initial game state
initialGameState :: GameState
initialGameState = GameState {
    gs_score = 0,
    gs_target = 15,
    gs_additiveDice = 1,
    gs_multiplicativeDice = 0,
    gs_persistentSprites = [],
    gs_rollState = Nothing,
    gs_round = 1
}

-- | Calculate final score from dice results
-- X = sum of additive dice values
-- Y = sum of multiplicative dice values (defaults to 1 if none)
-- Score = X * Y
calculateScore :: [DieResult] -> Int
calculateScore results =
    let x = sum [dr_value r | r <- results, dr_type r == Additive]
        y = sum [dr_value r | r <- results, dr_type r == Multiplicative]
        yFinal = if y == 0 then 1 else y
    in x * yFinal

-- | Calculate new target after reaching current target
calculateNewTarget :: Int -> Int -> Int -> Int
calculateNewTarget currentTarget numAdditive numMultiplicative =
    let expectedX = 3.5 * fromIntegral numAdditive
        expectedY = if numMultiplicative == 0 then 1.0 else 3.5 * fromIntegral numMultiplicative
        expectedScore = expectedX * expectedY
    in round (expectedScore * 1.3) + (currentTarget `div` 10)

-- | Detect combos: groups of same-type dice showing same number
detectCombos :: [DieResult] -> [(DiceType, Int, [DieResult])]
detectCombos results =
    let sorted = sortBy (comparing (\r -> (dr_type r, dr_value r))) results
        grouped = groupBy sameTypeAndValue sorted
        combos = filter (\g -> length g >= 2) grouped
    in [(dr_type (head g), dr_value (head g), g) | g <- combos]
  where
    sameTypeAndValue a b = dr_type a == dr_type b && dr_value a == dr_value b

-- | Count bonus dice from combos (each combo gives exactly 1 bonus die)
countBonusDice :: [(DiceType, Int, [DieResult])] -> [(DiceType, Int)]
countBonusDice combos = [(dt, 1) | (dt, _, _) <- combos]


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
-- Returns the sprite JSVal for persistence tracking
playDiceAnimation :: JSVal -> Application -> Container -> Int -> Int -> Int -> DiceType -> IO () -> IO JSVal
playDiceAnimation spritesheetCtx _app container screenW screenH finalFace diceType onComplete = do
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
    setScale animSprite 1.0 1.0  -- 128px native resolution
    setTint animSprite tintColor

    -- Apply negative filter for multiplicative dice
    when (diceType == Multiplicative) $ do
        negFilter <- newColorMatrixFilter
        colorMatrixNegative negFilter False
        let ColorMatrixFilter f = negFilter
        addFilter animSpriteVal f

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
            -- Don't remove sprite - keep it on screen until next roll
            onComplete

    add animTicker tickerCallback
    start animTicker
    return animSpriteVal

-- | Roll all dice (additive and multiplicative) simultaneously
rollAllDice :: JSVal -> Application -> Container -> Int -> Int
            -> IORef GameState -> Text -> Text -> Text -> Text -> IO ()
rollAllDice spritesheetCtx app container screenW screenH
            gameStateRef scoreText targetText diceCountText xyText = do
    gs <- readIORef gameStateRef

    -- Prevent rolling while roll in progress
    case gs_rollState gs of
        Just _ -> return ()  -- Roll already in progress
        Nothing -> do
            -- Clear previous roll's sprites
            forM_ (gs_persistentSprites gs) $ \spriteVal -> do
                let sprite = fromJSVal spriteVal :: Sprite
                void $ removeChild container sprite

            let totalDice = gs_additiveDice gs + gs_multiplicativeDice gs
            playDiceRollSound totalDice

            -- Initialize roll state
            writeIORef gameStateRef $ gs {
                gs_rollState = Just $ RollState {
                    rs_pending = totalDice,
                    rs_newResults = [],
                    rs_prevResults = [],
                    rs_isComboRoll = False
                },
                gs_persistentSprites = []
            }

            -- Create list of dice types to roll
            let diceToRoll = replicate (gs_additiveDice gs) Additive
                          ++ replicate (gs_multiplicativeDice gs) Multiplicative

            -- Roll each die
            forM_ diceToRoll $ \diceType -> do
                finalFace <- randomRIO (1, 6) :: IO Int
                spriteRef <- newIORef (error "sprite not initialized")
                let onComplete = do
                        spriteVal <- readIORef spriteRef
                        onDieComplete gameStateRef scoreText targetText diceCountText xyText
                                     container screenW screenH spritesheetCtx app
                                     diceType finalFace spriteVal
                spriteVal <- playDiceAnimation spritesheetCtx app container
                                               screenW screenH finalFace diceType
                                               onComplete
                writeIORef spriteRef spriteVal
                modifyIORef gameStateRef $ \s ->
                    s { gs_persistentSprites = spriteVal : gs_persistentSprites s }

-- | Called when a single die animation completes
onDieComplete :: IORef GameState
              -> Text -> Text -> Text -> Text
              -> Container -> Int -> Int
              -> JSVal -> Application
              -> DiceType -> Int -> JSVal
              -> IO ()
onDieComplete gameStateRef scoreText targetText diceCountText xyText
              container screenW screenH spritesheetCtx app
              diceType value spriteVal = do
    gs <- readIORef gameStateRef
    case gs_rollState gs of
        Nothing -> return ()  -- Shouldn't happen
        Just rs -> do
            let newResult = DieResult diceType value spriteVal
                updatedNewResults = newResult : rs_newResults rs
                newPending = rs_pending rs - 1

            if newPending == 0
                then onAllDiceComplete gameStateRef scoreText targetText diceCountText xyText
                                      container screenW screenH spritesheetCtx app
                                      updatedNewResults (rs_prevResults rs)
                else writeIORef gameStateRef $ gs {
                    gs_rollState = Just $ rs {
                        rs_pending = newPending,
                        rs_newResults = updatedNewResults
                    }
                }

-- | Called when all dice in current roll have completed
onAllDiceComplete :: IORef GameState -> Text -> Text -> Text -> Text
                  -> Container -> Int -> Int -> JSVal -> Application
                  -> [DieResult] -> [DieResult]  -- newResults, prevResults
                  -> IO ()
onAllDiceComplete gameStateRef scoreText targetText diceCountText xyText
                  container screenW screenH spritesheetCtx app
                  newResults prevResults = do
    -- Calculate and display current X * Y
    let allResults = newResults ++ prevResults
        x = sum [dr_value r | r <- allResults, dr_type r == Additive]
        y = sum [dr_value r | r <- allResults, dr_type r == Multiplicative]
        yDisplay = if y == 0 then 1 else y
        rollScore = x * yDisplay
    setText xyText (toJSString $ show x ++ " * " ++ show yDisplay ++ " = " ++ show rollScore)

    -- Detect combos ONLY in newly rolled dice
    let combos = detectCombos newResults
        bonusDice = countBonusDice combos
        totalBonus = sum [count | (_, count) <- bonusDice]

    if totalBonus > 0
        then do
            -- Trigger combo effects
            triggerComboEffects container screenW screenH combos
            -- Roll bonus dice - newResults become prevResults for next batch
            rollBonusDice spritesheetCtx app container screenW screenH
                         gameStateRef scoreText targetText diceCountText xyText
                         bonusDice (newResults ++ prevResults)
        else do
            -- No combos, finalize with all results
            finalizeRoll gameStateRef scoreText targetText diceCountText xyText
                        container screenW screenH (newResults ++ prevResults)

-- | Roll bonus dice from combos
rollBonusDice :: JSVal -> Application -> Container -> Int -> Int
              -> IORef GameState -> Text -> Text -> Text -> Text
              -> [(DiceType, Int)] -> [DieResult]
              -> IO ()
rollBonusDice spritesheetCtx app container screenW screenH
              gameStateRef scoreText targetText diceCountText xyText
              bonusDice allPreviousResults = do
    let totalBonus = sum [count | (_, count) <- bonusDice]
    playDiceRollSound totalBonus

    -- Set up roll state for bonus dice
    -- Previous results go to prevResults, newResults starts empty
    gs <- readIORef gameStateRef
    writeIORef gameStateRef $ gs {
        gs_rollState = Just $ RollState {
            rs_pending = totalBonus,
            rs_newResults = [],  -- Bonus dice will be added here
            rs_prevResults = allPreviousResults,  -- All previous results
            rs_isComboRoll = True
        }
    }

    -- Roll each bonus die
    forM_ bonusDice $ \(diceType, count) ->
        forM_ [1..count] $ \_ -> do
            finalFace <- randomRIO (1, 6) :: IO Int
            spriteRef <- newIORef (error "sprite not initialized")
            let onComplete = do
                    spriteVal <- readIORef spriteRef
                    onDieComplete gameStateRef scoreText targetText diceCountText xyText
                                 container screenW screenH spritesheetCtx app
                                 diceType finalFace spriteVal
            spriteVal <- playDiceAnimation spritesheetCtx app container
                                           screenW screenH finalFace diceType
                                           onComplete
            writeIORef spriteRef spriteVal
            modifyIORef gameStateRef $ \s ->
                s { gs_persistentSprites = spriteVal : gs_persistentSprites s }

-- | Trigger visual effects for combos (screen flash + dice highlight)
triggerComboEffects :: Container -> Int -> Int
                    -> [(DiceType, Int, [DieResult])]
                    -> IO ()
triggerComboEffects container screenW screenH combos = do
    -- Screen flash
    flashGraphics <- newGraphics
    beginFillWithAlpha flashGraphics 0xFFFFFF 0.8
    drawRect flashGraphics 0 0 (fromIntegral screenW) (fromIntegral screenH)
    endFill flashGraphics
    void $ addChild container flashGraphics

    -- Fade out animation using ticker
    flashTicker <- newTicker
    setMaxFPS flashTicker 60
    alphaRef <- newIORef (0.8 :: Float)

    tickerCallback <- jsFuncFromHs_ $ \tickerVal -> do
        let ticker = fromJSVal tickerVal :: Ticker
        alpha <- readIORef alphaRef
        let newAlpha = alpha - 0.05  -- Fade over ~16 frames (~270ms)
        if newAlpha <= 0
            then do
                stop ticker
                void $ removeChild container flashGraphics
            else do
                setAlpha flashGraphics newAlpha
                writeIORef alphaRef newAlpha

    add flashTicker tickerCallback
    start flashTicker

    -- Highlight matching dice
    forM_ combos $ \(_, _, matchingDice) ->
        forM_ matchingDice $ \die ->
            highlightDie (dr_sprite die)

-- | Highlight a die sprite (pulsing scale effect)
highlightDie :: JSVal -> IO ()
highlightDie spriteVal = do
    let sprite = fromJSVal spriteVal :: Sprite
    -- Temporarily increase scale for "pop" effect
    setScale sprite 1.2 1.2  -- Slightly larger than normal 1.0

    -- Animate back to normal
    highlightTicker <- newTicker
    setMaxFPS highlightTicker 60
    scaleRef <- newIORef (1.2 :: Float)

    tickerCallback <- jsFuncFromHs_ $ \tickerVal -> do
        let ticker = fromJSVal tickerVal :: Ticker
        currentScale <- readIORef scaleRef
        let newScale = currentScale - 0.01  -- 20 frames to return to normal
        if newScale <= 1.0
            then do
                stop ticker
                setScale sprite 1.0 1.0
            else do
                setScale sprite newScale newScale
                writeIORef scaleRef newScale

    add highlightTicker tickerCallback
    start highlightTicker

-- | Finalize roll: calculate score, check target, update display
finalizeRoll :: IORef GameState -> Text -> Text -> Text -> Text
             -> Container -> Int -> Int
             -> [DieResult]
             -> IO ()
finalizeRoll gameStateRef scoreText targetText diceCountText xyText
             container screenW screenH results = do
    gs <- readIORef gameStateRef

    let rollScore = calculateScore results
        newScore = gs_score gs + rollScore

    -- Clear roll state
    writeIORef gameStateRef $ gs {
        gs_score = newScore,
        gs_rollState = Nothing
    }

    -- Update score display
    setText scoreText (toJSString $ "Score: " ++ show newScore)

    -- Check if target reached
    when (newScore >= gs_target gs) $
        showChoiceDialog container screenW screenH
                        gameStateRef scoreText targetText diceCountText xyText

-- | Show choice dialog when target is reached
showChoiceDialog :: Container -> Int -> Int
                 -> IORef GameState -> Text -> Text -> Text -> Text
                 -> IO ()
showChoiceDialog container screenW screenH
                 gameStateRef scoreText targetText diceCountText xyText = do
    blipWithFreq 800.0  -- Success sound

    -- Create modal container
    modalContainer <- newContainer

    -- Dim background
    dimOverlay <- newGraphics
    beginFillWithAlpha dimOverlay 0x000000 0.7
    drawRect dimOverlay 0 0 (fromIntegral screenW) (fromIntegral screenH)
    endFill dimOverlay
    setEventMode dimOverlay "static"
    void $ addChild modalContainer dimOverlay

    -- Dialog box dimensions
    let dialogW = 400.0
        dialogH = 200.0
        dialogX = (fromIntegral screenW - dialogW) / 2
        dialogY = (fromIntegral screenH - dialogH) / 2

    -- Dialog background
    dialogBg <- newGraphics
    beginFill dialogBg 0xFFFFFF
    drawRoundedRect dialogBg dialogX dialogY dialogW dialogH 10.0
    endFill dialogBg
    void $ addChild modalContainer dialogBg

    -- Title text
    titleText <- newTextWithStyle "Target Reached!" "black"
    setAnchor titleText 0.5 0.5
    setX titleText (fromIntegral screenW / 2)
    setY titleText (dialogY + 40)
    void $ addChild modalContainer titleText

    -- Subtitle
    subtitleText <- newTextWithStyle "Choose your new die:" "gray"
    setAnchor subtitleText 0.5 0.5
    setX subtitleText (fromIntegral screenW / 2)
    setY subtitleText (dialogY + 70)
    void $ addChild modalContainer subtitleText

    -- Choice buttons layout
    let buttonY = dialogY + 130
        buttonW = 150.0
        buttonH = 50.0
        gap = 20.0
        leftX = fromIntegral screenW / 2 - buttonW - gap / 2
        rightX = fromIntegral screenW / 2 + gap / 2

    -- Additive button (green)
    createChoiceButton modalContainer leftX buttonY buttonW buttonH
                      "Additive (+)" 0x44FF44 $ do
        void $ removeChild container modalContainer
        applyChoice gameStateRef Additive scoreText targetText diceCountText xyText

    -- Multiplicative button (inverted/dark)
    createChoiceButton modalContainer rightX buttonY buttonW buttonH
                      "Multiply (x)" 0x444444 $ do
        void $ removeChild container modalContainer
        applyChoice gameStateRef Multiplicative scoreText targetText diceCountText xyText

    void $ addChild container modalContainer

-- | Create a styled button for the choice dialog
createChoiceButton :: Container -> Float -> Float -> Float -> Float
                   -> String -> Int -> IO () -> IO ()
createChoiceButton parent x y w h label color onClick = do
    btnContainer <- newContainer
    setX btnContainer x
    setY btnContainer y
    setEventMode btnContainer "static"
    setCursor btnContainer "pointer"

    -- Button background
    bg <- newGraphics
    beginFill bg color
    drawRoundedRect bg 0 0 w h 5.0
    endFill bg
    void $ addChild btnContainer bg

    -- Button text
    btnText <- newTextWithStyle (toJSString label) "white"
    setAnchor btnText 0.5 0.5
    setX btnText (w / 2)
    setY btnText (h / 2)
    void $ addChild btnContainer btnText

    on "pointerdown" btnContainer =<< jsFuncFromHs_ (\_ -> onClick)

    void $ addChild parent btnContainer

-- | Apply player's choice and advance game
applyChoice :: IORef GameState -> DiceType -> Text -> Text -> Text -> Text -> IO ()
applyChoice gameStateRef choice scoreText targetText diceCountText xyText = do
    gs <- readIORef gameStateRef

    let newAdditive = if choice == Additive
                      then gs_additiveDice gs + 1
                      else gs_additiveDice gs
        newMultiplicative = if choice == Multiplicative
                           then gs_multiplicativeDice gs + 1
                           else gs_multiplicativeDice gs
        newTarget = calculateNewTarget (gs_target gs) newAdditive newMultiplicative
        newRound = gs_round gs + 1

    writeIORef gameStateRef $ gs {
        gs_score = 0,
        gs_target = newTarget,
        gs_additiveDice = newAdditive,
        gs_multiplicativeDice = newMultiplicative,
        gs_round = newRound
    }

    -- Update display
    setText scoreText (toJSString "Score: 0")
    setText targetText (toJSString $ "Target: " ++ show newTarget)
    updateDiceCountDisplay diceCountText newAdditive newMultiplicative
    setText xyText (toJSString "")  -- Clear X*Y display

-- | Update the dice count display
updateDiceCountDisplay :: Text -> Int -> Int -> IO ()
updateDiceCountDisplay diceCountText additive multiplicative =
    setText diceCountText (toJSString $
        show additive ++ " additive | " ++ show multiplicative ++ " multiply")

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

    -- Dice count display below score
    dice_count_text <- newTextWithStyle (toJSString $
        show gs_additiveDice ++ " additive | " ++ show gs_multiplicativeDice ++ " multiply") "gray"
    setX dice_count_text (fromIntegral screen_width / 2.0)
    setY dice_count_text (fromIntegral screen_height / 2.0 + 40.0)
    setAnchor dice_count_text 0.5 0.5
    void $ addChild screenContainer dice_count_text

    -- X * Y = score display (shows current roll breakdown)
    xy_text <- newTextWithStyle "" "gray"
    setX xy_text (fromIntegral screen_width / 2.0)
    setY xy_text (fromIntegral screen_height / 2.0 + 80.0)
    setAnchor xy_text 0.5 0.5
    void $ addChild screenContainer xy_text

    -- Buttons at the bottom
    let sample_button = Button {
        button_text = "Roll",
        button_x = fromIntegral screen_width / 2.0,
        button_y = fromIntegral screen_height - 150.0,
        button_width = 100.0,
        button_height = 100.0,
        button_color = "black",
        button_on_click = \_ -> rollAllDice spritesheetCtx app screenContainer screen_width screen_height game_state_ref score_text target_text dice_count_text xy_text
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
