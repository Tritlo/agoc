{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE LambdaCase #-}

module Main where
import Lib (blipWithFreq, playDiceRollSound, closeWindow, generateDiceSpritesheet, getAnimationFrames, newAnimatedSpriteFromJSArray, getWindowWidth, getWindowHeight, onWindowResize, getState, setState)
import Game.State
import Game.Logic
import Graphics.PixiJS
import Data.IORef (IORef, atomicModifyIORef', modifyIORef, newIORef, readIORef, writeIORef)
import Control.Monad (when, forM_, forM, void)
import System.Random (randomRIO)
import Data.List (groupBy, sortBy)
import Data.Ord (comparing)
import qualified Data.Set as Set
import Data.Array (listArray, elems, (!), (//))

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
    menu_color :: JSString,
    menu_hoverColor :: JSString
}

-- | Render a menu and return the container holding it
renderMenu :: Menu -> IO Container
renderMenu menu = do
    container <- newContainer
    setX container menu.menu_x
    setY container menu.menu_y

    forM_ (zip [0 :: Int ..] menu.menu_items) $ \(idx, item) -> do
        -- Create a container for each menu item with background
        itemContainer <- newContainer
        setX itemContainer 0.0
        setY itemContainer (fromIntegral idx * menu.menu_spacing)
        setEventMode itemContainer "static"
        setCursor itemContainer "pointer"

        -- Semi-transparent background for click area
        let itemW = 200.0
            itemH = 50.0
        itemBg <- newGraphics
        beginFillWithAlpha itemBg colorPanelMid 0.0  -- Invisible initially
        drawRoundedRect itemBg (-itemW / 2) (-itemH / 2) itemW itemH 8.0
        endFill itemBg
        void $ addChild itemContainer itemBg

        text_obj <- newTextWithStyle (toJSString item.menuItem_text) menu.menu_color
        setAnchor text_obj 0.5 0.5
        void $ addChild itemContainer text_obj

        let textVal = toJSVal text_obj
            bgVal = toJSVal itemBg

        -- Click handler on container
        on "pointerdown" itemContainer =<< jsFuncFromHs_ (\_ -> item.menuItem_action)

        -- Hover effect - change color and show background
        on "pointerover" itemContainer =<< jsFuncFromHs_
             (\_ -> do
                 setPropertyKey ["style", "fill"] textVal (stringAsVal menu.menu_hoverColor)
                 -- Show subtle background on hover
                 clear (fromJSVal bgVal :: Graphics)
                 beginFillWithAlpha (fromJSVal bgVal :: Graphics) colorPanelMid 0.3
                 drawRoundedRect (fromJSVal bgVal :: Graphics) (-itemW / 2) (-itemH / 2) itemW itemH 8.0
                 endFill (fromJSVal bgVal :: Graphics)
             )
        on "pointerout" itemContainer =<< jsFuncFromHs_
             (\_ -> do
                 setPropertyKey ["style", "fill"] textVal (stringAsVal menu.menu_color)
                 -- Hide background
                 clear (fromJSVal bgVal :: Graphics)
                 beginFillWithAlpha (fromJSVal bgVal :: Graphics) colorPanelMid 0.0
                 drawRoundedRect (fromJSVal bgVal :: Graphics) (-itemW / 2) (-itemH / 2) itemW itemH 8.0
                 endFill (fromJSVal bgVal :: Graphics)
             )

        addChild container itemContainer

    return container

-- *****************************************************************************
-- * Screen Management
-- *****************************************************************************

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
    -- Button container for background + text
    btnContainer <- newContainer
    setX btnContainer button.button_x
    setY btnContainer button.button_y
    setEventMode btnContainer "static"
    setCursor btnContainer "pointer"

    -- Button background with gold styling
    btnBg <- newGraphics
    beginFillWithAlpha btnBg colorPanelMid 0.9
    lineStyleWithOptions btnBg 2.0 colorAccentGold 0.8 0.5
    drawRoundedRect btnBg (-button.button_width / 2) (-button.button_height / 2)
                    button.button_width button.button_height 8.0
    endFill btnBg
    void $ addChild btnContainer btnBg

    button_text <- newTextWithStyle (toJSString button.button_text) colorTextGold
    setAnchor button_text 0.5 0.5
    void $ addChild btnContainer button_text

    let bgVal = toJSVal btnBg
        textVal = toJSVal button_text

    -- Attach events to container, not text
    on "pointerdown" btnContainer =<< jsFuncFromHs_ button.button_on_click

    on "pointerover" btnContainer =<< jsFuncFromHs_
         (\_ -> do
             setPropertyKey ["style", "fill"] textVal (stringAsVal "#ffd700")
             -- Brighten background
             clear (fromJSVal bgVal :: Graphics)
             beginFillWithAlpha (fromJSVal bgVal :: Graphics) colorPanelMid 1.0
             lineStyleWithOptions (fromJSVal bgVal :: Graphics) 2.0 colorAccentGoldBright 1.0 0.5
             drawRoundedRect (fromJSVal bgVal :: Graphics) (-button.button_width / 2) (-button.button_height / 2)
                             button.button_width button.button_height 8.0
             endFill (fromJSVal bgVal :: Graphics)
             )
    on "pointerout" btnContainer =<< jsFuncFromHs_
         (\_ -> do
             setPropertyKey ["style", "fill"] textVal (stringAsVal colorTextGold)
             -- Reset background
             clear (fromJSVal bgVal :: Graphics)
             beginFillWithAlpha (fromJSVal bgVal :: Graphics) colorPanelMid 0.9
             lineStyleWithOptions (fromJSVal bgVal :: Graphics) 2.0 colorAccentGold 0.8 0.5
             drawRoundedRect (fromJSVal bgVal :: Graphics) (-button.button_width / 2) (-button.button_height / 2)
                             button.button_width button.button_height 8.0
             endFill (fromJSVal bgVal :: Graphics)
             )

    void $ addChild container btnContainer
    return ()

-- *****************************************************************************
-- * State Helpers
-- *****************************************************************************

-- | Apply a pure action, update the runtime reference, and persist a snapshot.
applyActionAndStore :: IORef GameState -> GameAction -> IO GameState
applyActionAndStore ref action = do
    newState <- atomicModifyIORef' ref (\gs -> let gs' = applyAction action gs in (gs', gs'))
    setState (toSnapshot newState)
    pure newState

-- | Persist the current state snapshot to `window.GAMESTATE`.
persistSnapshot :: IORef GameState -> IO ()
persistSnapshot ref = readIORef ref >>= setState . toSnapshot


-- *****************************************************************************
-- * Shop System
-- *****************************************************************************

-- | Generate shop contents with random joker offerings
generateShop :: [Joker] -> Int -> IO ShopContents
generateShop ownedJokers _nextDieId = do
    -- Find jokers not already owned
    let availableJokers = filter (\j -> j `notElem` ownedJokers) allJokers
        hasJokerSlot = length ownedJokers < 5

    -- Pick first random joker if available and player has slot
    (maybeJoker1, remaining1) <- if hasJokerSlot && not (null availableJokers)
        then do
            idx <- randomRIO (0, length availableJokers - 1)
            let picked = availableJokers !! idx
                remaining = take idx availableJokers ++ drop (idx + 1) availableJokers
            return (Just picked, remaining)
        else return (Nothing, availableJokers)

    -- Pick second random joker from remaining if available
    maybeJoker2 <- if hasJokerSlot && not (null remaining1)
        then do
            idx <- randomRIO (0, length remaining1 - 1)
            return $ Just (remaining1 !! idx)
        else return Nothing

    -- Random joker costs (4-8)
    jokerCost1 <- randomRIO (4, 8)
    jokerCost2 <- randomRIO (4, 8)

    return ShopContents {
        shop_joker = maybeJoker1,
        shop_jokerCost = jokerCost1,
        shop_joker2 = maybeJoker2,
        shop_joker2Cost = jokerCost2,
        shop_rerollCost = 2
    }

-- *****************************************************************************
-- * Deckbuilding Core Mechanics
-- *****************************************************************************

-- | Shuffle a list using Fisher-Yates algorithm
shuffleList :: [a] -> IO [a]
shuffleList [] = return []
shuffleList [x] = return [x]
shuffleList xs = do
    let n = length xs
    indices <- mapM (\i -> randomRIO (0, i)) [n-1, n-2 .. 1]
    let arr = listArray (0, n-1) xs
        swapped = foldl doSwap arr (zip [n-1, n-2 .. 1] indices)
    return $ elems swapped
  where
    doSwap arr' (i, j) =
        let a = arr' ! i
            b = arr' ! j
        in arr' // [(i, b), (j, a)]

-- | Draw dice from bag to hand
-- If bag empties mid-draw, shuffle discard into bag and continue
drawToHand :: Int -> DeckZones -> IO DeckZones
drawToHand n zones
    | n <= 0 = return zones
    | null (dz_bag zones) && null (dz_discard zones) = return zones
    | null (dz_bag zones) = do
        -- Shuffle discard into bag
        shuffled <- shuffleList (dz_discard zones)
        drawToHand n zones { dz_bag = shuffled, dz_discard = [] }
    | otherwise = do
        let (drawn, remaining) = splitAt (min n (length (dz_bag zones))) (dz_bag zones)
            newZones = zones {
                dz_bag = remaining,
                dz_hand = dz_hand zones ++ drawn
            }
        -- If we didn't draw enough and there's discard, reshuffle and continue
        let stillNeeded = n - length drawn
        if stillNeeded > 0 && not (null (dz_discard newZones))
            then drawToHand stillNeeded newZones
            else return newZones

-- | Move all hand dice to discard (after throw)
discardHand :: DeckZones -> DeckZones
discardHand zones = zones {
    dz_hand = [],
    dz_discard = dz_discard zones ++ dz_hand zones
}

-- | Check if another die can be selected
canSelectMore :: SelectionState -> Bool
canSelectMore ss = Set.size (ss_selected ss) < ss_maxSelectable ss

-- | Toggle selection of a die
toggleDieSelection :: DieId -> IORef GameState -> IO Bool
toggleDieSelection dieId gsRef = do
    gs <- readIORef gsRef
    let sel = gs_selection gs
        currentlySelected = Set.member dieId (ss_selected sel)

    if currentlySelected
        then do
            -- Deselect via action
            _ <- applyActionAndStore gsRef (DeselectDie dieId)
            return False  -- Now deselected
        else if canSelectMore sel
            then do
                -- Select if under limit
                _ <- applyActionAndStore gsRef (SelectDie dieId)
                return True  -- Now selected
            else return False  -- Can't select more

-- | Get the selected dice from hand
getSelectedDice :: GameState -> [Die]
getSelectedDice gs =
    filter (\d -> Set.member (die_id d) (ss_selected $ gs_selection gs))
           (dz_hand $ gs_deckZones gs)

-- | Add a new die to the bag (when player reaches target)
addNewDieToBag :: DiceType -> IORef GameState -> IO ()
addNewDieToBag diceType gsRef = do
    gs <- readIORef gsRef
    let newId = gs_nextDieId gs
        newDie = Die (DieId newId) diceType
        zones = gs_deckZones gs
        newZones = zones { dz_bag = newDie : dz_bag zones }
    writeIORef gsRef gs {
        gs_deckZones = newZones,
        gs_nextDieId = newId + 1
    }

-- | Count total dice across all zones
countTotalDice :: DeckZones -> (Int, Int)  -- (additive, multiplicative)
countTotalDice zones =
    let allDice = dz_bag zones ++ dz_hand zones ++ dz_discard zones
        additive = length [d | d <- allDice, die_type d == Additive]
        multiplicative = length [d | d <- allDice, die_type d == Multiplicative]
    in (additive, multiplicative)

-- | FPS settings for power management
idleFPS :: Float
idleFPS = 2.0  -- Low FPS when nothing is animating

activeFPS :: Float
activeFPS = 60.0  -- Full FPS during animations

-- *****************************************************************************
-- * Fixed 16:9 Aspect Ratio with Portrait Rotation
-- *****************************************************************************

-- | Fixed design resolution (16:9 aspect ratio)
designWidth :: Int
designWidth = 1280

designHeight :: Int
designHeight = 720

-- | Calculate viewport transformation for fixed aspect ratio with portrait rotation
-- Returns (scale, rotation, pivotX, pivotY, posX, posY)
calculateViewportTransform :: Int -> Int -> (Float, Float, Float, Float, Float, Float)
calculateViewportTransform windowW windowH =
    let wW = fromIntegral windowW :: Float
        wH = fromIntegral windowH :: Float
        dW = fromIntegral designWidth :: Float
        dH = fromIntegral designHeight :: Float

        -- Check if window is portrait (taller than wide)
        isPortrait = wH > wW

        -- In portrait mode, we rotate 90 degrees and swap effective dimensions
        (effectiveWinW, effectiveWinH) = if isPortrait then (wH, wW) else (wW, wH)

        -- Calculate scale to fit while maintaining aspect ratio
        scaleX = effectiveWinW / dW
        scaleY = effectiveWinH / dH
        scale = min scaleX scaleY

        -- Rotation angle (90 degrees counter-clockwise for portrait)
        rotation = if isPortrait then (-pi / 2) else 0

        -- Pivot point (center of design area)
        pivotX = dW / 2
        pivotY = dH / 2

        -- Position (center of window)
        posX = wW / 2
        posY = wH / 2

    in (scale, rotation, pivotX, pivotY, posX, posY)

-- | Apply viewport transformation to a container
applyViewportTransform :: Container -> Int -> Int -> IO ()
applyViewportTransform viewport windowW windowH = do
    let (scale, rotation, pivotX, pivotY, posX, posY) =
            calculateViewportTransform windowW windowH
    setPivot viewport pivotX pivotY
    setX viewport posX
    setY viewport posY
    setScale viewport scale scale
    setRotation viewport rotation

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
-- Grows slowly to keep the game moving at a good pace
calculateNewTarget :: Int -> Int -> Int -> Int
calculateNewTarget currentTarget _numAdditive _numMultiplicative =
    -- Simple linear growth: add 5 + 10% of current target
    currentTarget + 5 + (currentTarget `div` 10)

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

-- | Roll selected dice from hand (deckbuilding version)
rollSelectedDice :: JSVal -> Application -> Container -> Int -> Int
                 -> IORef GameState -> Text -> Text -> Text -> Text -> Text -> Text -> IO ()
rollSelectedDice spritesheetCtx app container screenW screenH
                 gameStateRef scoreText targetText entropyText deckText selectionText xyText = do
    gs <- readIORef gameStateRef

    -- Only roll during selection phase and when not already rolling
    case (gs_phase gs, gs_rollState gs) of
        (SelectionPhase, Nothing) -> do
            let selectedDice = getSelectedDice gs

            -- Must have at least 1 die selected
            when (not $ null selectedDice) $ do
                -- Clear hand display
                case gs_handContainer gs of
                    Just hc -> void $ removeChild container hc
                    Nothing -> return ()

                -- Clear previous roll's sprites
                forM_ (gs_persistentSprites gs) $ \spriteVal -> do
                    let sprite = fromJSVal spriteVal :: Sprite
                    void $ removeChild container sprite

                -- Boost FPS for animations
                appTicker <- getTicker app
                setMaxFPS appTicker activeFPS

                let totalDice = length selectedDice
                playDiceRollSound totalDice

                -- Set phase to rolling and initialize roll state
                writeIORef gameStateRef $ gs {
                    gs_phase = RollingPhase,
                    gs_rollState = Just $ RollState {
                        rs_pending = totalDice,
                        rs_newResults = [],
                        rs_prevResults = [],
                        rs_isComboRoll = False,
                        rs_comboCount = 0
                    },
                    gs_persistentSprites = [],
                    gs_handContainer = Nothing,
                    gs_handSprites = []
                }

                -- Roll each selected die
                forM_ selectedDice $ \die -> do
                    finalFace <- randomRIO (1, 6) :: IO Int
                    spriteRef <- newIORef (error "sprite not initialized")
                    let onComplete = do
                            spriteVal <- readIORef spriteRef
                            onDieComplete gameStateRef scoreText targetText entropyText deckText selectionText xyText
                                         container screenW screenH spritesheetCtx app
                                         (die_type die) finalFace spriteVal
                    spriteVal <- playDiceAnimation spritesheetCtx app container
                                                   screenW screenH finalFace (die_type die)
                                                   onComplete
                    writeIORef spriteRef spriteVal
                    modifyIORef gameStateRef $ \s ->
                        s { gs_persistentSprites = spriteVal : gs_persistentSprites s }

        _ -> return ()  -- Not in selection phase or roll in progress

-- | Called when a single die animation completes
onDieComplete :: IORef GameState
              -> Text -> Text -> Text -> Text -> Text -> Text
              -> Container -> Int -> Int
              -> JSVal -> Application
              -> DiceType -> Int -> JSVal
              -> IO ()
onDieComplete gameStateRef scoreText targetText entropyText deckText selectionText xyText
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
                then onAllDiceComplete gameStateRef scoreText targetText entropyText deckText selectionText xyText
                                      container screenW screenH spritesheetCtx app
                                      updatedNewResults (rs_prevResults rs)
                else writeIORef gameStateRef $ gs {
                    gs_rollState = Just $ rs {
                        rs_pending = newPending,
                        rs_newResults = updatedNewResults
                    }
                }

-- | Called when all dice in current roll have completed
onAllDiceComplete :: IORef GameState -> Text -> Text -> Text -> Text -> Text -> Text
                  -> Container -> Int -> Int -> JSVal -> Application
                  -> [DieResult] -> [DieResult]  -- newResults, prevResults
                  -> IO ()
onAllDiceComplete gameStateRef scoreText targetText entropyText deckText selectionText xyText
                  container screenW screenH spritesheetCtx app
                  newResults prevResults = do
    gs <- readIORef gameStateRef
    let jokers = gs_jokers gs

    -- Get current combo count from roll state
    let currentComboCount = case gs_rollState gs of
            Just rs -> rs_comboCount rs
            Nothing -> 0

    -- Detect combos ONLY in newly rolled dice
    let combos = detectCombos newResults
        newComboCount = length combos
        totalComboCount = currentComboCount + newComboCount
        bonusDice = countBonusDice combos
        totalBonus = sum [count | (_, count) <- bonusDice]

    -- Calculate and display current X * Y with joker effects
    let allResults = newResults ++ prevResults
        (modX, modY, rollScore) = calculateScoreWithJokers jokers allResults totalComboCount
    setText xyText (toJSString $ show modX ++ " * " ++ show modY ++ " = " ++ show rollScore)

    if totalBonus > 0
        then do
            -- Trigger combo effects
            triggerComboEffects container screenW screenH combos
            -- Roll bonus dice - newResults become prevResults for next batch
            rollBonusDice spritesheetCtx app container screenW screenH
                         gameStateRef scoreText targetText entropyText deckText selectionText xyText
                         bonusDice (newResults ++ prevResults) totalComboCount
        else do
            -- No combos, finalize with all results
            finalizeRoll gameStateRef scoreText targetText entropyText deckText selectionText xyText
                        container screenW screenH spritesheetCtx app (newResults ++ prevResults) totalComboCount

-- | Roll bonus dice from combos
rollBonusDice :: JSVal -> Application -> Container -> Int -> Int
              -> IORef GameState -> Text -> Text -> Text -> Text -> Text -> Text
              -> [(DiceType, Int)] -> [DieResult] -> Int  -- Added comboCount parameter
              -> IO ()
rollBonusDice spritesheetCtx app container screenW screenH
              gameStateRef scoreText targetText entropyText deckText selectionText xyText
              bonusDice allPreviousResults currentComboCount = do
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
            rs_isComboRoll = True,
            rs_comboCount = currentComboCount  -- Preserve combo count
        }
    }

    -- Roll each bonus die
    forM_ bonusDice $ \(diceType, count) ->
        forM_ [1..count] $ \_ -> do
            finalFace <- randomRIO (1, 6) :: IO Int
            spriteRef <- newIORef (error "sprite not initialized")
            let onComplete = do
                    spriteVal <- readIORef spriteRef
                    onDieComplete gameStateRef scoreText targetText entropyText deckText selectionText xyText
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

-- | Finalize roll: calculate score, discard hand, check target, start new round
finalizeRoll :: IORef GameState -> Text -> Text -> Text -> Text -> Text -> Text
             -> Container -> Int -> Int -> JSVal -> Application
             -> [DieResult] -> Int  -- Added comboCount parameter
             -> IO ()
finalizeRoll gameStateRef scoreText targetText entropyText deckText selectionText xyText
             container screenW screenH spritesheetCtx app results comboCount = do
    gs <- readIORef gameStateRef

    let jokers = gs_jokers gs
        (_, _, rollScore) = calculateScoreWithJokers jokers results comboCount
        newScore = gs_score gs + rollScore
        newRollsRemaining = gs_rollsRemaining gs - 1

    -- Discard entire hand (both thrown and unthrown)
    let newZones = discardHand (gs_deckZones gs)

    -- Clear roll state and update game state
    writeIORef gameStateRef $ gs {
        gs_score = newScore,
        gs_deckZones = newZones,
        gs_rollState = Nothing,
        gs_selection = SelectionState Set.empty maxSelection,
        gs_phase = ResolutionPhase,
        gs_rollsRemaining = newRollsRemaining
    }

    -- Update displays
    setText scoreText (toJSString $ "Score: " ++ show newScore)
    updateDeckZonesDisplay deckText newZones
    persistSnapshot gameStateRef

    -- Restore idle FPS now that animations are done
    appTicker <- getTicker app
    setMaxFPS appTicker idleFPS

    -- Check if target reached
    if newScore >= gs_target gs
        then showRoundComplete container screenW screenH spritesheetCtx
                               gameStateRef scoreText targetText entropyText deckText selectionText xyText
        else if newRollsRemaining <= 0
            then showGameOver container screenW screenH
            else startNewRound spritesheetCtx container screenW screenH
                              gameStateRef scoreText targetText entropyText deckText selectionText xyText

-- | Start a new round: draw cards and enter selection phase
startNewRound :: JSVal -> Container -> Int -> Int
              -> IORef GameState -> Text -> Text -> Text -> Text -> Text -> Text
              -> IO ()
startNewRound spritesheetCtx container screenW screenH
              gsRef scoreText targetText entropyText deckText selectionText xyText = do
    gs <- readIORef gsRef

    -- Calculate how many to draw
    let drawNum = drawCount (gs_entropy gs)

    -- Shuffle bag before drawing if empty
    shuffledBag <- if null (dz_bag $ gs_deckZones gs) && not (null (dz_discard $ gs_deckZones gs))
        then shuffleList (dz_discard $ gs_deckZones gs)
        else return (dz_bag $ gs_deckZones gs)

    let zonesWithShuffledBag = (gs_deckZones gs) {
        dz_bag = shuffledBag,
        dz_discard = if null (dz_bag $ gs_deckZones gs) then [] else dz_discard (gs_deckZones gs)
    }

    -- Draw from bag (with reshuffle if needed)
    newZones <- drawToHand drawNum zonesWithShuffledBag

    -- Reset selection (fixed at maxSelection)
    let newSelection = SelectionState Set.empty maxSelection

    let nextRound =
          if gs_phase gs == ResolutionPhase then gs_round gs + 1 else gs_round gs

    writeIORef gsRef gs {
        gs_deckZones = newZones,
        gs_selection = newSelection,
        gs_phase = SelectionPhase,
        gs_round = nextRound
    }
    persistSnapshot gsRef

    -- Update displays
    updateEntropyDisplay entropyText (gs_entropy gs)
    updateDeckZonesDisplay deckText newZones

    -- Clear X*Y text
    setText xyText (toJSString "")

    -- Render hand
    renderHandDisplay spritesheetCtx container screenW screenH gsRef selectionText

-- | Show choice dialog when target is reached
showChoiceDialog :: Container -> Int -> Int -> JSVal
                 -> IORef GameState -> Text -> Text -> Text -> Text -> Text -> Text
                 -> IO ()
showChoiceDialog container screenW screenH spritesheetCtx
                 gameStateRef scoreText targetText entropyText deckText selectionText xyText = do
    blipWithFreq 800.0  -- Success sound

    -- Set phase to choice
    modifyIORef gameStateRef $ \gs -> gs { gs_phase = ChoicePhase }

    -- Create modal container
    modalContainer <- newContainer

    -- Dim background with dark tint
    dimOverlay <- newGraphics
    beginFillWithAlpha dimOverlay 0x0a0a15 0.92
    drawRect dimOverlay 0 0 (fromIntegral screenW) (fromIntegral screenH)
    endFill dimOverlay
    setEventMode dimOverlay "static"
    void $ addChild modalContainer dimOverlay

    -- Dialog box dimensions
    let dialogW = 450.0
        dialogH = 220.0
        dialogX = (fromIntegral screenW - dialogW) / 2
        dialogY = (fromIntegral screenH - dialogH) / 2

    -- Dialog background with Casino Noir styling
    dialogBg <- newGraphics
    beginFillWithAlpha dialogBg colorPanelDark 0.98
    drawRoundedRect dialogBg dialogX dialogY dialogW dialogH 12.0
    endFill dialogBg
    -- Gold border
    lineStyleWithOptions dialogBg 2.0 colorAccentGold 1.0 0.5
    drawRoundedRect dialogBg dialogX dialogY dialogW dialogH 12.0
    endFill dialogBg
    void $ addChild modalContainer dialogBg

    -- Title text with gold color
    titleText <- newTextWithStyle "Target Reached!" colorTextGold
    setAnchor titleText 0.5 0.5
    setX titleText (fromIntegral screenW / 2)
    setY titleText (dialogY + 40)
    void $ addChild modalContainer titleText

    -- Subtitle
    subtitleText <- newTextWithStyle "Choose a new die to add to your bag" colorTextSecondary
    setAnchor subtitleText 0.5 0.5
    setX subtitleText (fromIntegral screenW / 2)
    setY subtitleText (dialogY + 75)
    void $ addChild modalContainer subtitleText

    -- Choice buttons layout
    let buttonY = dialogY + 140
        buttonW = 150.0
        buttonH = 50.0
        gap = 20.0
        leftX = fromIntegral screenW / 2 - buttonW - gap / 2
        rightX = fromIntegral screenW / 2 + gap / 2

    -- Additive button (themed green)
    createChoiceButton modalContainer leftX buttonY buttonW buttonH
                      "Additive (+)" 0x0a3d1a $ do
        void $ removeChild container modalContainer
        applyChoice spritesheetCtx container screenW screenH
                   gameStateRef Additive scoreText targetText entropyText deckText selectionText xyText

    -- Multiplicative button (themed red)
    createChoiceButton modalContainer rightX buttonY buttonW buttonH
                      "Multiply (x)" 0x3d1a1a $ do
        void $ removeChild container modalContainer
        applyChoice spritesheetCtx container screenW screenH
                   gameStateRef Multiplicative scoreText targetText entropyText deckText selectionText xyText

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

    -- Button background with border
    bg <- newGraphics
    beginFillWithAlpha bg color 0.9
    drawRoundedRect bg 0 0 w h 6.0
    endFill bg
    -- Gold border for emphasis
    lineStyleWithOptions bg 1.5 colorAccentGold 0.8 0.5
    drawRoundedRect bg 0 0 w h 6.0
    endFill bg
    void $ addChild btnContainer bg

    -- Button text
    btnText <- newTextWithStyle (toJSString label) colorTextPrimary
    setAnchor btnText 0.5 0.5
    setX btnText (w / 2)
    setY btnText (h / 2)
    void $ addChild btnContainer btnText

    -- Hover effect
    on "pointerover" btnContainer =<< jsFuncFromHs_ (\_ -> setScale btnContainer 1.05 1.05)
    on "pointerout" btnContainer =<< jsFuncFromHs_ (\_ -> setScale btnContainer 1.0 1.0)

    on "pointerdown" btnContainer =<< jsFuncFromHs_ (\_ -> onClick)

    void $ addChild parent btnContainer

-- | Apply player's choice and advance game (deckbuilding version)
applyChoice :: JSVal -> Container -> Int -> Int
            -> IORef GameState -> DiceType -> Text -> Text -> Text -> Text -> Text -> Text
            -> IO ()
applyChoice spritesheetCtx container screenW screenH
            gameStateRef choice scoreText targetText entropyText deckText selectionText xyText = do
    -- Add new die to bag and increase entropy
    addNewDieToBag choice gameStateRef

    gs <- readIORef gameStateRef

    -- Calculate new target based on total dice count
    let (numAdditive, numMultiplicative) = countTotalDice (gs_deckZones gs)
        newTarget = calculateNewTarget (gs_target gs) numAdditive numMultiplicative

    writeIORef gameStateRef $ gs {
        gs_score = 0,
        gs_target = newTarget
    }

    -- Update displays
    setText scoreText (toJSString "Score: 0")
    setText targetText (toJSString $ "Target: " ++ show newTarget)
    updateEntropyDisplay entropyText (gs_entropy gs)
    updateDeckZonesDisplay deckText (gs_deckZones gs)

    -- Start new round
    startNewRound spritesheetCtx container screenW screenH
                 gameStateRef scoreText targetText entropyText deckText selectionText xyText

-- | Update the dice count display (now shows deck zone counts)
updateDeckZonesDisplay :: Text -> DeckZones -> IO ()
updateDeckZonesDisplay deckText zones =
    let bagCount = length (dz_bag zones)
        discardCount = length (dz_discard zones)
    in setText deckText (toJSString $
        "Bag: " ++ show bagCount ++ " | Discard: " ++ show discardCount)

-- | Update entropy display text (now shows currency)
updateEntropyDisplay :: Text -> Int -> IO ()
updateEntropyDisplay entropyText entropy =
    setText entropyText (toJSString $ "Entropy: " ++ show entropy)

-- | Update selection counter display (fixed at maxSelection)
updateSelectionDisplay :: Text -> Int -> Int -> IO ()
updateSelectionDisplay selText selected _ =
    setText selText (toJSString $ "Selected: " ++ show selected ++ "/" ++ show maxSelection)

-- *****************************************************************************
-- * Round Completion & Game Over
-- *****************************************************************************

-- | Show round completion dialog - awards entropy and shows die reward choice
showRoundComplete :: Container -> Int -> Int -> JSVal
                  -> IORef GameState -> Text -> Text -> Text -> Text -> Text -> Text
                  -> IO ()
showRoundComplete container screenW screenH spritesheetCtx
                  gameStateRef scoreText targetText entropyText deckText selectionText xyText = do
    blipWithFreq 800.0  -- Success sound

    gs <- readIORef gameStateRef

    let currentRound = gs_roundInfo gs
        reward = round_reward currentRound
        newEntropy = gs_entropy gs + reward

    -- Award entropy
    writeIORef gameStateRef $ gs { gs_entropy = newEntropy }

    -- Update entropy display
    setText entropyText (toJSString $ "Entropy: " ++ show newEntropy)

    -- Create modal container
    modalContainer <- newContainer

    -- Dim background with dark tint
    dimOverlay <- newGraphics
    beginFillWithAlpha dimOverlay 0x0a0a15 0.92
    drawRect dimOverlay 0 0 (fromIntegral screenW) (fromIntegral screenH)
    endFill dimOverlay
    setEventMode dimOverlay "static"
    void $ addChild modalContainer dimOverlay

    -- Panel dimensions
    let panelW = 500.0
        panelH = 340.0
        panelX = (fromIntegral screenW - panelW) / 2
        panelY = 50.0

    -- Panel background with Casino Noir styling
    panelBg <- newGraphics
    beginFillWithAlpha panelBg colorPanelDark 0.98
    drawRoundedRect panelBg panelX panelY panelW panelH 16.0
    endFill panelBg
    -- Gold border
    lineStyleWithOptions panelBg 2.0 colorAccentGold 1.0 0.5
    drawRoundedRect panelBg panelX panelY panelW panelH 16.0
    endFill panelBg
    void $ addChild modalContainer panelBg

    -- Title text
    titleText <- newTextWithStyle (toJSString $ "Round " ++ show (round_number currentRound) ++ " Complete!") colorTextGold
    setAnchor titleText 0.5 0.5
    setX titleText (fromIntegral screenW / 2)
    setY titleText (panelY + 40)
    void $ addChild modalContainer titleText

    -- Reward text
    rewardText <- newTextWithStyle (toJSString $ "+" ++ show reward ++ " Entropy") (toJSString "#9b59b6")
    setAnchor rewardText 0.5 0.5
    setX rewardText (fromIntegral screenW / 2)
    setY rewardText (panelY + 80)
    void $ addChild modalContainer rewardText

    -- Die reward section
    rewardLabel <- newTextWithStyle "Choose a die to add:" colorTextPrimary
    setAnchor rewardLabel 0.5 0.5
    setX rewardLabel (fromIntegral screenW / 2)
    setY rewardLabel (panelY + 130)
    void $ addChild modalContainer rewardLabel

    -- Generate 3 random dice options
    let nextDieId = gs_nextDieId gs
    diceOptions <- forM [0, 1, 2] $ \i -> do
        dieTypeRoll <- randomRIO (0, 1) :: IO Int
        let dieType = if dieTypeRoll == 0 then Additive else Multiplicative
        return $ Die (DieId (nextDieId + i)) dieType

    -- Create die option buttons
    let centerX = fromIntegral screenW / 2
        optionSpacing = 150.0
        optionY = panelY + 180
        optionW = 120.0
        optionH = 100.0

    forM_ (zip [0..] diceOptions) $ \(i, dieOption) -> do
        let xPos = centerX + (fromIntegral i - 1) * optionSpacing - optionW / 2

        optionContainer <- newContainer
        setX optionContainer xPos
        setY optionContainer optionY
        setEventMode optionContainer "static"
        setCursor optionContainer "pointer"

        -- Background with themed colors
        let (bgColor, borderColor) = case die_type dieOption of
                Additive -> (0x0a3d1a, colorDiceAdditive)  -- Dark green bg, neon green border
                Multiplicative -> (0x3d1a1a, colorDiceMultiplicative)  -- Dark red bg, coral border
        optionBg <- newGraphics
        beginFillWithAlpha optionBg bgColor 0.9
        drawRoundedRect optionBg 0 0 optionW optionH 10.0
        endFill optionBg
        -- Colored border
        lineStyleWithOptions optionBg 2.0 borderColor 1.0 0.5
        drawRoundedRect optionBg 0 0 optionW optionH 10.0
        endFill optionBg
        void $ addChild optionContainer optionBg

        -- Die type text
        let (typeStr, typeColor) = case die_type dieOption of
                Additive -> ("+X", colorTextSuccess)
                Multiplicative -> ("×Y", colorTextDanger)
        typeText <- newTextWithStyle (toJSString typeStr) typeColor
        setAnchor typeText 0.5 0.5
        setX typeText (optionW / 2)
        setY typeText 35
        void $ addChild optionContainer typeText

        -- Die description
        let descStr = case die_type dieOption of
                Additive -> "Additive Die"
                Multiplicative -> "Mult Die"
        descText <- newTextWithStyle (toJSString descStr) colorTextSecondary
        setAnchor descText 0.5 0.5
        setX descText (optionW / 2)
        setY descText 65
        setScale descText 0.6 0.6
        void $ addChild optionContainer descText

        -- Hover effects
        let optionBgVal = toJSVal optionBg
        on "pointerover" optionContainer =<< jsFuncFromHs_ (\_ -> do
            setScale optionContainer 1.1 1.1
            )
        on "pointerout" optionContainer =<< jsFuncFromHs_ (\_ -> do
            setScale optionContainer 1.0 1.0
            )

        -- Click to add die
        on "pointerdown" optionContainer =<< jsFuncFromHs_ (\_ -> do
            -- Add die to bag
            currentGs <- readIORef gameStateRef
            let newZones = (gs_deckZones currentGs) {
                    dz_bag = dieOption : dz_bag (gs_deckZones currentGs)
                }
            writeIORef gameStateRef $ currentGs {
                gs_deckZones = newZones,
                gs_nextDieId = nextDieId + 3  -- Reserve all 3 IDs
            }
            -- Remove modal and go to shop
            void $ removeChild container modalContainer
            showShop spritesheetCtx container screenW screenH
                     gameStateRef scoreText targetText entropyText deckText selectionText xyText
            )

        void $ addChild modalContainer optionContainer

    -- Skip button with muted styling
    let skipY = panelY + panelH - 60
    createChoiceButton modalContainer (centerX - 60) skipY 120.0 40.0
                      "Skip" colorPanelMid $ do
        -- Skip die reward, go directly to shop
        modifyIORef gameStateRef $ \gst -> gst { gs_nextDieId = nextDieId + 3 }
        void $ removeChild container modalContainer
        showShop spritesheetCtx container screenW screenH
                 gameStateRef scoreText targetText entropyText deckText selectionText xyText

    void $ addChild container modalContainer

-- | Show the shop between rounds (jokers only, StS-style)
showShop :: JSVal -> Container -> Int -> Int
         -> IORef GameState -> Text -> Text -> Text -> Text -> Text -> Text
         -> IO ()
showShop spritesheetCtx container screenW screenH
         gameStateRef scoreText targetText entropyText deckText selectionText xyText = do
    gs <- readIORef gameStateRef

    -- Generate shop contents if not already generated
    shopContents <- case gs_shopContents gs of
        Just sc -> return sc
        Nothing -> do
            sc <- generateShop (gs_jokers gs) (gs_nextDieId gs)
            writeIORef gameStateRef $ gs { gs_shopContents = Just sc }
            return sc

    -- Re-read game state after potential update
    gs' <- readIORef gameStateRef

    -- Create shop container
    shopContainer <- newContainer

    -- Dim background with slight purple tint
    dimOverlay <- newGraphics
    beginFillWithAlpha dimOverlay 0x0a0a15 0.92
    drawRect dimOverlay 0 0 (fromIntegral screenW) (fromIntegral screenH)
    endFill dimOverlay
    setEventMode dimOverlay "static"
    void $ addChild shopContainer dimOverlay

    -- Shop panel dimensions
    let panelW = 550.0
        panelH = 380.0
        panelX = (fromIntegral screenW - panelW) / 2
        panelY = (fromIntegral screenH - panelH) / 2

    -- Panel background with Casino Noir styling
    panelBg <- newGraphics
    beginFillWithAlpha panelBg colorPanelDark 0.98
    drawRoundedRect panelBg panelX panelY panelW panelH 16.0
    endFill panelBg
    lineStyleWithOptions panelBg 2.0 colorAccentGold 1.0 0.5
    drawRoundedRect panelBg panelX panelY panelW panelH 16.0
    void $ addChild shopContainer panelBg

    -- Title with gold accent
    titleText <- newTextWithStyle (toJSString "SHOP") colorTextGold
    setAnchor titleText 0.5 0.5
    setX titleText (fromIntegral screenW / 2)
    setY titleText (panelY + 35)
    setScale titleText 1.1 1.1
    void $ addChild shopContainer titleText

    -- Entropy display in shop
    shopEntropyText <- newTextWithStyle (toJSString $ "Entropy: " ++ show (gs_entropy gs')) colorTextGold
    setAnchor shopEntropyText 0.5 0.5
    setX shopEntropyText (fromIntegral screenW / 2)
    setY shopEntropyText (panelY + 65)
    setScale shopEntropyText 0.8 0.8
    void $ addChild shopContainer shopEntropyText

    -- Joker slots (2 slots centered)
    let boxW = 200.0
        boxH = 110.0
        boxSpacing = 40.0
        totalBoxWidth = boxW * 2 + boxSpacing
        startBoxX = panelX + (panelW - totalBoxWidth) / 2
        boxY = panelY + 95

    -- Render each joker slot
    forM_ [(0 :: Int, shop_joker shopContents, shop_jokerCost shopContents),
           (1, shop_joker2 shopContents, shop_joker2Cost shopContents)] $ \(idx, maybeJoker, cost) -> do
        let boxX = startBoxX + fromIntegral idx * (boxW + boxSpacing)

        jokerBox <- newGraphics
        beginFillWithAlpha jokerBox colorPanelMid 0.9
        drawRoundedRect jokerBox boxX boxY boxW boxH 10.0
        endFill jokerBox
        lineStyleWithOptions jokerBox 1.5 colorAccentPurple 0.8 0.5
        drawRoundedRect jokerBox boxX boxY boxW boxH 10.0
        void $ addChild shopContainer jokerBox

        case maybeJoker of
            Just joker -> do
                jokerName <- newTextWithStyle (toJSString $ joker_name joker) colorTextPrimary
                setAnchor jokerName 0.5 0.5
                setX jokerName (boxX + boxW / 2)
                setY jokerName (boxY + 28)
                setScale jokerName 0.75 0.75
                void $ addChild shopContainer jokerName

                jokerDesc <- newTextWithStyle (toJSString $ joker_description joker) colorTextSecondary
                setAnchor jokerDesc 0.5 0.5
                setX jokerDesc (boxX + boxW / 2)
                setY jokerDesc (boxY + 55)
                setScale jokerDesc 0.5 0.5
                void $ addChild shopContainer jokerDesc

                let costStr = "€" ++ show cost
                jokerCostText <- newTextWithStyle (toJSString costStr) colorTextGold
                setAnchor jokerCostText 0.5 0.5
                setX jokerCostText (boxX + boxW / 2)
                setY jokerCostText (boxY + 82)
                setScale jokerCostText 0.9 0.9
                void $ addChild shopContainer jokerCostText

                -- Buy button with success green
                createChoiceButton shopContainer (boxX + boxW / 2 - 45) (boxY + boxH + 8) 90.0 32.0
                                  "Buy" colorDiceAdditive $ do
                    currentGs <- readIORef gameStateRef
                    when (gs_entropy currentGs >= cost && length (gs_jokers currentGs) < 5) $ do
                        -- Read current shop contents from state (not captured variable)
                        case gs_shopContents currentGs of
                            Just currentShopContents -> do
                                let newShopContents = if idx == 0
                                        then currentShopContents { shop_joker = Nothing }
                                        else currentShopContents { shop_joker2 = Nothing }
                                writeIORef gameStateRef $ currentGs {
                                    gs_entropy = gs_entropy currentGs - cost,
                                    gs_jokers = gs_jokers currentGs ++ [joker],
                                    gs_shopContents = Just newShopContents
                                }
                                setText shopEntropyText (toJSString $ "Entropy: " ++ show (gs_entropy currentGs - cost))
                                setText entropyText (toJSString $ "Entropy: " ++ show (gs_entropy currentGs - cost))
                                -- Refresh shop display
                                void $ removeChild container shopContainer
                                showShop spritesheetCtx container screenW screenH
                                         gameStateRef scoreText targetText entropyText deckText selectionText xyText
                            Nothing -> return ()  -- Shop not open, shouldn't happen

            Nothing -> do
                soldOutText <- newTextWithStyle "SOLD" colorTextSecondary
                setAnchor soldOutText 0.5 0.5
                setX soldOutText (boxX + boxW / 2)
                setY soldOutText (boxY + boxH / 2)
                setScale soldOutText 0.8 0.8
                void $ addChild shopContainer soldOutText

    -- Reroll button (centered below joker slots) - purple accent
    let rerollY = boxY + boxH + 55
    createChoiceButton shopContainer (fromIntegral screenW / 2 - 70) rerollY 140.0 35.0
                      ("Reroll (€" ++ show (shop_rerollCost shopContents) ++ ")") colorAccentPurple $ do
        currentGs <- readIORef gameStateRef
        when (gs_entropy currentGs >= shop_rerollCost shopContents) $ do
            newShop <- generateShop (gs_jokers currentGs) (gs_nextDieId currentGs)
            writeIORef gameStateRef $ currentGs {
                gs_entropy = gs_entropy currentGs - shop_rerollCost shopContents,
                gs_shopContents = Just newShop
            }
            setText shopEntropyText (toJSString $ "Entropy: " ++ show (gs_entropy currentGs - shop_rerollCost shopContents))
            setText entropyText (toJSString $ "Entropy: " ++ show (gs_entropy currentGs - shop_rerollCost shopContents))
            void $ removeChild container shopContainer
            showShop spritesheetCtx container screenW screenH
                     gameStateRef scoreText targetText entropyText deckText selectionText xyText

    -- Next Round button (bottom center) - gold accent for primary action
    createChoiceButton shopContainer (fromIntegral screenW / 2 - 80) (panelY + panelH - 55) 160.0 42.0
                      "Next Round" colorAccentGold $ do
        void $ removeChild container shopContainer
        -- Clear shop contents for next time
        modifyIORef gameStateRef $ \gst -> gst { gs_shopContents = Nothing }
        progressToNextRound spritesheetCtx container screenW screenH
                           gameStateRef scoreText targetText entropyText deckText selectionText xyText

    void $ addChild container shopContainer

-- | Progress to the next round
progressToNextRound :: JSVal -> Container -> Int -> Int
                    -> IORef GameState -> Text -> Text -> Text -> Text -> Text -> Text
                    -> IO ()
progressToNextRound spritesheetCtx container screenW screenH
                    gameStateRef scoreText targetText entropyText deckText selectionText xyText = do
    gs <- readIORef gameStateRef

    let currentRound = gs_roundInfo gs
        nextRoundNum = round_number currentRound + 1

    if nextRoundNum > 15  -- Win after beating 15 rounds
        then showVictory container screenW screenH
        else do
            -- Progress to next round
            let newRound = generateRound nextRoundNum

            -- Re-render joker slots (jokers may have been purchased in shop)
            case gs_jokerContainer gs of
                Just oldJokerCont -> void $ removeChild container oldJokerCont
                Nothing -> return ()
            newJokerCont <- renderJokerSlots container screenW (gs_jokers gs)

            writeIORef gameStateRef $ gs {
                gs_roundInfo = newRound,
                gs_score = 0,
                gs_target = round_target newRound,
                gs_rollsRemaining = rollsPerRound,
                gs_jokerContainer = Just newJokerCont
            }
            setText scoreText (toJSString "Score: 0")
            setText targetText (toJSString $ "Target: " ++ show (round_target newRound))
            updateEntropyDisplay entropyText (gs_entropy gs)
            startNewRound spritesheetCtx container screenW screenH
                         gameStateRef scoreText targetText entropyText deckText selectionText xyText

-- | Show game over screen (ran out of rolls)
showGameOver :: Container -> Int -> Int -> IO ()
showGameOver container screenW screenH = do
    blipWithFreq 200.0  -- Failure sound

    -- Create modal container
    modalContainer <- newContainer

    -- Dim background with red tint
    dimOverlay <- newGraphics
    beginFillWithAlpha dimOverlay 0x1a0505 0.92
    drawRect dimOverlay 0 0 (fromIntegral screenW) (fromIntegral screenH)
    endFill dimOverlay
    setEventMode dimOverlay "static"
    void $ addChild modalContainer dimOverlay

    -- Dialog box dimensions
    let dialogW = 420.0
        dialogH = 200.0
        dialogX = (fromIntegral screenW - dialogW) / 2
        dialogY = (fromIntegral screenH - dialogH) / 2

    -- Dialog background with Casino Noir styling
    dialogBg <- newGraphics
    beginFillWithAlpha dialogBg colorPanelDark 0.98
    drawRoundedRect dialogBg dialogX dialogY dialogW dialogH 14.0
    endFill dialogBg
    lineStyleWithOptions dialogBg 2.0 colorAccentRed 1.0 0.5
    drawRoundedRect dialogBg dialogX dialogY dialogW dialogH 14.0
    void $ addChild modalContainer dialogBg

    -- Title text with red accent
    titleText <- newTextWithStyle (toJSString "GAME OVER") colorTextDanger
    setAnchor titleText 0.5 0.5
    setX titleText (fromIntegral screenW / 2)
    setY titleText (dialogY + 55)
    setScale titleText 1.2 1.2
    void $ addChild modalContainer titleText

    -- Subtitle
    subtitleText <- newTextWithStyle (toJSString "Out of rolls!") colorTextSecondary
    setAnchor subtitleText 0.5 0.5
    setX subtitleText (fromIntegral screenW / 2)
    setY subtitleText (dialogY + 100)
    setScale subtitleText 0.8 0.8
    void $ addChild modalContainer subtitleText

    -- Restart hint
    restartText <- newTextWithStyle (toJSString "Press Menu to return") colorTextSecondary
    setAnchor restartText 0.5 0.5
    setX restartText (fromIntegral screenW / 2)
    setY restartText (dialogY + 145)
    setScale restartText 0.7 0.7
    void $ addChild modalContainer restartText

    void $ addChild container modalContainer

-- | Show victory screen (beat all 15 rounds)
showVictory :: Container -> Int -> Int -> IO ()
showVictory container screenW screenH = do
    blipWithFreq 1200.0  -- Victory sound

    -- Create modal container
    modalContainer <- newContainer

    -- Dim background with golden tint for victory
    dimOverlay <- newGraphics
    beginFillWithAlpha dimOverlay 0x1a1505 0.88
    drawRect dimOverlay 0 0 (fromIntegral screenW) (fromIntegral screenH)
    endFill dimOverlay
    setEventMode dimOverlay "static"
    void $ addChild modalContainer dimOverlay

    -- Dialog box dimensions
    let dialogW = 440.0
        dialogH = 200.0
        dialogX = (fromIntegral screenW - dialogW) / 2
        dialogY = (fromIntegral screenH - dialogH) / 2

    -- Dialog background with Casino Noir styling and gold accent
    dialogBg <- newGraphics
    beginFillWithAlpha dialogBg colorPanelDark 0.98
    drawRoundedRect dialogBg dialogX dialogY dialogW dialogH 14.0
    endFill dialogBg
    lineStyleWithOptions dialogBg 3.0 colorAccentGoldBright 1.0 0.5
    drawRoundedRect dialogBg dialogX dialogY dialogW dialogH 14.0
    void $ addChild modalContainer dialogBg

    -- Title text with gold
    titleText <- newTextWithStyle (toJSString "VICTORY!") colorTextGold
    setAnchor titleText 0.5 0.5
    setX titleText (fromIntegral screenW / 2)
    setY titleText (dialogY + 55)
    setScale titleText 1.3 1.3
    void $ addChild modalContainer titleText

    -- Subtitle
    subtitleText <- newTextWithStyle (toJSString "You beat all 15 rounds!") colorTextSuccess
    setAnchor subtitleText 0.5 0.5
    setX subtitleText (fromIntegral screenW / 2)
    setY subtitleText (dialogY + 100)
    setScale subtitleText 0.85 0.85
    void $ addChild modalContainer subtitleText

    -- Return hint
    returnText <- newTextWithStyle (toJSString "Press Menu to play again") colorTextSecondary
    setAnchor returnText 0.5 0.5
    setX returnText (fromIntegral screenW / 2)
    setY returnText (dialogY + 145)
    setScale returnText 0.7 0.7
    void $ addChild modalContainer returnText

    void $ addChild container modalContainer

-- *****************************************************************************
-- * Hand Display UI
-- *****************************************************************************

-- | Render the hand of drawn dice at the bottom of the screen
renderHandDisplay :: JSVal -> Container -> Int -> Int -> IORef GameState
                  -> Text -> IO ()
renderHandDisplay spritesheetCtx parent screenW screenH gsRef selectionText = do
    gs <- readIORef gsRef

    -- Remove old hand container if exists
    case gs_handContainer gs of
        Just oldContainer -> void $ removeChild parent oldContainer
        Nothing -> return ()

    -- Create new hand container
    handContainer <- newContainer
    let handY = fromIntegral screenH - 280.0  -- Above the roll button
    setY handContainer handY

    -- Get hand dice
    let handDice = dz_hand $ gs_deckZones gs
        numDice = length handDice
        dieSpacing = 110.0  -- Pixels between dice
        totalWidth = fromIntegral (max 0 (numDice - 1)) * dieSpacing
        startX = fromIntegral screenW / 2.0 - totalWidth / 2.0

    -- Create a sprite for each die in hand
    spriteRefs <- forM (zip [0..] handDice) $ \(idx, die) -> do
        let xPos = startX + fromIntegral idx * dieSpacing
        sprite <- createHandDieSprite spritesheetCtx die xPos 0.0 gsRef selectionText
        void $ addChild handContainer sprite
        return (die_id die, toJSVal sprite)

    void $ addChild parent handContainer

    -- Update game state with sprite references
    modifyIORef gsRef $ \s -> s {
        gs_handSprites = spriteRefs,
        gs_handContainer = Just handContainer
    }

    -- Update selection display
    let numSelected = Set.size (ss_selected $ gs_selection gs)
    updateSelectionDisplay selectionText numSelected (gs_entropy gs)

-- | Create a clickable die sprite for the hand
createHandDieSprite :: JSVal -> Die -> Float -> Float -> IORef GameState -> Text -> IO Container
createHandDieSprite spritesheetCtx die xPos yPos gsRef selectionText = do
    -- Create container for die + selection indicator
    dieContainer <- newContainer
    setX dieContainer xPos
    setY dieContainer yPos

    -- Get animation frames with random face (1-6) and random variant (0-2)
    randomFace <- randomRIO (1, 6) :: IO Int
    randomVariant <- randomRIO (0, 2) :: IO Int
    framesArray <- getAnimationFrames spritesheetCtx randomFace randomVariant

    -- Create sprite from frames
    spriteVal <- newAnimatedSpriteFromJSArray framesArray
    let sprite = fromJSVal spriteVal :: Sprite

    setAnchor sprite 0.5 0.5
    setScale sprite 0.9 0.9  -- Slightly smaller in hand

    -- Color based on type - Casino Noir theme
    let tintColor = case die_type die of
            Additive -> colorDiceAdditive       -- Neon green for additive
            Multiplicative -> colorDiceMultiplicative  -- Coral red for multiplicative
    setTint sprite tintColor

    -- Apply negative filter for multiplicative dice (like in dice roll)
    when (die_type die == Multiplicative) $ do
        negFilter <- newColorMatrixFilter
        colorMatrixNegative negFilter False
        let ColorMatrixFilter f = negFilter
        addFilter spriteVal f

    -- Selection indicator (circle behind die) - add FIRST so it's behind sprite
    selectionIndicator <- newGraphics
    -- Gold glow for selected dice
    beginFillWithAlpha selectionIndicator colorAccentGoldBright 0.6
    drawCircle selectionIndicator 0 0 55   -- Circle around die
    endFill selectionIndicator
    setAlpha selectionIndicator 0.0        -- Hidden by default
    void $ addChild dieContainer selectionIndicator  -- Add first (will be behind)

    -- Add sprite on top of indicator
    void $ addChild dieContainer sprite

    -- Make interactive
    setEventMode dieContainer "static"
    setCursor dieContainer "pointer"

    -- Store refs for updating visuals
    let indicatorVal = toJSVal selectionIndicator
        spriteContainerVal = toJSVal dieContainer

    -- Click handler
    on "pointerdown" dieContainer =<< jsFuncFromHs_ (\_ -> do
        isSelected <- toggleDieSelection (die_id die) gsRef
        -- Update visual
        if isSelected
            then do
                setAlpha (fromJSVal indicatorVal :: Graphics) 0.5
                setScale (fromJSVal spriteContainerVal :: Container) 1.0 1.0
            else do
                setAlpha (fromJSVal indicatorVal :: Graphics) 0.0
                setScale (fromJSVal spriteContainerVal :: Container) 0.9 0.9
        -- Update selection counter
        gs <- readIORef gsRef
        let numSelected = Set.size (ss_selected $ gs_selection gs)
        updateSelectionDisplay selectionText numSelected (gs_entropy gs)
        )

    -- Create tooltip for die type info (hidden by default)
    let (dieTypeName, tooltipColor) = case die_type die of
            Additive -> ("Additive (X)", colorTextSuccess)
            Multiplicative -> ("Mult (Y)", colorTextDanger)
    tooltip <- newContainer
    setY tooltip (-60)  -- Position above the die

    tooltipBg <- newGraphics
    beginFillWithAlpha tooltipBg colorPanelDark 0.95
    drawRoundedRect tooltipBg (-55) (-15) 110 30 6.0
    endFill tooltipBg
    lineStyleWithOptions tooltipBg 1.0 colorAccentGold 0.8 0.5
    drawRoundedRect tooltipBg (-55) (-15) 110 30 6.0
    void $ addChild tooltip tooltipBg

    tooltipText <- newTextWithStyle (toJSString dieTypeName) tooltipColor
    setAnchor tooltipText 0.5 0.5
    setScale tooltipText 0.55 0.55
    void $ addChild tooltip tooltipText

    setAlpha tooltip 0.0  -- Hidden by default
    void $ addChild dieContainer tooltip

    let tooltipVal = toJSVal tooltip

    -- Hover effects
    on "pointerover" dieContainer =<< jsFuncFromHs_ (\_ -> do
        gs <- readIORef gsRef
        let isSelected = Set.member (die_id die) (ss_selected $ gs_selection gs)
        if isSelected
            then setScale (fromJSVal spriteContainerVal :: Container) 1.05 1.05
            else setScale (fromJSVal spriteContainerVal :: Container) 0.95 0.95
        -- Show tooltip
        setAlpha (fromJSVal tooltipVal :: Container) 1.0
        )

    on "pointerout" dieContainer =<< jsFuncFromHs_ (\_ -> do
        gs <- readIORef gsRef
        let isSelected = Set.member (die_id die) (ss_selected $ gs_selection gs)
        if isSelected
            then setScale (fromJSVal spriteContainerVal :: Container) 1.0 1.0
            else setScale (fromJSVal spriteContainerVal :: Container) 0.9 0.9
        -- Hide tooltip
        setAlpha (fromJSVal tooltipVal :: Container) 0.0
        )

    return dieContainer

-- *****************************************************************************
-- * Joker UI
-- *****************************************************************************

-- | Render the joker slots at the top of the screen
renderJokerSlots :: Container -> Int -> [Joker] -> IO Container
renderJokerSlots parent screenW jokers = do
    jokerContainer <- newContainer
    setY jokerContainer 15.0  -- Near top of screen

    let maxJokers = 5
        slotWidth = 110.0
        slotHeight = 70.0
        spacing = 12.0
        totalWidth = fromIntegral maxJokers * slotWidth + fromIntegral (maxJokers - 1) * spacing
        startX = (fromIntegral screenW - totalWidth) / 2.0

    -- Render each slot (0 to 4)
    forM_ [0..maxJokers-1] $ \idx -> do
        let xPos = startX + fromIntegral idx * (slotWidth + spacing)
            maybeJoker = if idx < length jokers then Just (jokers !! idx) else Nothing

        slotContainer <- newContainer
        setX slotContainer xPos

        -- Card-style slot background with layered effects
        slotBg <- newGraphics
        case maybeJoker of
            Just _ -> do
                -- Filled slot: rich purple with gold border
                beginFillWithAlpha slotBg colorAccentPurple 0.9
                drawRoundedRect slotBg 0 0 slotWidth slotHeight 8.0
                endFill slotBg
                -- Gold border for filled slots
                lineStyleWithOptions slotBg 2.0 colorAccentGold 1.0 0.5
                drawRoundedRect slotBg 0 0 slotWidth slotHeight 8.0
            Nothing -> do
                -- Empty slot: dark with subtle border
                beginFillWithAlpha slotBg colorPanelDark 0.5
                drawRoundedRect slotBg 0 0 slotWidth slotHeight 8.0
                endFill slotBg
                lineStyleWithOptions slotBg 1.0 colorPanelMid 0.6 0.5
                drawRoundedRect slotBg 0 0 slotWidth slotHeight 8.0
        void $ addChild slotContainer slotBg

        -- Joker name or empty label
        case maybeJoker of
            Just joker -> do
                nameText <- newTextWithStyle (toJSString $ joker_name joker) colorTextPrimary
                setAnchor nameText 0.5 0.5
                setX nameText (slotWidth / 2)
                setY nameText (slotHeight / 2)
                setScale nameText 0.6 0.6
                void $ addChild slotContainer nameText

                -- Make interactive for hover tooltip
                setEventMode slotContainer "static"
                setCursor slotContainer "pointer"

                -- Create tooltip (positioned below slot)
                tooltip <- newContainer
                setX tooltip (slotWidth / 2)
                setY tooltip (slotHeight + 8)

                tooltipBg <- newGraphics
                beginFillWithAlpha tooltipBg colorPanelDark 0.95
                drawRoundedRect tooltipBg (-110) (-8) 220 40 6.0
                endFill tooltipBg
                lineStyleWithOptions tooltipBg 1.0 colorAccentGold 0.8 0.5
                drawRoundedRect tooltipBg (-110) (-8) 220 40 6.0
                void $ addChild tooltip tooltipBg

                tooltipText <- newTextWithStyle (toJSString $ joker_description joker) colorTextGold
                setAnchor tooltipText 0.5 0.5
                setY tooltipText 12
                setScale tooltipText 0.55 0.55
                void $ addChild tooltip tooltipText

                setAlpha tooltip 0.0  -- Hidden by default
                void $ addChild slotContainer tooltip

                let tooltipVal = toJSVal tooltip
                    slotBgVal = toJSVal slotBg

                -- Hover effects - brighten and glow
                on "pointerover" slotContainer =<< jsFuncFromHs_ (\_ -> do
                    setAlpha (fromJSVal tooltipVal :: Container) 1.0
                    -- Brighten the slot with gold highlight
                    beginFillWithAlpha (fromJSVal slotBgVal :: Graphics) 0xb366d4 1.0
                    drawRoundedRect (fromJSVal slotBgVal :: Graphics) 0 0 slotWidth slotHeight 8.0
                    endFill (fromJSVal slotBgVal :: Graphics)
                    lineStyleWithOptions (fromJSVal slotBgVal :: Graphics) 3.0 colorAccentGoldBright 1.0 0.5
                    drawRoundedRect (fromJSVal slotBgVal :: Graphics) 0 0 slotWidth slotHeight 8.0
                    )

                on "pointerout" slotContainer =<< jsFuncFromHs_ (\_ -> do
                    setAlpha (fromJSVal tooltipVal :: Container) 0.0
                    -- Reset to normal purple
                    beginFillWithAlpha (fromJSVal slotBgVal :: Graphics) colorAccentPurple 0.9
                    drawRoundedRect (fromJSVal slotBgVal :: Graphics) 0 0 slotWidth slotHeight 8.0
                    endFill (fromJSVal slotBgVal :: Graphics)
                    lineStyleWithOptions (fromJSVal slotBgVal :: Graphics) 2.0 colorAccentGold 1.0 0.5
                    drawRoundedRect (fromJSVal slotBgVal :: Graphics) 0 0 slotWidth slotHeight 8.0
                    )

            Nothing -> do
                emptyText <- newTextWithStyle (toJSString "Empty") colorTextSecondary
                setAnchor emptyText 0.5 0.5
                setX emptyText (slotWidth / 2)
                setY emptyText (slotHeight / 2)
                setScale emptyText 0.55 0.55
                void $ addChild slotContainer emptyText

        void $ addChild jokerContainer slotContainer

    void $ addChild parent jokerContainer
    return jokerContainer

-- *****************************************************************************
-- * Screen Rendering Functions
-- *****************************************************************************

-- | Render the game screen (the main gameplay)
renderGameScreen :: JSVal -> Application -> Container -> Int -> Int -> IORef GameState
                 -> (IO ())  -- ^ Action to show pause menu
                 -> IO ()
renderGameScreen spritesheetCtx app screenContainer screen_width screen_height game_state_ref showPauseMenu = do
    clearScreen screenContainer

    -- Clear stale container references (they were removed by clearScreen)
    modifyIORef game_state_ref $ \gs -> gs {
        gs_handContainer = Nothing,
        gs_handSprites = [],
        gs_persistentSprites = [],
        gs_jokerContainer = Nothing
    }

    GameState{..} <- readIORef game_state_ref

    -- Render joker slots at the top
    jokerCont <- renderJokerSlots screenContainer screen_width gs_jokers
    modifyIORef game_state_ref $ \gs -> gs { gs_jokerContainer = Just jokerCont }

    -- ═══════════════════════════════════════════════════════════════════════════
    -- INFO BAR: Round and Rolls (horizontal strip below jokers)
    -- ═══════════════════════════════════════════════════════════════════════════
    let currentRound = gs_roundInfo
        infoBarY = 98.0

    -- Round indicator (prominent, left-center)
    round_text <- newTextWithStyle (toJSString $
        "Round " ++ show (round_number currentRound)) colorTextGold
    setX round_text (fromIntegral screen_width / 2.0)
    setY round_text infoBarY
    setAnchor round_text 0.5 0.5
    setScale round_text 1.0 1.0
    void $ addChild screenContainer round_text

    -- Rolls remaining (below round, subtle red accent)
    rolls_text <- newTextWithStyle (toJSString $
        "Rolls: " ++ show gs_rollsRemaining ++ "/" ++ show rollsPerRound) colorTextDanger
    setX rolls_text (fromIntegral screen_width / 2.0)
    setY rolls_text (infoBarY + 28.0)
    setAnchor rolls_text 0.5 0.5
    setScale rolls_text 0.7 0.7
    void $ addChild screenContainer rolls_text

    -- ═══════════════════════════════════════════════════════════════════════════
    -- CENTRAL STATS PANEL: Clean, centered information display
    -- ═══════════════════════════════════════════════════════════════════════════
    let panelW = 280.0
        panelH = 180.0
        panelX = (fromIntegral screen_width - panelW) / 2.0
        panelY = fromIntegral screen_height / 2.0 - 105.0

    -- Panel background with subtle gradient effect
    statsPanelBg <- newGraphics
    beginFillWithAlpha statsPanelBg colorPanelDark 0.85
    drawRoundedRect statsPanelBg panelX panelY panelW panelH 14.0
    endFill statsPanelBg
    lineStyleWithOptions statsPanelBg 1.5 colorAccentGold 0.6 0.5
    drawRoundedRect statsPanelBg panelX panelY panelW panelH 14.0
    void $ addChild screenContainer statsPanelBg

    -- Target (top of panel, prominent)
    target_text <- newTextWithStyle (toJSString $ "Target: " ++ show gs_target) colorTextPrimary
    setX target_text (fromIntegral screen_width / 2.0)
    setY target_text (panelY + 30.0)
    setAnchor target_text 0.5 0.5
    setScale target_text 0.85 0.85
    void $ addChild screenContainer target_text

    -- Score (below target)
    score_text <- newTextWithStyle (toJSString $ "Score: " ++ show gs_score) colorTextPrimary
    setX score_text (fromIntegral screen_width / 2.0)
    setY score_text (panelY + 60.0)
    setAnchor score_text 0.5 0.5
    setScale score_text 0.85 0.85
    void $ addChild screenContainer score_text

    -- Entropy display (gold accent for currency)
    entropy_text <- newTextWithStyle (toJSString $ "Entropy: " ++ show gs_entropy) colorTextGold
    setX entropy_text (fromIntegral screen_width / 2.0)
    setY entropy_text (panelY + 92.0)
    setAnchor entropy_text 0.5 0.5
    setScale entropy_text 0.75 0.75
    void $ addChild screenContainer entropy_text

    -- Deck zones display (bag/discard counts - subdued)
    let bagCount = length (dz_bag gs_deckZones)
        discardCount = length (dz_discard gs_deckZones)
    deck_text <- newTextWithStyle (toJSString $
        "Bag: " ++ show bagCount ++ " | Discard: " ++ show discardCount) colorTextSecondary
    setX deck_text (fromIntegral screen_width / 2.0)
    setY deck_text (panelY + 120.0)
    setAnchor deck_text 0.5 0.5
    setScale deck_text 0.65 0.65
    void $ addChild screenContainer deck_text

    -- Selection counter (green for actionable info)
    let numSelected = Set.size (ss_selected gs_selection)
    selection_text <- newTextWithStyle (toJSString $
        "Selected: " ++ show numSelected ++ "/" ++ show maxSelection) colorTextSuccess
    setX selection_text (fromIntegral screen_width / 2.0)
    setY selection_text (panelY + 150.0)
    setAnchor selection_text 0.5 0.5
    setScale selection_text 0.75 0.75
    void $ addChild screenContainer selection_text

    -- X * Y = score display (appears during roll resolution)
    xy_text <- newTextWithStyle "" colorTextSecondary
    setX xy_text (fromIntegral screen_width / 2.0)
    setY xy_text (panelY + panelH + 20.0)
    setAnchor xy_text 0.5 0.5
    setScale xy_text 0.7 0.7
    void $ addChild screenContainer xy_text

    -- ═══════════════════════════════════════════════════════════════════════════
    -- ACTION BUTTONS: Roll (hero) + Menu (subtle)
    -- ═══════════════════════════════════════════════════════════════════════════
    let actionAreaY = fromIntegral screen_height - 115.0

    -- Action button container with subtle background
    actionBg <- newGraphics
    beginFillWithAlpha actionBg colorPanelMid 0.5
    drawRoundedRect actionBg (fromIntegral screen_width / 2.0 - 70.0) (actionAreaY - 30.0) 140.0 130.0 12.0
    endFill actionBg
    lineStyleWithOptions actionBg 1.0 colorAccentGold 0.3 0.5
    drawRoundedRect actionBg (fromIntegral screen_width / 2.0 - 70.0) (actionAreaY - 30.0) 140.0 130.0 12.0
    void $ addChild screenContainer actionBg

    -- Roll button (hero - prominent gold accent)
    let roll_button = Button {
        button_text = "Roll",
        button_x = fromIntegral screen_width / 2.0,
        button_y = actionAreaY,
        button_width = 100.0,
        button_height = 50.0,
        button_color = "black",
        button_on_click = \_ -> rollSelectedDice spritesheetCtx app screenContainer screen_width screen_height
                                                 game_state_ref score_text target_text entropy_text deck_text selection_text xy_text
    }
    renderButton screenContainer roll_button

    -- Menu button (subdued, below roll)
    let menu_button = Button {
        button_text = "Menu",
        button_x = fromIntegral screen_width / 2.0,
        button_y = actionAreaY + 60.0,
        button_width = 80.0,
        button_height = 35.0,
        button_color = "black",
        button_on_click = \_ -> showPauseMenu
    }
    renderButton screenContainer menu_button

    -- Shuffle the bag and start the first round (draw initial hand)
    shuffledBag <- shuffleList (dz_bag gs_deckZones)
    modifyIORef game_state_ref $ \gst -> gst { gs_deckZones = gs_deckZones { dz_bag = shuffledBag } }
    startNewRound spritesheetCtx screenContainer screen_width screen_height
                 game_state_ref score_text target_text entropy_text deck_text selection_text xy_text

-- | Render the pause menu screen
renderPauseMenu :: Application -> Container -> Int -> Int
                -> (IO ())  -- ^ Action to continue game
                -> (IO ())  -- ^ Action to quit to start screen
                -> IO ()
renderPauseMenu _app screenContainer screen_width screen_height continueGame quitGame = do
    clearScreen screenContainer

    -- Background panel
    bgPanel <- newGraphics
    beginFillWithAlpha bgPanel colorPanelDark 0.95
    drawRoundedRect bgPanel (fromIntegral screen_width / 2 - 150) 180 300 280 15.0
    endFill bgPanel
    lineStyleWithOptions bgPanel 2.0 colorAccentGold 1.0 0.5
    drawRoundedRect bgPanel (fromIntegral screen_width / 2 - 150) 180 300 280 15.0
    void $ addChild screenContainer bgPanel

    -- Title
    title <- newTextWithStyle (toJSString "Paused") colorTextGold
    setX title (fromIntegral screen_width / 2.0)
    setY title 230.0
    setAnchor title 0.5 0.5
    setScale title 1.2 1.2
    void $ addChild screenContainer title

    -- Menu
    let pauseMenu = Menu {
        menu_items = [
            MenuItem { menuItem_text = "Continue", menuItem_action = continueGame },
            MenuItem { menuItem_text = "Quit", menuItem_action = quitGame }
        ],
        menu_x = fromIntegral screen_width / 2.0,
        menu_y = fromIntegral screen_height / 2.0,
        menu_spacing = 70.0,
        menu_color = colorTextPrimary,
        menu_hoverColor = colorTextGold
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

    -- Decorative background panel
    bgPanel <- newGraphics
    beginFillWithAlpha bgPanel colorPanelDark 0.6
    drawRoundedRect bgPanel (fromIntegral screen_width / 2 - 200) 100 400 500 20.0
    endFill bgPanel
    -- Gold border
    lineStyleWithOptions bgPanel 2.0 colorAccentGold 1.0 0.5
    drawRoundedRect bgPanel (fromIntegral screen_width / 2 - 200) 100 400 500 20.0
    void $ addChild screenContainer bgPanel

    -- Title with gold color
    title <- newTextWithStyle (toJSString "A Game of Chance") colorTextGold
    setX title (fromIntegral screen_width / 2.0)
    setY title 180.0
    setAnchor title 0.5 0.5
    setScale title 1.3 1.3
    void $ addChild screenContainer title

    -- Subtitle
    subtitle <- newTextWithStyle (toJSString "Dice Roguelike") colorTextSecondary
    setX subtitle (fromIntegral screen_width / 2.0)
    setY subtitle 230.0
    setAnchor subtitle 0.5 0.5
    setScale subtitle 0.7 0.7
    void $ addChild screenContainer subtitle

    -- Menu with Casino Noir styling
    let startMenu = Menu {
        menu_items = [
            MenuItem { menuItem_text = "Start Game", menuItem_action = showGame },
            MenuItem { menuItem_text = "Options", menuItem_action = showOptions },
            MenuItem { menuItem_text = "Exit", menuItem_action = closeWindow }
        ],
        menu_x = fromIntegral screen_width / 2.0,
        menu_y = fromIntegral screen_height / 2.0 + 20,
        menu_spacing = 70.0,
        menu_color = colorTextPrimary,
        menu_hoverColor = colorTextGold
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

    -- Background panel
    bgPanel <- newGraphics
    beginFillWithAlpha bgPanel colorPanelDark 0.6
    drawRoundedRect bgPanel (fromIntegral screen_width / 2 - 200) 100 400 400 20.0
    endFill bgPanel
    lineStyleWithOptions bgPanel 2.0 colorAccentGold 1.0 0.5
    drawRoundedRect bgPanel (fromIntegral screen_width / 2 - 200) 100 400 400 20.0
    void $ addChild screenContainer bgPanel

    -- Title
    title <- newTextWithStyle (toJSString "Options") colorTextGold
    setX title (fromIntegral screen_width / 2.0)
    setY title 180.0
    setAnchor title 0.5 0.5
    setScale title 1.2 1.2
    void $ addChild screenContainer title

    -- Placeholder text (no settings currently)
    noSettings <- newTextWithStyle (toJSString "(No settings available)") colorTextSecondary
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
        menu_color = colorTextPrimary,
        menu_hoverColor = colorTextGold
    }

    menuContainer <- renderMenu backMenu
    void $ addChild screenContainer menuContainer
    return ()

-- | Render a loading screen
renderLoadingScreen :: Container -> Int -> Int -> IO ()
renderLoadingScreen screenContainer screen_width screen_height = do
    clearScreen screenContainer

    -- Loading text with gold styling
    loadingText <- newTextWithStyle (toJSString "Loading...") colorTextGold
    setX loadingText (fromIntegral screen_width / 2.0)
    setY loadingText (fromIntegral screen_height / 2.0)
    setAnchor loadingText 0.5 0.5
    void $ addChild screenContainer loadingText

-- *****************************************************************************
-- * Design System - Casino Noir Theme
-- *****************************************************************************

-- Color palette for the Casino Noir theme
colorBackground :: Int
colorBackground = 0x0f0f1a  -- Deep space black with blue tint

colorPanelDark :: Int
colorPanelDark = 0x1a1a2e  -- Dark navy panel

colorPanelMid :: Int
colorPanelMid = 0x252545  -- Medium panel

colorAccentGold :: Int
colorAccentGold = 0xd4a937  -- Warm gold

colorAccentGoldBright :: Int
colorAccentGoldBright = 0xffd700  -- Bright gold

colorAccentPurple :: Int
colorAccentPurple = 0x9b59b6  -- Rich purple

colorAccentRed :: Int
colorAccentRed = 0xe94560  -- Vibrant red/coral

colorDiceAdditive :: Int
colorDiceAdditive = 0x00ff88  -- Neon green

colorDiceMultiplicative :: Int
colorDiceMultiplicative = 0xff6b6b  -- Coral red

colorTextPrimary :: JSString
colorTextPrimary = toJSString "#eaeaea"  -- Off-white

colorTextSecondary :: JSString
colorTextSecondary = toJSString "#a0a0a0"  -- Muted gray

colorTextGold :: JSString
colorTextGold = toJSString "#d4a937"  -- Gold text

colorTextSuccess :: JSString
colorTextSuccess = toJSString "#00ff88"  -- Success green

colorTextDanger :: JSString
colorTextDanger = toJSString "#e94560"  -- Danger red

main :: IO ()
main = do
    -- Initialize PIXI application with dark casino background
    app <- newApp >>= flip initApp "#0f0f1a"
    appendCanvas app

    -- Use fixed design resolution for all game logic
    let screen_width = designWidth
        screen_height = designHeight

    -- Set idle FPS to reduce CPU usage when not animating
    appTicker <- getTicker app
    setMaxFPS appTicker idleFPS

    -- Get current window dimensions
    windowW <- getWindowWidth
    windowH <- getWindowHeight

    -- Create viewport container for scaling/rotation to fit window
    stage <- getStage app
    viewportContainer <- newContainer
    void $ addChild stage viewportContainer

    -- Apply initial viewport transformation
    applyViewportTransform viewportContainer windowW windowH

    -- Create screen container that will hold all screen content
    screenContainer <- newContainer
    void $ addChild viewportContainer screenContainer

    -- Store viewport ref for resize handling
    viewportRef <- newIORef viewportContainer

    -- Set up resize handler
    resizeCallback <- jsFuncFromHs_ $ \_ -> do
        viewport <- readIORef viewportRef
        newW <- getWindowWidth
        newH <- getWindowHeight
        applyViewportTransform viewport newW newH
    onWindowResize resizeCallback

    -- Show loading screen immediately
    renderLoadingScreen screenContainer screen_width screen_height

    -- Generate dice spritesheet (async operation)
    spritesheetCtx <- generateDiceSpritesheet

    -- Initialize game state
    game_state_ref <- newIORef initialGameState
    storedState <- getState
    case storedState of
        Just snap -> writeIORef game_state_ref (applySnapshot snap initialGameState)
        Nothing -> return ()
    persistSnapshot game_state_ref

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

    let renderGame = renderGameScreen spritesheetCtx app screenContainer screen_width screen_height game_state_ref
            showPauseMenu

    let startGame = do
            _ <- applyActionAndStore game_state_ref StartGame
            renderGame

    let resumeGame = do
            _ <- applyActionAndStore game_state_ref ResumeGame
            renderGame

    let quitToStart = do
            _ <- applyActionAndStore game_state_ref QuitToStart
            showStartScreen

    -- Set up the actual screen rendering functions
    writeIORef showStartScreenRef $
        do
            _ <- applyActionAndStore game_state_ref (SetScreen StartScreen)
            renderStartScreen app screenContainer screen_width screen_height
                startGame showOptionsScreen

    writeIORef showOptionsScreenRef $
        do
            _ <- applyActionAndStore game_state_ref (SetScreen OptionsScreen)
            renderOptionsScreen app screenContainer screen_width screen_height
                showStartScreen

    writeIORef showGameScreenRef resumeGame

    writeIORef showPauseMenuRef $
        do
            _ <- applyActionAndStore game_state_ref PauseGame
            renderPauseMenu app screenContainer screen_width screen_height
                resumeGame quitToStart

    -- Transition from loading to start screen
    showStartScreen
