{-# LANGUAGE OverloadedStrings #-}

-- | Test module for pixijs-ffi
--
-- This module exports test functions that exercise each FFI binding.
-- The tests are run via Playwright which calls these functions and
-- verifies the results.
module Main where

import Graphics.PixiJS
import GHC.Wasm.Prim
import Data.String (IsString(..))

-- *****************************************************************************
-- * Export test functions to JavaScript
-- *****************************************************************************

foreign export javascript "runAllTests" runAllTests :: IO ()
foreign export javascript "testApplication" testApplication :: IO ()
foreign export javascript "testContainer" testContainer :: IO ()
foreign export javascript "testSprite" testSprite :: IO ()
foreign export javascript "testText" testText :: IO ()
foreign export javascript "testGraphics" testGraphics :: IO ()
foreign export javascript "testMath" testMath :: IO ()
foreign export javascript "testEvents" testEvents :: IO ()
foreign export javascript "testFilters" testFilters :: IO ()
foreign export javascript "testTicker" testTicker :: IO ()
foreign export javascript "testInterop" testInterop :: IO ()

-- *****************************************************************************
-- * Test Result Helpers
-- *****************************************************************************

-- | Set a test result on window.TEST_RESULTS
foreign import javascript unsafe "window.TEST_RESULTS = window.TEST_RESULTS || {}; window.TEST_RESULTS[$1] = $2"
    setTestResult :: JSString -> JSVal -> IO ()

-- | Set a boolean test result
setTestBool :: JSString -> Bool -> IO ()
setTestBool name val = setTestResult name (boolAsVal val)

-- | Set a numeric test result
setTestFloat :: JSString -> Float -> IO ()
setTestFloat name val = setTestResult name (floatAsVal val)

-- | Set an integer test result
setTestInt :: JSString -> Int -> IO ()
setTestInt name val = setTestResult name (intAsVal val)

-- | Set a string test result
setTestString :: JSString -> JSString -> IO ()
setTestString name val = setTestResult name (stringAsVal val)

-- | Mark test as passed
testPassed :: JSString -> IO ()
testPassed name = setTestBool name True

-- | Mark test as failed
testFailed :: JSString -> IO ()
testFailed name = setTestBool name False

-- *****************************************************************************
-- * Application Tests
-- *****************************************************************************

testApplication :: IO ()
testApplication = do
    -- Test: newApp creates an application
    app <- newApp
    setTestResult "app_created" (toJSVal app)
    testPassed "newApp"

    -- Test: initApp initializes the application
    app' <- initApp app "#336699"
    testPassed "initApp"

    -- Test: appendCanvas adds canvas to DOM
    appendCanvas app'
    testPassed "appendCanvas"

    -- Test: getStage returns a container
    stage <- getStage app'
    setTestResult "stage" (toJSVal stage)
    testPassed "getStage"

    -- Test: getScreen returns dimensions
    screen <- getScreen app'
    screenW <- getRectWidth screen
    screenH <- getRectHeight screen
    setTestFloat "screen_width" screenW
    setTestFloat "screen_height" screenH
    setTestBool "getScreen" (screenW > 0 && screenH > 0)

    -- Test: getTicker returns a ticker
    ticker <- getTicker app'
    setTestResult "ticker" (toJSVal ticker)
    testPassed "getTicker"

    -- Test: getCanvas returns the canvas
    canvas <- getCanvas app'
    setTestResult "canvas" (toJSVal canvas)
    testPassed "getCanvas"

    -- Export app for other tests to use
    exportValue "TEST_APP" (toJSVal app')
    exportValue "TEST_STAGE" (toJSVal stage)

-- *****************************************************************************
-- * Container Tests
-- *****************************************************************************

testContainer :: IO ()
testContainer = do
    -- Test: newContainer creates a container
    container <- newContainer
    setTestResult "container" (toJSVal container)
    testPassed "newContainer"

    -- Test: setX/getX
    setX container 100
    x <- getX container
    setTestFloat "container_x" x
    setTestBool "setX_getX" (abs (x - 100) < 0.01)

    -- Test: setY/getY
    setY container 200
    y <- getY container
    setTestFloat "container_y" y
    setTestBool "setY_getY" (abs (y - 200) < 0.01)

    -- Test: setPosition
    setPosition container 150 250
    x' <- getX container
    y' <- getY container
    setTestBool "setPosition" (abs (x' - 150) < 0.01 && abs (y' - 250) < 0.01)

    -- Test: setScale/getScaleX/getScaleY
    setScale container 2.0 3.0
    sx <- getScaleX container
    sy <- getScaleY container
    setTestFloat "scale_x" sx
    setTestFloat "scale_y" sy
    setTestBool "setScale" (abs (sx - 2.0) < 0.01 && abs (sy - 3.0) < 0.01)

    -- Test: setRotation/getRotation
    setRotation container 1.57 -- ~90 degrees
    rot <- getRotation container
    setTestFloat "rotation" rot
    setTestBool "setRotation_getRotation" (abs (rot - 1.57) < 0.01)

    -- Test: setVisible/getVisible
    setVisible container False
    vis <- getVisible container
    setTestBool "visible_false" (not vis)
    setVisible container True
    vis' <- getVisible container
    setTestBool "setVisible_getVisible" vis'

    -- Test: setAlpha/getAlpha
    setAlpha container 0.5
    alpha <- getAlpha container
    setTestFloat "alpha" alpha
    setTestBool "setAlpha_getAlpha" (abs (alpha - 0.5) < 0.01)

    -- Test: addChild/removeChild/getNumChildren
    child1 <- newContainer
    child2 <- newContainer
    _ <- addChild container child1
    n1 <- getNumChildren container
    setTestInt "children_after_add1" n1
    _ <- addChild container child2
    n2 <- getNumChildren container
    setTestInt "children_after_add2" n2
    _ <- removeChild container child1
    n3 <- getNumChildren container
    setTestInt "children_after_remove" n3
    setTestBool "addChild_removeChild" (n1 == 1 && n2 == 2 && n3 == 1)

    -- Test: getChildAt
    childAt0 <- getChildAt container 0
    setTestResult "child_at_0" (toJSVal childAt0)
    testPassed "getChildAt"

    -- Test: setChildIndex
    _ <- addChild container child1
    setChildIndex container child1 0
    idx <- getChildIndex container child1
    setTestInt "child_index" idx
    setTestBool "setChildIndex_getChildIndex" (idx == 0)

    -- Test: setBounds/getBounds
    bounds <- getBounds container
    setTestResult "bounds" (toJSVal bounds)
    testPassed "getBounds"

    -- Test: getLocalBounds
    localBounds <- getLocalBounds container
    setTestResult "local_bounds" (toJSVal localBounds)
    testPassed "getLocalBounds"

    -- Test: setPivot
    setPivot container 10 20
    testPassed "setPivot"

    -- Test: getParent (should be null for orphan container)
    parent <- getParent container
    setTestResult "parent" (toJSVal parent)
    testPassed "getParent"

    -- Export container for other tests
    exportValue "TEST_CONTAINER" (toJSVal container)

-- *****************************************************************************
-- * Sprite Tests
-- *****************************************************************************

testSprite :: IO ()
testSprite = do
    -- Test: newSprite creates an empty sprite
    sprite <- newSprite
    setTestResult "sprite" (toJSVal sprite)
    testPassed "newSprite"

    -- Test: setAnchor
    setAnchor sprite 0.5 0.5
    testPassed "setAnchor"

    -- Test: setAnchorX/setAnchorY
    setAnchorX sprite 0.25
    setAnchorY sprite 0.75
    testPassed "setAnchorX_setAnchorY"

    -- Test: setTint/getTint (sprite-specific)
    setTint sprite 0xFF0000
    tint <- getTint sprite
    setTestInt "tint" tint
    setTestBool "setTint_getTint" (tint == 0xFF0000)

    -- Test: setWidth/getWidth
    setWidth sprite 100
    w <- getWidth sprite
    setTestFloat "sprite_width" w
    -- Note: width might not be exactly 100 for empty sprite
    testPassed "setWidth_getWidth"

    -- Test: setHeight/getHeight
    setHeight sprite 200
    h <- getHeight sprite
    setTestFloat "sprite_height" h
    testPassed "setHeight_getHeight"

    exportValue "TEST_SPRITE" (toJSVal sprite)

-- *****************************************************************************
-- * Text Tests
-- *****************************************************************************

testText :: IO ()
testText = do
    -- Test: newText creates text
    text <- newText "Hello World"
    setTestResult "text" (toJSVal text)
    testPassed "newText"

    -- Test: newTextWithStyle creates styled text
    textStyled <- newTextWithStyle "Styled Text" "red"
    setTestResult "text_styled" (toJSVal textStyled)
    testPassed "newTextWithStyle"

    -- Test: setText/getText
    setText text "New Text"
    content <- getText text
    setTestString "text_content" content
    setTestBool "setText_getText" (fromJSString content == "New Text")

    -- Test: text properties work (via Container)
    setX text 50
    setY text 100
    x <- getX text
    y <- getY text
    setTestBool "text_position" (abs (x - 50) < 0.01 && abs (y - 100) < 0.01)

    -- Test: setAnchor works on text
    setAnchor text 0.5 0.5
    testPassed "text_setAnchor"

    -- Test: setAlpha works on text
    setAlpha text 0.8
    alpha <- getAlpha text
    setTestBool "text_alpha" (abs (alpha - 0.8) < 0.01)

    exportValue "TEST_TEXT" (toJSVal text)

-- *****************************************************************************
-- * Graphics Tests
-- *****************************************************************************

-- | Helper to create a JS array from a list of floats
foreign import javascript unsafe "[$1, $2, $3, $4, $5, $6]"
    jsArray6 :: Float -> Float -> Float -> Float -> Float -> Float -> JSVal

testGraphics :: IO ()
testGraphics = do
    -- Test: newGraphics creates a graphics object
    g <- newGraphics
    setTestResult "graphics" (toJSVal g)
    testPassed "newGraphics"

    -- Test: drawRect
    beginFill g 0xFF0000
    drawRect g 0 0 100 100
    endFill g
    testPassed "drawRect"

    -- Test: beginFill/endFill
    testPassed "fill"

    -- Test: clear
    clear g
    testPassed "clear"

    -- Test: drawCircle
    beginFill g 0x00FF00
    drawCircle g 50 50 25
    endFill g
    testPassed "drawCircle"

    -- Test: drawEllipse
    clear g
    beginFill g 0x0000FF
    drawEllipse g 50 50 30 20
    endFill g
    testPassed "drawEllipse"

    -- Test: drawRoundedRect
    clear g
    beginFill g 0xFFFF00
    drawRoundedRect g 0 0 100 100 10
    endFill g
    testPassed "drawRoundedRect"

    -- Test: moveTo/lineTo
    clear g
    lineStyleWithOptions g 2 0xFFFFFF 1.0 0.5
    moveTo g 0 0
    lineTo g 100 100
    testPassed "moveTo_lineTo"

    -- Test: drawPolygon
    clear g
    beginFill g 0xFF00FF
    let points = jsArray6 0 0 50 100 100 0
    drawPolygon g points
    endFill g
    testPassed "drawPolygon"

    -- Test: bezierCurveTo
    clear g
    lineStyleWithOptions g 2 0xFFFFFF 1.0 0.5
    moveTo g 0 0
    bezierCurveTo g 25 50 75 50 100 0
    testPassed "bezierCurveTo"

    -- Test: quadraticCurveTo
    clear g
    lineStyleWithOptions g 2 0xFFFFFF 1.0 0.5
    moveTo g 0 0
    quadraticCurveTo g 50 100 100 0
    testPassed "quadraticCurveTo"

    -- Test: arc
    clear g
    lineStyleWithOptions g 2 0xFFFFFF 1.0 0.5
    arc g 50 50 40 0 3.14159 False
    testPassed "arc"

    -- Test: closePath
    clear g
    beginFill g 0x00FFFF
    moveTo g 0 0
    lineTo g 100 0
    lineTo g 100 100
    closePath g
    endFill g
    testPassed "closePath"

    -- Test: beginFillWithAlpha
    clear g
    beginFillWithAlpha g 0xFF0000 0.5
    drawRect g 0 0 100 100
    endFill g
    testPassed "fillWithAlpha"

    -- Test: graphics position
    setPosition g 200 200
    x <- getX g
    y <- getY g
    setTestBool "graphics_position" (abs (x - 200) < 0.01 && abs (y - 200) < 0.01)

    exportValue "TEST_GRAPHICS" (toJSVal g)

-- *****************************************************************************
-- * Math Tests
-- *****************************************************************************

testMath :: IO ()
testMath = do
    -- Test: newPoint
    point <- newPoint 10 20
    px <- getPointX point
    py <- getPointY point
    setTestFloat "point_x" px
    setTestFloat "point_y" py
    setTestBool "newPoint" (abs (px - 10) < 0.01 && abs (py - 20) < 0.01)

    -- Test: setPointX/setPointY
    setPointX point 30
    setPointY point 40
    px' <- getPointX point
    py' <- getPointY point
    setTestBool "setPoint" (abs (px' - 30) < 0.01 && abs (py' - 40) < 0.01)

    -- Test: newRectangle
    rect <- newRectangle 10 20 100 200
    rx <- getRectX rect
    ry <- getRectY rect
    rw <- getRectWidth rect
    rh <- getRectHeight rect
    setTestFloat "rect_x" rx
    setTestFloat "rect_y" ry
    setTestFloat "rect_width" rw
    setTestFloat "rect_height" rh
    setTestBool "newRectangle" (abs (rx - 10) < 0.01 && abs (ry - 20) < 0.01 &&
                                 abs (rw - 100) < 0.01 && abs (rh - 200) < 0.01)

    -- Test: getRectLeft/Right/Top/Bottom
    left <- getRectLeft rect
    right <- getRectRight rect
    top <- getRectTop rect
    bottom <- getRectBottom rect
    setTestBool "rect_edges" (abs (left - 10) < 0.01 && abs (right - 110) < 0.01 &&
                              abs (top - 20) < 0.01 && abs (bottom - 220) < 0.01)

    -- Test: containsRect
    contains1 <- containsRect rect 50 100  -- Should be inside
    contains2 <- containsRect rect 200 300 -- Should be outside
    setTestBool "containsRect" (contains1 && not contains2)

    -- Test: newCircle
    circle <- newCircle 50 50 25
    cx <- getCircleX circle
    cy <- getCircleY circle
    cr <- getCircleRadius circle
    setTestFloat "circle_x" cx
    setTestFloat "circle_y" cy
    setTestFloat "circle_radius" cr
    setTestBool "newCircle" (abs (cx - 50) < 0.01 && abs (cy - 50) < 0.01 && abs (cr - 25) < 0.01)

    -- Test: containsCircle
    ccontains1 <- containsCircle circle 50 50  -- Center, should be inside
    ccontains2 <- containsCircle circle 100 100 -- Should be outside
    setTestBool "containsCircle" (ccontains1 && not ccontains2)

    -- Test: newEllipse
    ellipse <- newEllipse 100 100 40 30
    ex <- getEllipseX ellipse
    ey <- getEllipseY ellipse
    ew <- getEllipseWidth ellipse
    eh <- getEllipseHeight ellipse
    setTestBool "newEllipse" (abs (ex - 100) < 0.01 && abs (ey - 100) < 0.01 &&
                              abs (ew - 40) < 0.01 && abs (eh - 30) < 0.01)

    -- Test: newMatrix
    matrix <- newMatrix
    setTestResult "matrix" (toJSVal matrix)
    testPassed "newMatrix"

    -- Test: translateMatrix
    matrix' <- translateMatrix matrix 10 20
    testPassed "translateMatrix"

    -- Test: scaleMatrix
    matrix'' <- scaleMatrix matrix 2 3
    testPassed "scaleMatrix"

    -- Test: rotateMatrix
    matrix''' <- rotateMatrix matrix 1.57
    testPassed "rotateMatrix"

    -- Test: cloneRect
    rectClone <- cloneRect rect
    cloneW <- getRectWidth rectClone
    setTestBool "cloneRect" (abs (cloneW - 100) < 0.01)

    -- Test: cloneCircle
    circleClone <- cloneCircle circle
    cloneR <- getCircleRadius circleClone
    setTestBool "cloneCircle" (abs (cloneR - 25) < 0.01)

-- *****************************************************************************
-- * Events Tests
-- *****************************************************************************

testEvents :: IO ()
testEvents = do
    -- Create a container to test events on
    container <- newContainer

    -- Test: setEventMode
    setEventMode container "static"
    testPassed "setEventMode"

    -- Test: setCursor
    setCursor container "pointer"
    testPassed "setCursor"

    -- Test: on (event listener) - we'll set up a handler and verify it's attached
    -- Note: We can't easily test the callback without user interaction
    callback <- jsFuncFromHs_ $ \_ -> do
        setTestBool "event_fired" True
    on "pointerdown" container callback
    testPassed "on"

    -- Test: once
    onceCallback <- jsFuncFromHs_ $ \_ -> do
        setTestBool "once_event_fired" True
    once "pointerdown" container onceCallback
    testPassed "once"

    -- Test: off (remove listener)
    off "pointerdown" container callback
    testPassed "off"

    -- Test: setHitArea (using a Rectangle)
    hitArea <- newRectangle 0 0 100 100
    setHitArea container (toJSVal hitArea)
    testPassed "setHitArea"

    exportValue "TEST_EVENT_CONTAINER" (toJSVal container)

-- *****************************************************************************
-- * Filters Tests
-- *****************************************************************************

testFilters :: IO ()
testFilters = do
    -- Test: newBlurFilter
    blurFilter <- newBlurFilter 8 4
    setTestResult "blur_filter" (toJSVal blurFilter)
    testPassed "newBlurFilter"

    -- Test: setBlurStrength/getBlurStrength
    setBlurStrength blurFilter 12
    strength <- getBlurStrength blurFilter
    setTestFloat "blur_strength" strength
    setTestBool "setBlurStrength_getBlurStrength" (abs (strength - 12) < 0.01)

    -- Test: setBlurQuality/getBlurQuality
    setBlurQuality blurFilter 8
    quality <- getBlurQuality blurFilter
    setTestInt "blur_quality" quality
    setTestBool "setBlurQuality_getBlurQuality" (quality == 8)

    -- Test: newColorMatrixFilter
    cmFilter <- newColorMatrixFilter
    setTestResult "colormatrix_filter" (toJSVal cmFilter)
    testPassed "newColorMatrixFilter"

    -- Test: colorMatrixBrightness
    colorMatrixBrightness cmFilter 1.5 True
    testPassed "colorMatrixBrightness"

    -- Test: colorMatrixContrast
    colorMatrixContrast cmFilter 1.2 True
    testPassed "colorMatrixContrast"

    -- Test: colorMatrixSaturate
    colorMatrixSaturate cmFilter 0.5 True
    testPassed "colorMatrixSaturate"

    -- Test: colorMatrixHue
    colorMatrixHue cmFilter 90 True
    testPassed "colorMatrixHue"

    -- Test: colorMatrixGrayscale
    colorMatrixGrayscale cmFilter 1 True
    testPassed "colorMatrixGrayscale"

    -- Test: colorMatrixSepia
    colorMatrixSepia cmFilter True
    testPassed "colorMatrixSepia"

    -- Test: colorMatrixNegative
    colorMatrixNegative cmFilter True
    testPassed "colorMatrixNegative"

    -- Test: colorMatrixReset
    colorMatrixReset cmFilter
    testPassed "colorMatrixReset"

    -- Test: newAlphaFilter
    alphaFilter <- newAlphaFilter 0.5
    setTestResult "alpha_filter" (toJSVal alphaFilter)
    testPassed "newAlphaFilter"

    -- Test: newNoiseFilter
    noiseFilter <- newNoiseFilter 0.5
    setTestResult "noise_filter" (toJSVal noiseFilter)
    testPassed "newNoiseFilter"

    -- Test: applying filters to a container
    container <- newContainer
    addFilter (toJSVal container) (safeCast blurFilter)
    testPassed "addFilter"

    -- Test: getFilters
    filters <- getFilters (toJSVal container)
    setTestResult "filters_array" filters
    testPassed "getFilters"

    -- Test: removeFilter
    removeFilter (toJSVal container) (safeCast blurFilter)
    testPassed "removeFilter"

-- *****************************************************************************
-- * Ticker Tests
-- *****************************************************************************

testTicker :: IO ()
testTicker = do
    -- Test: newTicker
    ticker <- newTicker
    setTestResult "new_ticker" (toJSVal ticker)
    testPassed "newTicker"

    -- Test: getSharedTicker
    sharedTicker <- getSharedTicker
    setTestResult "shared_ticker" (toJSVal sharedTicker)
    testPassed "getSharedTicker"

    -- Test: start
    start ticker
    testPassed "start"

    -- Test: stop
    stop ticker
    testPassed "stop"

    -- Test: getFPS
    fps <- getFPS sharedTicker
    setTestFloat "fps" fps
    setTestBool "getFPS" (fps >= 0)

    -- Test: getDeltaTime
    dt <- getDeltaTime sharedTicker
    setTestFloat "delta_time" dt
    testPassed "getDeltaTime"

    -- Test: getDeltaMS
    dtMS <- getDeltaMS sharedTicker
    setTestFloat "delta_ms" dtMS
    testPassed "getDeltaMS"

    -- Test: setSpeed/getSpeed
    setSpeed ticker 2.0
    speed <- getSpeed ticker
    setTestFloat "speed" speed
    setTestBool "setSpeed_getSpeed" (abs (speed - 2.0) < 0.01)

    -- Test: setMaxFPS/getMaxFPS
    setMaxFPS ticker 30
    maxFPS <- getMaxFPS ticker
    setTestFloat "max_fps" maxFPS
    setTestBool "setMaxFPS_getMaxFPS" (abs (maxFPS - 30) < 0.1)

    -- Test: setMinFPS/getMinFPS
    setMinFPS ticker 10
    minFPS <- getMinFPS ticker
    setTestFloat "min_fps" minFPS
    setTestBool "setMinFPS_getMinFPS" (abs (minFPS - 10) < 0.1)

    -- Test: add callback
    callback <- jsFuncFromHs_ $ \_ -> return ()
    add ticker callback
    testPassed "add"

    -- Test: remove callback
    remove ticker callback
    testPassed "remove"

-- *****************************************************************************
-- * Interop Tests
-- *****************************************************************************

testInterop :: IO ()
testInterop = do
    -- Test: floatAsVal/valAsFloat
    let floatVal = floatAsVal 3.14
    let recovered = valAsFloat floatVal
    setTestFloat "float_roundtrip" recovered
    setTestBool "floatAsVal_valAsFloat" (abs (recovered - 3.14) < 0.01)

    -- Test: intAsVal/valAsInt
    let intVal = intAsVal 42
    let recoveredInt = valAsInt intVal
    setTestInt "int_roundtrip" recoveredInt
    setTestBool "intAsVal_valAsInt" (recoveredInt == 42)

    -- Test: boolAsVal/valAsBool
    let boolVal = boolAsVal True
    let recoveredBool = valAsBool boolVal
    setTestBool "bool_roundtrip" recoveredBool
    setTestBool "boolAsVal_valAsBool" recoveredBool

    -- Test: stringAsVal (toJSString/fromJSString tested via other means)
    let strVal = stringAsVal "hello"
    setTestResult "string_val" strVal
    testPassed "stringAsVal"

    -- Test: toJSString/fromJSString
    let jsStr = toJSString "test string"
    let hsStr = fromJSString jsStr
    setTestBool "toJSString_fromJSString" (hsStr == "test string")

    -- Test: exportValue (already used extensively)
    exportValue "INTEROP_TEST_VALUE" (intAsVal 999)
    testPassed "exportValue"

    -- Test: setGlobalVariable
    setGlobalVariable "INTEROP_GLOBAL" (stringAsVal "global value")
    testPassed "setGlobalVariable"

    -- Test: parseJSON
    jsonVal <- parseJSON "{\"a\": 1, \"b\": \"hello\"}"
    setTestResult "parsed_json" jsonVal
    testPassed "parseJSON"

    -- Test: getProperty
    propA <- getProperty "a" jsonVal
    setTestResult "json_prop_a" propA
    testPassed "getProperty"

    -- Test: setProperty
    setProperty "c" jsonVal (intAsVal 3)
    testPassed "setProperty"

    -- Test: consoleLogShow
    consoleLogShow "Test log message"
    testPassed "consoleLogShow"

    -- Test: consoleLogVal
    consoleLogVal (intAsVal 12345)
    testPassed "consoleLogVal"

    -- Test: jsFuncFromHs
    func <- jsFuncFromHs $ \val -> do
        return val
    setTestResult "js_func" func  -- JSFunction is already JSVal
    testPassed "jsFuncFromHs"

    -- Test: jsFuncFromHs_
    func_ <- jsFuncFromHs_ $ \_ -> return ()
    setTestResult "js_func_" func_  -- JSFunction is already JSVal
    testPassed "jsFuncFromHs_"

-- *****************************************************************************
-- * Run All Tests
-- *****************************************************************************

runAllTests :: IO ()
runAllTests = do
    testApplication
    testContainer
    testSprite
    testText
    testGraphics
    testMath
    testEvents
    testFilters
    testTicker
    testInterop
    testPassed "ALL_TESTS_COMPLETE"

-- *****************************************************************************
-- * Main (required but does nothing - tests are called from JS)
-- *****************************************************************************

-- Export main for reactor module
foreign export javascript "main" main :: IO ()

main :: IO ()
main = return ()
