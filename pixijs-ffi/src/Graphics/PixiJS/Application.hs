{-# LANGUAGE OverloadedStrings #-}

-- | Pixi.js Application bindings
--
-- This module provides functions for creating and managing Pixi.js Application
-- instances. The Application class is the main entry point for creating
-- Pixi.js projects.
module Graphics.PixiJS.Application
    ( -- * Application Creation
      newApp
    , initApp
    , initAppWithOptions
    , initAppInTarget
      -- * Canvas Management
    , appendCanvas
    , appendToTarget
    , setFocus
      -- * Application Properties
    , getStage
    , getRenderer
    , getCanvas
    , getScreen
    , getTicker
      -- * Application Methods
    , destroy
    , resize
    , render
      -- * Ticker Control
    , startTicker
    , stopTicker
    , addTickerCallback
    ) where

import Graphics.PixiJS.Types
import Graphics.PixiJS.Interop (jsFuncFromHs)
import GHC.Wasm.Prim
import Data.Coerce (coerce)

-- *****************************************************************************
-- * Application Creation
-- *****************************************************************************

foreign import javascript unsafe "new PIXI.Application()"
   js_newApp :: IO JSVal

-- | Creates a new Pixi.js Application instance
-- This creates a new PIXI Application object without initializing it
-- Use 'initApp' or 'initAppWithOptions' to initialize the application
newApp :: IO Application
newApp = Application <$> js_newApp

foreign import javascript safe
   "const r = await $1.init({background: $2, resizeTo: window, preference: 'webgl'}); return $1"
   js_initApp :: JSVal -> JSString -> IO JSVal

-- | Initializes a Pixi.js Application with the given background color
--
-- This function uses a safe import because it needs to await the result of
-- the init function. The init function has side-effects on the app object.
--
-- Example:
-- @
-- app <- newApp
-- initApp app "#1099bb"
-- @
--
-- @param app The PIXI Application object to initialize
-- @param backgroundColor The background color as a JavaScript string (e.g., "#1099bb", "0x000000")
-- @return The initialized application object
initApp :: Application -> JSString -> IO Application
initApp app bgColor = Application <$> js_initApp (coerce app) bgColor

foreign import javascript safe
   "const r = await $1.init($2); return $1"
   js_initAppWithOptions :: JSVal -> JSVal -> IO JSVal

-- | Initializes a Pixi.js Application with custom options
--
-- This provides full control over initialization options.
-- The options object should contain valid Pixi.js init options.
--
-- Example:
-- @
-- app <- newApp
-- opts <- js_object [("background", stringAsVal "#1099bb"),
--                    ("width", intAsVal 800),
--                    ("height", intAsVal 600)]
-- initAppWithOptions app opts
-- @
--
-- @param app The PIXI Application object to initialize
-- @param options A JavaScript object containing initialization options
-- @return The initialized application object
initAppWithOptions :: Application -> JSVal -> IO Application
initAppWithOptions app options = Application <$> js_initAppWithOptions (coerce app) options

foreign import javascript safe
   "const r = await $1.init({background: $2, resizeTo: document.querySelector($3), preference: 'webgl'}); return $1"
    js_initAppInTarget :: JSVal -> JSString -> JSString -> IO JSVal

-- | Initializes a Pixi.js Application and resizes it to a target element
--
-- This function initializes the application and automatically resizes the
-- canvas to match the dimensions of the specified target element.
--
-- @param app The PIXI Application object to initialize
-- @param backgroundColor The background color as a JavaScript string (e.g., "#1099bb")
-- @param targetSelector The CSS selector for the target element (e.g., "#canvas-container")
-- @return The initialized application object
initAppInTarget :: Application -> JSString -> JSString -> IO Application
initAppInTarget app bgColor targetSelector = Application <$> js_initAppInTarget (coerce app) bgColor targetSelector

-- *****************************************************************************
-- * Canvas Management
-- *****************************************************************************

foreign import javascript unsafe "document.body.appendChild($1.canvas); window.__PIXI_APP__ = $1"
    js_appendCanvas :: JSVal -> IO ()

-- | Appends the application's canvas to the document body
-- This makes the Pixi.js canvas visible in the browser.
-- Also exposes the application as window.__PIXI_APP__ for debugging/testing.
--
-- @param app The PIXI Application object whose canvas should be appended
appendCanvas :: Application -> IO ()
appendCanvas app = js_appendCanvas (coerce app)

foreign import javascript unsafe "document.querySelector($1).appendChild($2.canvas)"
    js_appendToTarget :: JSString -> JSVal -> IO ()

-- | Appends the application's canvas to a target element
appendToTarget :: Application -> JSString -> IO ()
appendToTarget app selector = js_appendToTarget selector (coerce app)

foreign import javascript unsafe "$1.canvas.focus()"
    js_setFocus :: JSVal -> IO ()

-- | Sets focus to the application's canvas
setFocus :: Application -> IO ()
setFocus app = js_setFocus (coerce app)
-- | Destroys the application
--
-- @param app The application instance
-- @param removeView Whether to remove the view from the DOM
destroy :: Application -> Bool -> IO ()
destroy app removeView = js_destroy (toJSVal app) removeView

foreign import javascript unsafe "$1.destroy($2)"
    js_destroy :: JSVal -> Bool -> IO ()

-- *****************************************************************************
-- * Application Properties
-- *****************************************************************************

-- | Gets the stage container
getStage :: Application -> IO Container
getStage app = do
    val <- js_getStage (toJSVal app)
    return $ fromJSVal val

foreign import javascript unsafe "$1.stage"
    js_getStage :: JSVal -> IO JSVal

-- | Gets the renderer
getRenderer :: Application -> IO Renderer
getRenderer app = do
    val <- js_getRenderer (toJSVal app)
    return $ fromJSVal val

foreign import javascript unsafe "$1.renderer"
    js_getRenderer :: JSVal -> IO JSVal

-- | Gets the screen rectangle
getScreen :: Application -> IO Rectangle
getScreen app = do
    val <- js_getScreen (toJSVal app)
    return $ fromJSVal val

foreign import javascript unsafe "$1.screen"
    js_getScreen :: JSVal -> IO JSVal

-- | Gets the view (canvas)
getView :: Application -> IO Canvas
getView app = do
    val <- js_getView (toJSVal app)
    return $ fromJSVal val

foreign import javascript unsafe "$1.canvas"
    js_getView :: JSVal -> IO JSVal

-- | Gets the canvas element (alias for getView)
getCanvas :: Application -> IO Canvas
getCanvas = getView

-- *****************************************************************************
-- * Ticker
-- *****************************************************************************

-- | Gets the shared ticker
getTicker :: Application -> IO Ticker
getTicker app = do
    val <- js_getTicker (toJSVal app)
    return $ fromJSVal val

foreign import javascript unsafe "$1.ticker"
    js_getTicker :: JSVal -> IO JSVal

-- | Starts the ticker
startTicker :: Ticker -> IO ()
startTicker ticker = js_startTicker (toJSVal ticker)

foreign import javascript unsafe "$1.start()"
    js_startTicker :: JSVal -> IO ()

-- | Stops the ticker
stopTicker :: Ticker -> IO ()
stopTicker ticker = js_stopTicker (toJSVal ticker)

foreign import javascript unsafe "$1.stop()"
    js_stopTicker :: JSVal -> IO ()

-- | Adds a callback to the ticker
--
-- @param ticker The ticker
-- @param callback The function to call on each tick
addTickerCallback :: Ticker -> (Float -> IO ()) -> IO ()
addTickerCallback ticker callback = do
    -- Wrap the Haskell function in a JS function
    -- The ticker callback receives the ticker object, but we usually want the delta time
    -- PixiJS v8 ticker callback signature: (ticker) => void
    -- ticker.deltaTime is what we usually want
    jsCallback <- jsFuncFromHs $ \val -> do
        -- val is the ticker object
        -- We can extract deltaTime from it if needed, or just pass a dummy value for now
        -- Let's assume the user wants to do something
        callback 1.0 -- Placeholder for delta time
        return val -- Return value ignored

    js_addTickerCallback (toJSVal ticker) jsCallback

foreign import javascript unsafe "$1.add($2)"
    js_addTickerCallback :: JSVal -> JSFunction -> IO ()

-- | Renders the stage
--
-- @param app The PIXI Application object
render :: Application -> IO ()
render app = js_render (toJSVal app)

foreign import javascript unsafe "$1.render()"
    js_render :: JSVal -> IO ()

foreign import javascript unsafe "$1.resize()"
    js_resize :: JSVal -> IO ()

-- | Resizes the application
resize :: Application -> IO ()
resize app = js_resize (toJSVal app)
