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
    ) where

import Graphics.PixiJS.Types
import GHC.Wasm.Prim

-- *****************************************************************************
-- * Application Creation
-- *****************************************************************************

-- | Creates a new Pixi.js Application instance
-- This creates a new PIXI Application object without initializing it
-- Use 'initApp' or 'initAppWithOptions' to initialize the application
foreign import javascript unsafe "new PIXI.Application()"
   newApp :: IO Application

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
foreign import javascript safe
   """
  const r = await $1.init({background: $2, resizeTo: window, preference: "webgl"});
  return $1
   """
   initApp :: Application -> JSString -> IO Application

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
foreign import javascript safe
   """
  const r = await $1.init($2);
  return $1
   """
   initAppWithOptions :: Application -> JSVal -> IO Application

-- | Initializes a Pixi.js Application and resizes it to a target element
--
-- This function initializes the application and automatically resizes the
-- canvas to match the dimensions of the specified target element.
--
-- @param app The PIXI Application object to initialize
-- @param backgroundColor The background color as a JavaScript string (e.g., "#1099bb")
-- @param targetSelector The CSS selector for the target element (e.g., "#canvas-container")
-- @return The initialized application object
foreign import javascript safe
 """
  const r = await $1.init({background: $2, resizeTo: document.querySelector($3), preference: "webgl"});
  return $1
 """
    initAppInTarget :: Application -> JSString -> JSString -> IO Application

-- *****************************************************************************
-- * Canvas Management
-- *****************************************************************************

-- | Appends the application's canvas to the document body
-- This makes the Pixi.js canvas visible in the browser
--
-- @param app The PIXI Application object whose canvas should be appended
foreign import javascript unsafe "document.body.appendChild($1.canvas)"
    appendCanvas :: Application -> IO ()

-- | Appends the application's canvas to a target element specified by a CSS selector
-- This makes the Pixi.js canvas visible in the specified target element
--
-- @param targetSelector The CSS selector for the target element (e.g., "#canvas-container")
-- @param app The PIXI Application object whose canvas should be appended
foreign import javascript unsafe "document.querySelector($1).appendChild($2.canvas)"
    appendToTarget :: JSString -> Application -> IO ()

-- | Sets focus on the application's canvas and makes it interactive
--
-- @param app The PIXI Application object
foreign import javascript unsafe
   """
   $1.canvas.tabIndex = 0;
   $1.canvas.focus();
   """
    setFocus :: Application -> IO ()

-- *****************************************************************************
-- * Application Properties
-- *****************************************************************************

-- | Gets the application's stage (root container)
--
-- @param app The PIXI Application object
-- @return The stage container
foreign import javascript unsafe "$1.stage"
    getStage :: Application -> IO Container

-- | Gets the application's renderer
--
-- @param app The PIXI Application object
-- @return The renderer
foreign import javascript unsafe "$1.renderer"
    getRenderer :: Application -> IO Renderer

-- | Gets the application's canvas element
--
-- @param app The PIXI Application object
-- @return The canvas element
foreign import javascript unsafe "$1.canvas"
    getCanvas :: Application -> IO Canvas

-- | Gets the application's screen rectangle
--
-- @param app The PIXI Application object
-- @return The screen rectangle
foreign import javascript unsafe "$1.screen"
    getScreen :: Application -> IO Rectangle

-- | Gets the application's ticker
--
-- @param app The PIXI Application object
-- @return The ticker
foreign import javascript unsafe "$1.ticker"
    getTicker :: Application -> IO Ticker

-- *****************************************************************************
-- * Application Methods
-- *****************************************************************************

-- | Destroys the application and all of its children
--
-- @param app The PIXI Application object to destroy
-- @param removeCanvas Whether to remove the canvas from the DOM (default: false)
foreign import javascript unsafe "$1.destroy($2)"
    destroy :: Application -> Bool -> IO ()

-- | Resizes the application
--
-- @param app The PIXI Application object
foreign import javascript unsafe "$1.resize()"
    resize :: Application -> IO ()

-- | Renders the stage
--
-- @param app The PIXI Application object
foreign import javascript unsafe "$1.render()"
    render :: Application -> IO ()
