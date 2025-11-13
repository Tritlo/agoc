{-# LANGUAGE OverloadedStrings #-}

-- | Pixi.js Ticker bindings
--
-- This module provides functions for working with the Pixi.js ticker system,
-- which handles animation loops and frame updates.
module Graphics.PixiJS.Ticker
    ( -- * Ticker Creation
      newTicker
    , getSharedTicker
    , getSystemTicker
      -- * Ticker Control
    , start
    , stop
    , update
      -- * Adding Callbacks
    , add
    , addOnce
    , remove
      -- * Ticker Properties
    , getFPS
    , getDeltaTime
    , getElapsedMS
    , getLastTime
    , getSpeed
    , setSpeed
    , getMaxFPS
    , setMaxFPS
    , getMinFPS
    , setMinFPS
    , getStarted
      -- * Ticker Delta
    , getDeltaMS
    ) where

import Graphics.PixiJS.Types
import GHC.Wasm.Prim

-- *****************************************************************************
-- * Ticker Creation
-- *****************************************************************************

-- | Creates a new Pixi.js Ticker instance
--
-- A ticker is used to call functions repeatedly at a specified rate.
-- Use 'start' to begin ticking, and 'add' to add callbacks.
foreign import javascript unsafe "new PIXI.Ticker()"
    newTicker :: IO Ticker

-- | Gets the shared ticker instance
--
-- This is the ticker used by default for all animations
foreign import javascript unsafe "PIXI.Ticker.shared"
    getSharedTicker :: IO Ticker

-- | Gets the system ticker instance
--
-- This ticker is used for system updates and runs independently
foreign import javascript unsafe "PIXI.Ticker.system"
    getSystemTicker :: IO Ticker

-- *****************************************************************************
-- * Ticker Control
-- *****************************************************************************

-- | Starts the ticker
foreign import javascript unsafe "$1.start()"
    start :: Ticker -> IO ()

-- | Stops the ticker
foreign import javascript unsafe "$1.stop()"
    stop :: Ticker -> IO ()

-- | Manually updates the ticker
--
-- @param ticker The ticker to update
-- @param currentTime The current time in milliseconds
foreign import javascript unsafe "$1.update($2)"
    update :: Ticker -> Float -> IO ()

-- *****************************************************************************
-- * Adding Callbacks
-- *****************************************************************************

-- | Adds a callback function to the ticker
--
-- The callback will be called on each tick with the ticker object as an argument.
--
-- @param ticker The ticker to add the callback to
-- @param callback The function to call on each tick
-- @param priority Optional priority (higher = earlier, default = 0)
foreign import javascript unsafe "$1.add($2)"
    add :: Ticker -> JSFunction -> IO ()

-- | Adds a callback that will only fire once
--
-- @param ticker The ticker to add the callback to
-- @param callback The function to call on the next tick
foreign import javascript unsafe "$1.addOnce($2)"
    addOnce :: Ticker -> JSFunction -> IO ()

-- | Removes a callback from the ticker
--
-- @param ticker The ticker to remove the callback from
-- @param callback The callback function to remove
foreign import javascript unsafe "$1.remove($2)"
    remove :: Ticker -> JSFunction -> IO ()

-- *****************************************************************************
-- * Ticker Properties
-- *****************************************************************************

-- | Gets the current frames per second
foreign import javascript unsafe "$1.FPS"
    getFPS :: Ticker -> IO Float

-- | Gets the delta time between ticks (as a multiplier, typically 1.0)
foreign import javascript unsafe "$1.deltaTime"
    getDeltaTime :: Ticker -> IO Float

-- | Gets the elapsed time in milliseconds since the last tick
foreign import javascript unsafe "$1.elapsedMS"
    getElapsedMS :: Ticker -> IO Float

-- | Gets the time of the last tick in milliseconds
foreign import javascript unsafe "$1.lastTime"
    getLastTime :: Ticker -> IO Float

-- | Gets the speed multiplier (1.0 = normal speed)
foreign import javascript unsafe "$1.speed"
    getSpeed :: Ticker -> IO Float

-- | Sets the speed multiplier (1.0 = normal speed)
foreign import javascript unsafe "$1.speed = $2"
    setSpeed :: Ticker -> Float -> IO ()

-- | Gets the maximum FPS
foreign import javascript unsafe "$1.maxFPS"
    getMaxFPS :: Ticker -> IO Float

-- | Sets the maximum FPS (0 = unlimited)
foreign import javascript unsafe "$1.maxFPS = $2"
    setMaxFPS :: Ticker -> Float -> IO ()

-- | Gets the minimum FPS
foreign import javascript unsafe "$1.minFPS"
    getMinFPS :: Ticker -> IO Float

-- | Sets the minimum FPS
foreign import javascript unsafe "$1.minFPS = $2"
    setMinFPS :: Ticker -> Float -> IO ()

-- | Gets whether the ticker has been started
foreign import javascript unsafe "$1.started"
    getStarted :: Ticker -> IO Bool

-- *****************************************************************************
-- * Ticker Delta
-- *****************************************************************************

-- | Gets the delta time in milliseconds
foreign import javascript unsafe "$1.deltaMS"
    getDeltaMS :: Ticker -> IO Float
