{-# LANGUAGE OverloadedStrings #-}

-- | Browser Gamepad API bindings
--
-- This module provides functions for working with gamepads using the
-- browser's Gamepad API. While not part of Pixi.js itself, this is
-- commonly used alongside Pixi.js for game input.
--
-- Note: This uses the standard browser Gamepad API, which is separate
-- from Pixi.js but often used together with it.
module Graphics.PixiJS.Gamepad
    ( -- * Gamepad Access
      getFirstGamepad
    , getGamepads
    , getGamepad
      -- * Gamepad Properties
    , getGamepadId
    , getGamepadIndex
    , getGamepadConnected
    , getGamepadMapping
    , getGamepadTimestamp
      -- * Axes
    , getGamepadAxis
    , getNumAxes
      -- * Buttons
    , isGamepadButtonPressed
    , getGamepadButtonValue
    , getNumButtons
    ) where

import Graphics.PixiJS.Types
import GHC.Wasm.Prim

-- *****************************************************************************
-- * Gamepad Access
-- *****************************************************************************

-- | Gets the first connected gamepad, or null if none connected
-- Uses browser Gamepad API. Filters for standard mapping gamepads first,
-- then falls back to any connected gamepad.
foreign import javascript safe
  """
  (() => {
    if (navigator.getGamepads) {
      const pads = navigator.getGamepads();
      for (let i = 0; i < pads.length; i++) {
        if (pads[i] !== null && pads[i].mapping === 'standard') {
          return pads[i];
        }
      }
      // If no standard-mapped pad, return the first non-null pad
      for (let i = 0; i < pads.length; i++) {
        if (pads[i] !== null) {
          return pads[i];
        }
      }
    }
    return null;
  })()
  """
    getFirstGamepad :: IO JSVal

-- | Gets all connected gamepads
-- Returns an array-like object of gamepads (may contain nulls)
foreign import javascript unsafe "navigator.getGamepads()"
    getGamepads :: IO JSVal

-- | Gets a gamepad by index
--
-- @param index The gamepad index
-- @return The gamepad object, or null if not connected
foreign import javascript unsafe "navigator.getGamepads()[$1]"
    getGamepad :: Int -> IO JSVal

-- *****************************************************************************
-- * Gamepad Properties
-- *****************************************************************************

-- | Gets the gamepad ID string
foreign import javascript unsafe "$1 ? $1.id : ''"
    getGamepadId :: JSVal -> IO JSString

-- | Gets the gamepad index
foreign import javascript unsafe "$1 ? $1.index : -1"
    getGamepadIndex :: JSVal -> IO Int

-- | Checks if the gamepad is connected
foreign import javascript unsafe "$1 ? $1.connected : false"
    getGamepadConnected :: JSVal -> IO Bool

-- | Gets the gamepad mapping type (e.g., "standard")
foreign import javascript unsafe "$1 ? $1.mapping : ''"
    getGamepadMapping :: JSVal -> IO JSString

-- | Gets the gamepad timestamp
foreign import javascript unsafe "$1 ? $1.timestamp : 0"
    getGamepadTimestamp :: JSVal -> IO Float

-- *****************************************************************************
-- * Axes
-- *****************************************************************************

-- | Gets a gamepad axis value
--
-- Standard gamepad axis indices:
-- - 0: Left stick X (left = -1, right = 1)
-- - 1: Left stick Y (up = -1, down = 1)
-- - 2: Right stick X (left = -1, right = 1)
-- - 3: Right stick Y (up = -1, down = 1)
--
-- @param gamepadHandle The gamepad object from browser Gamepad API
-- @param axisIndex The axis index
-- @return The axis value (-1.0 to 1.0), or 0.0 if gamepad is null or axis doesn't exist
foreign import javascript unsafe
  """
  (() => {
    const handle = $1;
    const axisIndex = $2;
    if (handle && handle.axes && handle.axes[axisIndex] !== undefined) {
      return handle.axes[axisIndex];
    }
    return 0.0;
  })()
  """
    getGamepadAxis :: JSVal -> Int -> IO Float

-- | Gets the number of axes on a gamepad
foreign import javascript unsafe "$1 && $1.axes ? $1.axes.length : 0"
    getNumAxes :: JSVal -> IO Int

-- *****************************************************************************
-- * Buttons
-- *****************************************************************************

-- | Checks if a gamepad button is pressed
--
-- Standard gamepad button indices:
-- - 0: Bottom button (A/Cross)
-- - 1: Right button (B/Circle)
-- - 2: Left button (X/Square)
-- - 3: Top button (Y/Triangle)
-- - 4: Left shoulder (L1/LB)
-- - 5: Right shoulder (R1/RB)
-- - 6: Left trigger (L2/LT)
-- - 7: Right trigger (R2/RT)
-- - 8: Select/Back
-- - 9: Start/Forward
-- - 10: Left stick press (L3)
-- - 11: Right stick press (R3)
-- - 12: D-pad up
-- - 13: D-pad down
-- - 14: D-pad left
-- - 15: D-pad right
--
-- @param gamepadHandle The gamepad object from browser Gamepad API
-- @param buttonIndex The button index
-- @return True if button is pressed, False otherwise
foreign import javascript unsafe "($1 && $1.buttons && $1.buttons[$2]) ? $1.buttons[$2].pressed : false"
    isGamepadButtonPressed :: JSVal -> Int -> IO Bool

-- | Gets the value of a gamepad button (0.0 to 1.0)
-- Useful for analog triggers
--
-- @param gamepadHandle The gamepad object
-- @param buttonIndex The button index
-- @return The button value (0.0 to 1.0), or 0.0 if not available
foreign import javascript unsafe "($1 && $1.buttons && $1.buttons[$2]) ? $1.buttons[$2].value : 0.0"
    getGamepadButtonValue :: JSVal -> Int -> IO Float

-- | Gets the number of buttons on a gamepad
foreign import javascript unsafe "$1 && $1.buttons ? $1.buttons.length : 0"
    getNumButtons :: JSVal -> IO Int
