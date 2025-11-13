{-# LANGUAGE OverloadedStrings #-}

-- | Pixi.js Events bindings
--
-- This module provides functions for handling events in Pixi.js, including
-- mouse, touch, and pointer events.
module Graphics.PixiJS.Events
    ( -- * Event Listeners
      on
    , once
    , off
    , emit
      -- * Event Properties
    , getEventType
    , getEventTarget
    , getCurrentTarget
    , getEventData
      -- * Pointer Event Properties
    , getGlobalX
    , getGlobalY
    , getClientX
    , getClientY
    , getScreenX
    , getScreenY
    , getButton
    , getButtons
      -- * Event Methods
    , stopPropagation
    , preventDefault
      -- * Interactive Properties
    , setEventMode
    , setCursor
    , setHitArea
    ) where

import Graphics.PixiJS.Types
import GHC.Wasm.Prim

-- *****************************************************************************
-- * Event Listeners
-- *****************************************************************************

-- | Adds an event listener to an object
--
-- Common event types include:
-- - "pointerdown", "pointerup", "pointermove", "pointerover", "pointerout"
-- - "click", "tap"
-- - "mousedown", "mouseup", "mousemove", "mouseover", "mouseout"
-- - "touchstart", "touchend", "touchmove"
--
-- @param eventName The name of the event
-- @param obj The object to add the listener to
-- @param callback The callback function
foreign import javascript unsafe "$2.on($1, $3)"
    on :: JSString -> JSVal -> JSFunction -> IO ()

-- | Adds an event listener that will only fire once
--
-- @param eventName The name of the event
-- @param obj The object to add the listener to
-- @param callback The callback function
foreign import javascript unsafe "$2.once($1, $3)"
    once :: JSString -> JSVal -> JSFunction -> IO ()

-- | Removes an event listener
--
-- @param eventName The name of the event
-- @param obj The object to remove the listener from
-- @param callback The callback function to remove
foreign import javascript unsafe "$2.off($1, $3)"
    off :: JSString -> JSVal -> JSFunction -> IO ()

-- | Emits an event
--
-- @param eventName The name of the event
-- @param obj The object to emit the event from
-- @param data Optional event data
foreign import javascript unsafe "$2.emit($1, $3)"
    emit :: JSString -> JSVal -> JSVal -> IO ()

-- *****************************************************************************
-- * Event Properties
-- *****************************************************************************

-- | Gets the type of the event
foreign import javascript unsafe "$1.type"
    getEventType :: FederatedEvent -> IO JSString

-- | Gets the target of the event
foreign import javascript unsafe "$1.target"
    getEventTarget :: FederatedEvent -> IO JSVal

-- | Gets the current target of the event
foreign import javascript unsafe "$1.currentTarget"
    getCurrentTarget :: FederatedEvent -> IO JSVal

-- | Gets the data associated with the event
foreign import javascript unsafe "$1.data"
    getEventData :: FederatedEvent -> IO JSVal

-- *****************************************************************************
-- * Pointer Event Properties
-- *****************************************************************************

-- | Gets the global X coordinate of the pointer event
foreign import javascript unsafe "$1.global.x"
    getGlobalX :: FederatedEvent -> IO Float

-- | Gets the global Y coordinate of the pointer event
foreign import javascript unsafe "$1.global.y"
    getGlobalY :: FederatedEvent -> IO Float

-- | Gets the client X coordinate of the pointer event
foreign import javascript unsafe "$1.client.x"
    getClientX :: FederatedEvent -> IO Float

-- | Gets the client Y coordinate of the pointer event
foreign import javascript unsafe "$1.client.y"
    getClientY :: FederatedEvent -> IO Float

-- | Gets the screen X coordinate of the pointer event
foreign import javascript unsafe "$1.screen.x"
    getScreenX :: FederatedEvent -> IO Float

-- | Gets the screen Y coordinate of the pointer event
foreign import javascript unsafe "$1.screen.y"
    getScreenY :: FederatedEvent -> IO Float

-- | Gets the button that was pressed (0 = left, 1 = middle, 2 = right)
foreign import javascript unsafe "$1.button"
    getButton :: FederatedEvent -> IO Int

-- | Gets a bitmask of buttons that are pressed
foreign import javascript unsafe "$1.buttons"
    getButtons :: FederatedEvent -> IO Int

-- *****************************************************************************
-- * Event Methods
-- *****************************************************************************

-- | Stops the event from propagating to parent objects
foreign import javascript unsafe "$1.stopPropagation()"
    stopPropagation :: FederatedEvent -> IO ()

-- | Prevents the default action of the event
foreign import javascript unsafe "$1.preventDefault()"
    preventDefault :: FederatedEvent -> IO ()

-- *****************************************************************************
-- * Interactive Properties
-- *****************************************************************************

-- | Sets the event mode of a display object
--
-- Modes include:
-- - "auto" - automatic event handling
-- - "static" - object is interactive but not its children
-- - "dynamic" - object and children are interactive
-- - "none" - not interactive
--
-- @param obj The display object
-- @param mode The event mode
foreign import javascript unsafe "$1.eventMode = $2"
    setEventMode :: JSVal -> JSString -> IO ()

-- | Sets the cursor style when hovering over the object
--
-- @param obj The display object
-- @param cursor The cursor style (e.g., "pointer", "default")
foreign import javascript unsafe "$1.cursor = $2"
    setCursor :: JSVal -> JSString -> IO ()

-- | Sets a custom hit area for the object
--
-- @param obj The display object
-- @param hitArea A shape (Rectangle, Circle, Polygon, etc.) or null
foreign import javascript unsafe "$1.hitArea = $2"
    setHitArea :: JSVal -> JSVal -> IO ()
