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
import Data.Coerce (coerce)

-- *****************************************************************************
-- * Event Listeners
-- *****************************************************************************

foreign import javascript unsafe "$2.on($1, $3)"
    js_on :: JSString -> JSVal -> JSFunction -> IO ()

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
on :: (IsEventEmitter obj) => JSString -> obj -> JSFunction -> IO ()
on eventName obj callback = js_on eventName (toJSVal obj) callback

foreign import javascript unsafe "$2.once($1, $3)"
    js_once :: JSString -> JSVal -> JSFunction -> IO ()

-- | Adds an event listener that will only fire once
--
-- @param eventName The name of the event
-- @param obj The object to add the listener to
-- @param callback The callback function
once :: (IsEventEmitter obj) => JSString -> obj -> JSFunction -> IO ()
once eventName obj callback = js_once eventName (toJSVal obj) callback

foreign import javascript unsafe "$2.off($1, $3)"
    js_off :: JSString -> JSVal -> JSFunction -> IO ()

-- | Removes an event listener
--
-- @param eventName The name of the event
-- @param obj The object to remove the listener from
-- @param callback The callback function to remove
off :: (IsEventEmitter obj) => JSString -> obj -> JSFunction -> IO ()
off eventName obj callback = js_off eventName (toJSVal obj) callback

foreign import javascript unsafe "$2.emit($1, $3)"
    js_emit :: JSString -> JSVal -> JSVal -> IO ()

-- | Emits an event
--
-- @param eventName The name of the event
-- @param obj The object to emit the event from
-- @param data Optional event data
emit :: (IsEventEmitter obj) => JSString -> obj -> JSVal -> IO ()
emit eventName obj dataVal = js_emit eventName (toJSVal obj) dataVal

-- *****************************************************************************
-- * Event Properties
-- *****************************************************************************

foreign import javascript unsafe "$1.type"
    js_getEventType :: JSVal -> IO JSString

-- | Gets the type of the event
getEventType :: FederatedEvent -> IO JSString
getEventType evt = js_getEventType (toJSVal evt)

foreign import javascript unsafe "$1.target"
    js_getEventTarget :: JSVal -> IO JSVal

-- | Gets the target of the event
getEventTarget :: FederatedEvent -> IO JSVal
getEventTarget evt = js_getEventTarget (toJSVal evt)

foreign import javascript unsafe "$1.currentTarget"
    js_getCurrentTarget :: JSVal -> IO JSVal

-- | Gets the current target of the event
getCurrentTarget :: FederatedEvent -> IO JSVal
getCurrentTarget evt = js_getCurrentTarget (coerce evt)

foreign import javascript unsafe "$1.data"
    js_getEventData :: JSVal -> IO JSVal

-- | Gets the data associated with the event
getEventData :: FederatedEvent -> IO JSVal
getEventData evt = js_getEventData (coerce evt)

-- *****************************************************************************
-- * Pointer Event Properties
-- *****************************************************************************

foreign import javascript unsafe "$1.global.x"
    js_getGlobalX :: JSVal -> IO Float

-- | Gets the global X coordinate of the pointer event
getGlobalX :: FederatedEvent -> IO Float
getGlobalX evt = js_getGlobalX (coerce evt)

foreign import javascript unsafe "$1.global.y"
    js_getGlobalY :: JSVal -> IO Float

-- | Gets the global Y coordinate of the pointer event
getGlobalY :: FederatedEvent -> IO Float
getGlobalY evt = js_getGlobalY (coerce evt)

foreign import javascript unsafe "$1.client.x"
    js_getClientX :: JSVal -> IO Float

-- | Gets the client X coordinate of the pointer event
getClientX :: FederatedEvent -> IO Float
getClientX evt = js_getClientX (coerce evt)

foreign import javascript unsafe "$1.client.y"
    js_getClientY :: JSVal -> IO Float

-- | Gets the client Y coordinate of the pointer event
getClientY :: FederatedEvent -> IO Float
getClientY evt = js_getClientY (coerce evt)

foreign import javascript unsafe "$1.screen.x"
    js_getScreenX :: JSVal -> IO Float

-- | Gets the screen X coordinate of the pointer event
getScreenX :: FederatedEvent -> IO Float
getScreenX evt = js_getScreenX (coerce evt)

foreign import javascript unsafe "$1.screen.y"
    js_getScreenY :: JSVal -> IO Float

-- | Gets the screen Y coordinate of the pointer event
getScreenY :: FederatedEvent -> IO Float
getScreenY evt = js_getScreenY (coerce evt)

foreign import javascript unsafe "$1.button"
    js_getButton :: JSVal -> IO Int

-- | Gets the button that was pressed (0 = left, 1 = middle, 2 = right)
getButton :: FederatedEvent -> IO Int
getButton evt = js_getButton (coerce evt)

foreign import javascript unsafe "$1.buttons"
    js_getButtons :: JSVal -> IO Int

-- | Gets a bitmask of buttons that are pressed
getButtons :: FederatedEvent -> IO Int
getButtons evt = js_getButtons (coerce evt)

-- *****************************************************************************
-- * Event Methods
-- *****************************************************************************

foreign import javascript unsafe "$1.stopPropagation()"
    js_stopPropagation :: JSVal -> IO ()

-- | Stops the event from propagating to parent objects
stopPropagation :: FederatedEvent -> IO ()
stopPropagation evt = js_stopPropagation (coerce evt)

foreign import javascript unsafe "$1.preventDefault()"
    js_preventDefault :: JSVal -> IO ()

-- | Prevents the default action of the event
preventDefault :: FederatedEvent -> IO ()
preventDefault evt = js_preventDefault (coerce evt)

-- *****************************************************************************
-- * Interactive Properties
-- *****************************************************************************

foreign import javascript unsafe "$1.eventMode = $2"
    js_setEventMode :: JSVal -> JSString -> IO ()

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
setEventMode :: (IsContainer obj) => obj -> JSString -> IO ()
setEventMode obj mode = js_setEventMode (toJSVal obj) mode

foreign import javascript unsafe "$1.cursor = $2"
    js_setCursor :: JSVal -> JSString -> IO ()

-- | Sets the cursor style when hovering over the object
--
-- @param obj The display object
-- @param cursor The cursor style (e.g., "pointer", "default")
setCursor :: (IsContainer obj) => obj -> JSString -> IO ()
setCursor obj cursor = js_setCursor (toJSVal obj) cursor

foreign import javascript unsafe "$1.hitArea = $2"
    js_setHitArea :: JSVal -> JSVal -> IO ()

-- | Sets a custom hit area for the object
--
-- @param obj The display object
-- @param hitArea A shape (Rectangle, Circle, Polygon, etc.) or null
setHitArea :: (IsContainer obj) => obj -> JSVal -> IO ()
setHitArea obj hitArea = js_setHitArea (toJSVal obj) hitArea
