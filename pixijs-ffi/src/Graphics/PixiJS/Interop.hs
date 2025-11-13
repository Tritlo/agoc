{-# LANGUAGE OverloadedStrings #-}

-- | JavaScript interop utilities for Pixi.js FFI
--
-- This module provides utility functions for converting between Haskell and
-- JavaScript types, working with JavaScript objects and properties, and
-- creating JavaScript callbacks from Haskell functions.
module Graphics.PixiJS.Interop
    ( -- * Type Conversions
      -- ** To JavaScript
      floatAsVal
    , intAsVal
    , boolAsVal
    , stringAsVal
      -- ** From JavaScript
    , valAsFloat
    , valAsInt
    , valAsBool
    , valAsString
      -- * String Utilities
    , toJSString
    , fromJSString
      -- * Property Access
    , getProperty
    , getPropertyKey
    , setProperty
    , setPropertyKey
    , incrementProperty
      -- * Function Wrapping
    , jsFuncFromHs
    , jsFuncFromHs_
    , callFunction
    , callMethod
      -- * Console Logging
    , consoleLogVal
    , consoleLogShow
      -- * JSON
    , parseJSON
      -- * Global Variables
    , exportValue
    , setGlobalVariable
      -- * Event Listeners
    , addEventListener
    ) where

import Graphics.PixiJS.Types
import GHC.Wasm.Prim
import Data.String (IsString(..))

-- *****************************************************************************
-- * Type Conversions
-- *****************************************************************************

-- | Converts a Float to a JavaScript value
foreign import javascript "$1"
  floatAsVal :: Float -> JSVal

-- | Converts an Int to a JavaScript value
foreign import javascript "$1"
  intAsVal :: Int -> JSVal

-- | Converts a Bool to a JavaScript value
foreign import javascript "$1"
  boolAsVal :: Bool -> JSVal

-- | Converts a JSString to a JavaScript value
foreign import javascript "$1"
  stringAsVal :: JSString -> JSVal

-- | Converts a JavaScript value to a Float
-- This is an unsafe conversion - the value must be a number
foreign import javascript "$1"
  valAsFloat :: JSVal -> Float

-- | Converts a JavaScript value to an Int
-- This is an unsafe conversion - the value must be a number
foreign import javascript "$1"
  valAsInt :: JSVal -> Int

-- | Converts a JavaScript value to a Bool
-- This is an unsafe conversion - the value must be a boolean
foreign import javascript "$1"
  valAsBool :: JSVal -> Bool

-- | Converts a JavaScript value to a JSString
-- This is an unsafe conversion - the value must be a string
foreign import javascript "$1"
  valAsString :: JSVal -> JSString

-- *****************************************************************************
-- * Property Access
-- *****************************************************************************

-- | Gets a property from a JavaScript object
--
-- @param propName The name of the property to get
-- @param obj The JavaScript object
-- @return The value of the property
foreign import javascript "$2[$1]"
  getProperty :: JSString -> JSVal -> IO JSVal

-- | Gets a property from a JavaScript object by a list of keys
getPropertyKey :: [JSString] -> JSVal -> IO JSVal
getPropertyKey keys obj =
    case keys of
        [] -> return obj
        (k:ks) -> do
            tmp <- getProperty k obj
            getPropertyKey ks tmp

-- | Sets a property on a JavaScript object
--
-- @param propName The name of the property to set
-- @param obj The JavaScript object
-- @param value The value to set
foreign import javascript "$2[$1] = $3"
  setProperty :: JSString -> JSVal -> JSVal -> IO ()

-- | Sets a property on a JavaScript object by a list of keys
setPropertyKey :: [JSString] -> JSVal -> JSVal -> IO ()
setPropertyKey keys obj value =
    case keys of
        [k] -> setProperty k obj value
        (k:ks) -> do
            tmp <- getProperty k obj
            setPropertyKey ks tmp value
            setProperty k obj tmp
        [] -> return ()

-- | Increments a property on a JavaScript object
-- This is equivalent to @obj[propName] += value@ in JavaScript
--
-- @param propName The name of the property to increment
-- @param obj The JavaScript object
-- @param value The value to add to the property
foreign import javascript "$2[$1] += $3"
  incrementProperty :: JSString -> JSVal -> JSVal -> IO ()

-- *****************************************************************************
-- * Function Wrapping
-- *****************************************************************************

-- | Converts a Haskell function to a JavaScript function
-- This creates a JavaScript function that can be called from JavaScript code
-- and will execute the provided Haskell function
--
-- @param hsFunc A Haskell function that takes a JSVal and returns an IO JSVal
-- @return A JavaScript function that can be passed to JavaScript code
foreign import javascript "wrapper"
  jsFuncFromHs :: (JSVal -> IO JSVal) -> IO JSVal

-- | Converts a Haskell function to a JavaScript function that does not return a value
jsFuncFromHs_ :: (JSVal -> IO ()) -> IO JSVal
jsFuncFromHs_ func =
    jsFuncFromHs (\val -> do
        func val
        return val
    )

-- | Calls a JavaScript function with a single argument
--
-- @param func The JavaScript function to call
-- @param arg The argument to pass to the function
-- @return The return value of the function call
foreign import javascript unsafe "$1($2)"
  callFunction :: JSFunction -> JSVal -> IO JSVal

-- | Calls a method on a JavaScript object
--
-- @param obj The JavaScript object
-- @param methodName The name of the method to call
-- @return The return value of the method call
foreign import javascript unsafe "$1[$2]()"
  callMethod :: JSVal -> JSString -> IO JSVal

-- *****************************************************************************
-- * Console Logging
-- *****************************************************************************

-- | Logs a JavaScript value to the browser console
--
-- @param val The JavaScript value to log
foreign import javascript unsafe "console.log($1)"
    consoleLogVal :: JSVal -> IO ()

-- | Logs a Haskell value to the browser console by converting it to a string
-- This is a convenience function that combines 'show', 'toJSString', 'stringAsVal',
-- and 'consoleLogVal' to log any Show instance
--
-- @param a A value of any type that has a Show instance
consoleLogShow :: Show a => a -> IO ()
consoleLogShow = consoleLogVal . stringAsVal . toJSString . show

-- *****************************************************************************
-- * JSON
-- *****************************************************************************

-- | Parses a JSON string into a JavaScript value
foreign import javascript unsafe "JSON.parse($1)"
  parseJSON :: JSString -> IO JSVal

-- *****************************************************************************
-- * Global Variables
-- *****************************************************************************

-- | Exports a value to the window object
foreign import javascript unsafe "window[$1] = $2"
  exportValue :: JSString -> JSVal -> IO ()

-- | Sets a global variable on the window object
foreign import javascript unsafe "window[$1] = $2"
  setGlobalVariable :: JSString -> JSVal -> IO ()

-- *****************************************************************************
-- * Event Listeners
-- *****************************************************************************

-- | Adds an event listener to a JavaScript object
--
-- @param event The event to listen for (e.g., "click")
-- @param obj The JavaScript object to add the event listener to
-- @param listener The function to call when the event occurs
foreign import javascript unsafe "$2.on($1, $3)"
  addEventListener :: JSString -> JSVal -> JSFunction -> IO ()

-- *****************************************************************************
-- * String Utilities
-- *****************************************************************************

-- | Instance allowing JSString to be created from Haskell strings using
-- string literals or 'fromString'. This enables convenient syntax like
-- @\"hello\" :: JSString@
instance IsString JSString where
    fromString = toJSString
