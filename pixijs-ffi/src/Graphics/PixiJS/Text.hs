{-# LANGUAGE OverloadedStrings #-}

-- | Pixi.js Text bindings
--
-- This module provides functions for creating and manipulating text objects
-- in Pixi.js, including regular Text and BitmapText.
module Graphics.PixiJS.Text
    ( -- * Text Creation
      newText
    , newTextWithStyle
    , newTextWithStyleObject
      -- * Text Properties
    , getText
    , setText
    , getTextStyle
    , setTextStyle
      -- * Text Style Properties
    , setFontFamily
    , setFontSize
    , setFill
    , setAlign
    , setFontWeight
    , setFontStyle
    , setStroke
    , setStrokeThickness
    , setDropShadow
    , setDropShadowColor
    , setDropShadowBlur
    , setDropShadowAngle
    , setDropShadowDistance
    , setWordWrap
    , setWordWrapWidth
    , setLineHeight
    , setLetterSpacing
      -- * BitmapText
    , newBitmapText
    , getBitmapText
    , setBitmapText
    , getBitmapFontSize
    , setBitmapFontSize
    , getBitmapTint
    , setBitmapTint
    ) where

import Graphics.PixiJS.Types
import GHC.Wasm.Prim

-- *****************************************************************************
-- * Text Creation
-- *****************************************************************************

-- | Creates a new Pixi.js Text object with default styling
--
-- @param text The text content to display
-- @return A new Text object
foreign import javascript unsafe "new PIXI.Text($1)"
   newText :: JSString -> IO Text

-- | Creates a new Pixi.js Text object with text and fill color
--
-- @param text The text content to display
-- @param fillColor The fill color as a JavaScript string (e.g., "white", "#FF0000")
-- @return A new Text object
foreign import javascript unsafe "new PIXI.Text({text: $1, style: {fill: $2}})"
   newTextWithStyle :: JSString -> JSString -> IO Text

-- | Creates a new Pixi.js Text object with a full style object
--
-- @param text The text content to display
-- @param styleObject A JavaScript object containing style properties
-- @return A new Text object
foreign import javascript unsafe "new PIXI.Text({text: $1, style: $2})"
   newTextWithStyleObject :: JSString -> JSVal -> IO Text

-- *****************************************************************************
-- * Text Properties
-- *****************************************************************************

-- | Gets the text content
foreign import javascript unsafe "$1.text"
    getText :: Text -> IO JSString

-- | Sets the text content
foreign import javascript unsafe "$1.text = $2"
    setText :: Text -> JSString -> IO ()

-- | Gets the style object
foreign import javascript unsafe "$1.style"
    getTextStyle :: Text -> IO JSVal

-- | Sets the style object
foreign import javascript unsafe "$1.style = $2"
    setTextStyle :: Text -> JSVal -> IO ()

-- *****************************************************************************
-- * Text Style Properties
-- *****************************************************************************

-- | Sets the font family
foreign import javascript unsafe "$1.style.fontFamily = $2"
    setFontFamily :: Text -> JSString -> IO ()

-- | Sets the font size (in pixels)
foreign import javascript unsafe "$1.style.fontSize = $2"
    setFontSize :: Text -> Int -> IO ()

-- | Sets the fill color
-- Can be a string (e.g., "white", "#FF0000") or number (e.g., 0xFFFFFF)
foreign import javascript unsafe "$1.style.fill = $2"
    setFill :: Text -> JSVal -> IO ()

-- | Sets the text alignment ("left", "center", "right")
foreign import javascript unsafe "$1.style.align = $2"
    setAlign :: Text -> JSString -> IO ()

-- | Sets the font weight ("normal", "bold", "100"-"900")
foreign import javascript unsafe "$1.style.fontWeight = $2"
    setFontWeight :: Text -> JSString -> IO ()

-- | Sets the font style ("normal", "italic", "oblique")
foreign import javascript unsafe "$1.style.fontStyle = $2"
    setFontStyle :: Text -> JSString -> IO ()

-- | Sets the stroke color
foreign import javascript unsafe "$1.style.stroke = $2"
    setStroke :: Text -> JSVal -> IO ()

-- | Sets the stroke thickness
foreign import javascript unsafe "$1.style.strokeThickness = $2"
    setStrokeThickness :: Text -> Int -> IO ()

-- | Enables or disables drop shadow
foreign import javascript unsafe "$1.style.dropShadow = $2"
    setDropShadow :: Text -> Bool -> IO ()

-- | Sets the drop shadow color
foreign import javascript unsafe "$1.style.dropShadowColor = $2"
    setDropShadowColor :: Text -> JSVal -> IO ()

-- | Sets the drop shadow blur radius
foreign import javascript unsafe "$1.style.dropShadowBlur = $2"
    setDropShadowBlur :: Text -> Float -> IO ()

-- | Sets the drop shadow angle (in radians)
foreign import javascript unsafe "$1.style.dropShadowAngle = $2"
    setDropShadowAngle :: Text -> Float -> IO ()

-- | Sets the drop shadow distance
foreign import javascript unsafe "$1.style.dropShadowDistance = $2"
    setDropShadowDistance :: Text -> Float -> IO ()

-- | Enables or disables word wrapping
foreign import javascript unsafe "$1.style.wordWrap = $2"
    setWordWrap :: Text -> Bool -> IO ()

-- | Sets the word wrap width
foreign import javascript unsafe "$1.style.wordWrapWidth = $2"
    setWordWrapWidth :: Text -> Float -> IO ()

-- | Sets the line height
foreign import javascript unsafe "$1.style.lineHeight = $2"
    setLineHeight :: Text -> Float -> IO ()

-- | Sets the letter spacing
foreign import javascript unsafe "$1.style.letterSpacing = $2"
    setLetterSpacing :: Text -> Float -> IO ()

-- *****************************************************************************
-- * BitmapText
-- *****************************************************************************

-- | Creates a new Pixi.js BitmapText object
--
-- @param text The text content to display
-- @param styleObject A JavaScript object containing style properties (font name, size, etc.)
-- @return A new BitmapText object
foreign import javascript unsafe "new PIXI.BitmapText({text: $1, style: $2})"
    newBitmapText :: JSString -> JSVal -> IO BitmapText

-- | Gets the bitmap text content
foreign import javascript unsafe "$1.text"
    getBitmapText :: BitmapText -> IO JSString

-- | Sets the bitmap text content
foreign import javascript unsafe "$1.text = $2"
    setBitmapText :: BitmapText -> JSString -> IO ()

-- | Gets the bitmap text font size
foreign import javascript unsafe "$1.fontSize"
    getBitmapFontSize :: BitmapText -> IO Int

-- | Sets the bitmap text font size
foreign import javascript unsafe "$1.fontSize = $2"
    setBitmapFontSize :: BitmapText -> Int -> IO ()

-- | Gets the tint color
foreign import javascript unsafe "$1.tint"
    getBitmapTint :: BitmapText -> IO Int

-- | Sets the tint color
foreign import javascript unsafe "$1.tint = $2"
    setBitmapTint :: BitmapText -> Int -> IO ()
