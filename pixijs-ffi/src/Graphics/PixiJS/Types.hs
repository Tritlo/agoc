{-# LANGUAGE OverloadedStrings #-}

-- | Core types for Pixi.js FFI bindings
--
-- This module defines the fundamental types used throughout the Pixi.js FFI.
-- All Pixi.js objects are represented as opaque 'JSVal' references with
-- type aliases for clarity and type safety.
module Graphics.PixiJS.Types
    ( -- * Core Types
      JSVal
    , JSString
    , JSFunction
      -- * Application Types
    , Application
      -- * Display Types
    , Container
    , Sprite
    , AnimatedSprite
      -- * Graphics Types
    , Graphics
      -- * Text Types
    , Text
    , BitmapText
      -- * Texture Types
    , Texture
    , RenderTexture
      -- * Math Types
    , Point
    , Rectangle
    , Circle
    , Ellipse
    , Polygon
    , Matrix
      -- * Event Types
    , EventEmitter
    , FederatedEvent
      -- * Ticker Types
    , Ticker
    , TickerCallback
      -- * Filter Types
    , Filter
    , BlurFilter
    , ColorMatrixFilter
      -- * Other Types
    , Renderer
    , Canvas
    , ObservablePoint
    ) where

import GHC.Wasm.Prim (JSVal)

-- | A JavaScript string
type JSString = JSVal

-- | A JavaScript function represented as a JSVal
type JSFunction = JSVal

-- *****************************************************************************
-- * Application Types
-- *****************************************************************************

-- | A Pixi.js Application instance
type Application = JSVal

-- *****************************************************************************
-- * Display Types
-- *****************************************************************************

-- | A Pixi.js Container - can hold multiple display objects
type Container = JSVal

-- | A Pixi.js Sprite - displays a texture
type Sprite = JSVal

-- | A Pixi.js AnimatedSprite - plays animated sequences
type AnimatedSprite = JSVal

-- *****************************************************************************
-- * Graphics Types
-- *****************************************************************************

-- | A Pixi.js Graphics object for vector drawing
type Graphics = JSVal

-- *****************************************************************************
-- * Text Types
-- *****************************************************************************

-- | A Pixi.js Text object for rendering text
type Text = JSVal

-- | A Pixi.js BitmapText object for bitmap font rendering
type BitmapText = JSVal

-- *****************************************************************************
-- * Texture Types
-- *****************************************************************************

-- | A Pixi.js Texture
type Texture = JSVal

-- | A Pixi.js RenderTexture - a texture that can be rendered to
type RenderTexture = JSVal

-- *****************************************************************************
-- * Math Types
-- *****************************************************************************

-- | A 2D point
type Point = JSVal

-- | A rectangle
type Rectangle = JSVal

-- | A circle
type Circle = JSVal

-- | An ellipse
type Ellipse = JSVal

-- | A polygon
type Polygon = JSVal

-- | A 2D transformation matrix
type Matrix = JSVal

-- | An observable point that emits events when changed
type ObservablePoint = JSVal

-- *****************************************************************************
-- * Event Types
-- *****************************************************************************

-- | An event emitter
type EventEmitter = JSVal

-- | A federated event
type FederatedEvent = JSVal

-- *****************************************************************************
-- * Ticker Types
-- *****************************************************************************

-- | A Pixi.js Ticker for animation loops
type Ticker = JSVal

-- | A callback function for ticker updates
type TickerCallback = JSFunction

-- *****************************************************************************
-- * Filter Types
-- *****************************************************************************

-- | A Pixi.js Filter
type Filter = JSVal

-- | A blur filter
type BlurFilter = JSVal

-- | A color matrix filter
type ColorMatrixFilter = JSVal

-- *****************************************************************************
-- * Other Types
-- *****************************************************************************

-- | A Pixi.js Renderer
type Renderer = JSVal

-- | A HTML Canvas element
type Canvas = JSVal
