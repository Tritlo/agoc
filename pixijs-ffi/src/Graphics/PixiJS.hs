{-# LANGUAGE OverloadedStrings #-}

-- | Pixi.js FFI - Haskell bindings for Pixi.js v8
--
-- This is the main module that re-exports all Pixi.js functionality.
-- For most use cases, you can simply import this module to get access
-- to all Pixi.js features.
--
-- Example usage:
--
-- @
-- import Graphics.PixiJS
--
-- main :: IO ()
-- main = do
--     -- Create and initialize application
--     app <- newApp
--     initApp app "#1099bb"
--     appendCanvas app
--
--     -- Load an asset
--     texture <- loadAsset "bunny.png"
--
--     -- Create and position a sprite
--     sprite <- newSpriteFromTexture texture
--     setAnchor sprite 0.5
--     setPosition sprite 400 300
--
--     -- Add sprite to stage
--     stage <- getStage app
--     addChild stage sprite
--
--     -- Add animation
--     ticker <- getTicker app
--     callback <- jsFuncFromHs_ $ \\delta -> do
--         rot <- getRotation sprite
--         setRotation sprite (rot + 0.01)
--     add ticker callback
-- @
--
-- For more specific functionality, you can also import individual modules:
--
-- * "Graphics.PixiJS.Application" - Application management
-- * "Graphics.PixiJS.Display" - Display objects (Sprite, Container, etc.)
-- * "Graphics.PixiJS.Graphics" - Vector graphics
-- * "Graphics.PixiJS.Text" - Text rendering
-- * "Graphics.PixiJS.Assets" - Asset loading
-- * "Graphics.PixiJS.Texture" - Texture management
-- * "Graphics.PixiJS.Events" - Event handling
-- * "Graphics.PixiJS.Ticker" - Animation loops
-- * "Graphics.PixiJS.Math" - Mathematical primitives
-- * "Graphics.PixiJS.Filters" - Visual effects
-- * "Graphics.PixiJS.Gamepad" - Browser Gamepad API support
-- * "Graphics.PixiJS.Interop" - JavaScript interop utilities
module Graphics.PixiJS
    ( -- * Core Types
      module Graphics.PixiJS.Types
      -- * Application
    , module Graphics.PixiJS.Application
      -- * Display Objects
    , module Graphics.PixiJS.Display
      -- * Graphics
    , module Graphics.PixiJS.Graphics
      -- * Text
    , module Graphics.PixiJS.Text
      -- * Assets
    , module Graphics.PixiJS.Assets
      -- * Textures
    , module Graphics.PixiJS.Texture
      -- * Events
    , module Graphics.PixiJS.Events
      -- * Ticker
    , module Graphics.PixiJS.Ticker
      -- * Math
    , module Graphics.PixiJS.Math
      -- * Filters
    , module Graphics.PixiJS.Filters
      -- * Gamepad
    , module Graphics.PixiJS.Gamepad
      -- * Interop
    , module Graphics.PixiJS.Interop
    ) where

import Graphics.PixiJS.Types
import Graphics.PixiJS.Application
import Graphics.PixiJS.Display
import Graphics.PixiJS.Graphics
import Graphics.PixiJS.Text
import Graphics.PixiJS.Assets
import Graphics.PixiJS.Texture
import Graphics.PixiJS.Events
import Graphics.PixiJS.Ticker
import Graphics.PixiJS.Math
import Graphics.PixiJS.Filters
import Graphics.PixiJS.Gamepad
import Graphics.PixiJS.Interop
