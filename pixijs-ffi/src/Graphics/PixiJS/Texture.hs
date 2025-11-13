{-# LANGUAGE OverloadedStrings #-}

-- | Pixi.js Texture bindings
--
-- This module provides functions for creating and manipulating textures
-- in Pixi.js.
module Graphics.PixiJS.Texture
    ( -- * Texture Creation
      textureFrom
    , textureFromCanvas
    , textureFromOffscreenCanvas
    , textureFromImage
    , textureFromURL
    , textureEmpty
    , textureWhite
      -- * Texture Properties
    , getTextureWidth
    , getTextureHeight
    , getBaseTexture
    , getFrame
    , setFrame
      -- * Texture Methods
    , updateTexture
    , destroyTexture
    , cloneTexture
      -- * RenderTexture
    , newRenderTexture
    , createRenderTexture
      -- * Video Texture
    , textureFromVideo
    ) where

import Graphics.PixiJS.Types
import GHC.Wasm.Prim

-- *****************************************************************************
-- * Texture Creation
-- *****************************************************************************

-- | Creates a texture from various sources (canvas, image, video, etc.)
--
-- @param source The source object (Canvas, Image, Video, etc.)
-- @return A new texture
foreign import javascript unsafe "PIXI.Texture.from($1)"
    textureFrom :: JSVal -> IO Texture

-- | Creates a texture from a canvas element
--
-- @param canvas The canvas element
-- @return A new texture
foreign import javascript unsafe "PIXI.Texture.from($1)"
    textureFromCanvas :: Canvas -> IO Texture

-- | Creates a texture from an OffscreenCanvas
--
-- @param offscreenCanvas The OffscreenCanvas
-- @return A new texture
foreign import javascript unsafe "PIXI.Texture.from($1)"
    textureFromOffscreenCanvas :: JSVal -> IO Texture

-- | Creates a texture from an image element
--
-- @param image The image element
-- @return A new texture
foreign import javascript unsafe "PIXI.Texture.from($1)"
    textureFromImage :: JSVal -> IO Texture

-- | Creates a texture from a URL
--
-- @param url The URL of the image
-- @return A new texture (loaded asynchronously)
foreign import javascript safe "await PIXI.Texture.from($1)"
    textureFromURL :: JSString -> IO Texture

-- | Creates an empty texture
foreign import javascript unsafe "PIXI.Texture.EMPTY"
    textureEmpty :: IO Texture

-- | Gets the built-in white texture
foreign import javascript unsafe "PIXI.Texture.WHITE"
    textureWhite :: IO Texture

-- *****************************************************************************
-- * Texture Properties
-- *****************************************************************************

-- | Gets the width of a texture
foreign import javascript unsafe "$1.width"
    getTextureWidth :: Texture -> IO Float

-- | Gets the height of a texture
foreign import javascript unsafe "$1.height"
    getTextureHeight :: Texture -> IO Float

-- | Gets the base texture
foreign import javascript unsafe "$1.baseTexture"
    getBaseTexture :: Texture -> IO JSVal

-- | Gets the frame rectangle of the texture
foreign import javascript unsafe "$1.frame"
    getFrame :: Texture -> IO Rectangle

-- | Sets the frame rectangle of the texture
foreign import javascript unsafe "$1.frame = $2"
    setFrame :: Texture -> Rectangle -> IO ()

-- *****************************************************************************
-- * Texture Methods
-- *****************************************************************************

-- | Updates the texture
foreign import javascript unsafe "$1.update()"
    updateTexture :: Texture -> IO ()

-- | Destroys the texture
--
-- @param texture The texture to destroy
-- @param destroyBase Whether to destroy the base texture as well
foreign import javascript unsafe "$1.destroy($2)"
    destroyTexture :: Texture -> Bool -> IO ()

-- | Creates a clone of the texture
foreign import javascript unsafe "$1.clone()"
    cloneTexture :: Texture -> IO Texture

-- *****************************************************************************
-- * RenderTexture
-- *****************************************************************************

-- | Creates a new RenderTexture
--
-- @param width The width of the render texture
-- @param height The height of the render texture
-- @return A new RenderTexture
foreign import javascript unsafe "PIXI.RenderTexture.create({width: $1, height: $2})"
    newRenderTexture :: Int -> Int -> IO RenderTexture

-- | Creates a new RenderTexture with options
--
-- @param options A JavaScript object containing options
-- @return A new RenderTexture
foreign import javascript unsafe "PIXI.RenderTexture.create($1)"
    createRenderTexture :: JSVal -> IO RenderTexture

-- *****************************************************************************
-- * Video Texture
-- *****************************************************************************

-- | Creates a texture from a video element or URL
--
-- @param source The video element or URL
-- @return A new texture
foreign import javascript safe "await PIXI.Texture.from($1)"
    textureFromVideo :: JSVal -> IO Texture
