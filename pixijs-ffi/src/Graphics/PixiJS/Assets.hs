{-# LANGUAGE OverloadedStrings #-}

-- | Pixi.js Assets bindings
--
-- This module provides functions for loading and managing assets (textures,
-- sprites, fonts, etc.) using the Pixi.js Assets system.
module Graphics.PixiJS.Assets
    ( -- * Loading Assets
      loadAsset
    , loadAssets
    , addAsset
    , addBundle
    , loadBundle
      -- * Asset Management
    , getAsset
    , unloadAsset
    , unloadBundle
      -- * Cache
    , cacheGet
    , cacheSet
    , cacheHas
    , cacheRemove
    , cacheReset
    ) where

import Graphics.PixiJS.Types
import GHC.Wasm.Prim

-- *****************************************************************************
-- * Loading Assets
-- *****************************************************************************

-- | Loads a single asset (texture) asynchronously
-- This function uses a safe import because it needs to await the asset loading
--
-- Example:
-- @
-- texture <- loadAsset "path/to/image.png"
-- sprite <- newSpriteFromTexture texture
-- @
--
-- @param path The path to the asset as a JavaScript string
-- @return The loaded texture as a JSVal
foreign import javascript safe "await PIXI.Assets.load($1)"
    loadAsset :: JSString -> IO Texture

-- | Loads multiple assets asynchronously
--
-- The paths should be a JavaScript array of strings
--
-- @param paths An array of asset paths
-- @return An object containing the loaded assets, keyed by path
foreign import javascript safe "await PIXI.Assets.load($1)"
    loadAssets :: JSVal -> IO JSVal

-- | Adds an asset to the asset manager without loading it
--
-- @param alias The alias to use for this asset
-- @param path The path to the asset
foreign import javascript unsafe "PIXI.Assets.add($1, $2)"
    addAsset :: JSString -> JSString -> IO ()

-- | Adds a bundle of assets to the asset manager
--
-- @param bundleName The name of the bundle
-- @param assets An array or object containing asset definitions
foreign import javascript unsafe "PIXI.Assets.addBundle($1, $2)"
    addBundle :: JSString -> JSVal -> IO ()

-- | Loads a bundle of assets
--
-- @param bundleName The name of the bundle to load
-- @return An object containing the loaded assets
foreign import javascript safe "await PIXI.Assets.loadBundle($1)"
    loadBundle :: JSString -> IO JSVal

-- *****************************************************************************
-- * Asset Management
-- *****************************************************************************

-- | Gets an asset from the cache by its alias or path
--
-- @param key The alias or path of the asset
-- @return The asset, or null if not found
foreign import javascript unsafe "PIXI.Assets.get($1)"
    getAsset :: JSString -> IO JSVal

-- | Unloads an asset and removes it from the cache
--
-- @param key The alias or path of the asset to unload
foreign import javascript safe "await PIXI.Assets.unload($1)"
    unloadAsset :: JSString -> IO ()

-- | Unloads a bundle of assets
--
-- @param bundleName The name of the bundle to unload
foreign import javascript safe "await PIXI.Assets.unloadBundle($1)"
    unloadBundle :: JSString -> IO ()

-- *****************************************************************************
-- * Cache
-- *****************************************************************************

-- | Gets an asset from the texture cache
--
-- @param key The key of the asset
-- @return The asset, or null if not found
foreign import javascript unsafe "PIXI.Assets.cache.get($1)"
    cacheGet :: JSString -> IO JSVal

-- | Sets an asset in the texture cache
--
-- @param key The key for the asset
-- @param asset The asset to cache
foreign import javascript unsafe "PIXI.Assets.cache.set($1, $2)"
    cacheSet :: JSString -> JSVal -> IO ()

-- | Checks if an asset exists in the cache
--
-- @param key The key to check
-- @return True if the asset is cached
foreign import javascript unsafe "PIXI.Assets.cache.has($1)"
    cacheHas :: JSString -> IO Bool

-- | Removes an asset from the cache
--
-- @param key The key of the asset to remove
foreign import javascript unsafe "PIXI.Assets.cache.remove($1)"
    cacheRemove :: JSString -> IO ()

-- | Clears the entire asset cache
foreign import javascript unsafe "PIXI.Assets.cache.reset()"
    cacheReset :: IO ()
