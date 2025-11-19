{-# LANGUAGE OverloadedStrings #-}

-- | Pixi.js Filters bindings
--
-- This module provides functions for applying visual effects and filters
-- to display objects in Pixi.js.
module Graphics.PixiJS.Filters
    ( -- * Filter Management
      setFilters
    , getFilters
    , addFilter
    , removeFilter
      -- * Blur Filter
    , newBlurFilter
    , setBlurStrength
    , getBlurStrength
    , setBlurQuality
    , getBlurQuality
      -- * Color Matrix Filter
    , newColorMatrixFilter
    , colorMatrixBrightness
    , colorMatrixContrast
    , colorMatrixSaturate
    , colorMatrixHue
    , colorMatrixGrayscale
    , colorMatrixSepia
    , colorMatrixNegative
    , colorMatrixNight
    , colorMatrixPredator
    , colorMatrixLSD
    , colorMatrixReset
      -- * Other Common Filters
    , newAlphaFilter
    , newDisplacementFilter
    , newNoiseFilter
    ) where

import Graphics.PixiJS.Types
import GHC.Wasm.Prim

-- *****************************************************************************
-- * Filter Management
-- *****************************************************************************

-- | Sets the filters array on a display object
--
-- @param obj The display object
-- @param filters An array of filters
foreign import javascript unsafe "$1.filters = $2"
    setFilters :: JSVal -> JSVal -> IO ()

-- | Gets the filters array from a display object
foreign import javascript unsafe "$1.filters"
    getFilters :: JSVal -> IO JSVal

-- | Adds a filter to a display object
--
-- This is a helper function that handles the array manipulation
foreign import javascript unsafe
    """
    if (!$1.filters) {
        $1.filters = [];
    }
    $1.filters.push($2);
    """
    addFilter :: JSVal -> Filter -> IO ()

-- | Removes a filter from a display object
foreign import javascript unsafe
    """
    if ($1.filters) {
        const index = $1.filters.indexOf($2);
        if (index > -1) {
            $1.filters.splice(index, 1);
        }
    }
    """
    removeFilter :: JSVal -> Filter -> IO ()

-- *****************************************************************************
-- * Blur Filter
-- *****************************************************************************

-- | Creates a new BlurFilter
--
-- @param strength The strength of the blur (default: 8)
-- @param quality The quality of the blur (default: 4)
foreign import javascript unsafe "new PIXI.BlurFilter($1, $2)"
    newBlurFilter :: Float -> Int -> IO BlurFilter

-- | Sets the blur strength
foreign import javascript unsafe "$1.strength = $2"
    setBlurStrength :: BlurFilter -> Float -> IO ()

-- | Gets the blur strength
foreign import javascript unsafe "$1.strength"
    getBlurStrength :: BlurFilter -> IO Float

-- | Sets the blur quality
foreign import javascript unsafe "$1.quality = $2"
    setBlurQuality :: BlurFilter -> Int -> IO ()

-- | Gets the blur quality
foreign import javascript unsafe "$1.quality"
    getBlurQuality :: BlurFilter -> IO Int

-- *****************************************************************************
-- * Color Matrix Filter
-- *****************************************************************************

-- | Creates a new ColorMatrixFilter
foreign import javascript unsafe "new PIXI.ColorMatrixFilter()"
    newColorMatrixFilter :: IO ColorMatrixFilter

-- | Adjusts brightness
--
-- @param filter The color matrix filter
-- @param b The brightness value (0 to 1, where 0.5 is normal)
-- @param multiply Whether to multiply or override
foreign import javascript unsafe "$1.brightness($2, $3)"
    colorMatrixBrightness :: ColorMatrixFilter -> Float -> Bool -> IO ()

-- | Adjusts contrast
--
-- @param filter The color matrix filter
-- @param amount The contrast value (0 to 1, where 0.5 is normal)
-- @param multiply Whether to multiply or override
foreign import javascript unsafe "$1.contrast($2, $3)"
    colorMatrixContrast :: ColorMatrixFilter -> Float -> Bool -> IO ()

-- | Adjusts saturation
--
-- @param filter The color matrix filter
-- @param amount The saturation value (-1 to 1, where 0 is normal)
-- @param multiply Whether to multiply or override
foreign import javascript unsafe "$1.saturate($2, $3)"
    colorMatrixSaturate :: ColorMatrixFilter -> Float -> Bool -> IO ()

-- | Adjusts hue
--
-- @param filter The color matrix filter
-- @param rotation The hue rotation in degrees
-- @param multiply Whether to multiply or override
foreign import javascript unsafe "$1.hue($2, $3)"
    colorMatrixHue :: ColorMatrixFilter -> Float -> Bool -> IO ()

-- | Applies a grayscale effect
--
-- @param filter The color matrix filter
-- @param scale The scale of the grayscale (0 to 1)
-- @param multiply Whether to multiply or override
foreign import javascript unsafe "$1.grayscale($2, $3)"
    colorMatrixGrayscale :: ColorMatrixFilter -> Float -> Bool -> IO ()

-- | Applies a sepia effect
foreign import javascript unsafe "$1.sepia($2)"
    colorMatrixSepia :: ColorMatrixFilter -> Bool -> IO ()

-- | Applies a negative effect
foreign import javascript unsafe "$1.negative($2)"
    colorMatrixNegative :: ColorMatrixFilter -> Bool -> IO ()

-- | Applies a night vision effect
--
-- @param filter The color matrix filter
-- @param intensity The intensity (0 to 1)
-- @param multiply Whether to multiply or override
foreign import javascript unsafe "$1.night($2, $3)"
    colorMatrixNight :: ColorMatrixFilter -> Float -> Bool -> IO ()

-- | Applies a predator effect
--
-- @param filter The color matrix filter
-- @param amount The amount (0 to 1)
-- @param multiply Whether to multiply or override
foreign import javascript unsafe "$1.predator($2, $3)"
    colorMatrixPredator :: ColorMatrixFilter -> Float -> Bool -> IO ()

-- | Applies an LSD effect
foreign import javascript unsafe "$1.lsd($2)"
    colorMatrixLSD :: ColorMatrixFilter -> Bool -> IO ()

-- | Resets the color matrix to identity
foreign import javascript unsafe "$1.reset()"
    colorMatrixReset :: ColorMatrixFilter -> IO ()

-- *****************************************************************************
-- * Other Common Filters
-- *****************************************************************************

-- | Creates a new AlphaFilter
--
-- @param alpha The alpha value (0 to 1)
foreign import javascript unsafe "new PIXI.AlphaFilter($1)"
    newAlphaFilter :: Float -> IO Filter

-- | Creates a new DisplacementFilter
--
-- @param sprite The sprite to use for displacement
foreign import javascript unsafe "new PIXI.DisplacementFilter($1)"
    newDisplacementFilter :: Sprite -> IO Filter

-- | Creates a new NoiseFilter
--
-- @param noise The noise intensity (0 to 1)
foreign import javascript unsafe "new PIXI.NoiseFilter($1)"
    newNoiseFilter :: Float -> IO Filter
