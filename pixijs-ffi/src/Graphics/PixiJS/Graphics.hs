{-# LANGUAGE OverloadedStrings #-}

-- | Pixi.js Graphics bindings
--
-- This module provides functions for creating and manipulating vector graphics
-- in Pixi.js. The Graphics class allows you to draw shapes, lines, and fills
-- programmatically.
module Graphics.PixiJS.Graphics
    ( -- * Graphics Creation
      newGraphics
      -- * Line Style
    , lineStyle
    , lineStyleWithOptions
    , lineTextureStyle
      -- * Fill Style
    , beginFill
    , beginFillWithAlpha
    , endFill
    , beginTextureFill
      -- * Drawing Shapes
    , drawRect
    , drawRoundedRect
    , drawCircle
    , drawEllipse
    , drawPolygon
    , drawStar
      -- * Drawing Paths
    , moveTo
    , lineTo
    , bezierCurveTo
    , quadraticCurveTo
    , arcTo
    , arc
    , closePath
      -- * Clear
    , clear
    , clearRect
      -- * SVG
    , loadSVG
    , drawSVG
      -- * Advanced
    , setFillStyle
    , setLineStyle
    , clone
    , containsPoint
    ) where

import Graphics.PixiJS.Types
import GHC.Wasm.Prim

-- *****************************************************************************
-- * Graphics Creation
-- *****************************************************************************

-- | Creates a new Pixi.js Graphics object
foreign import javascript unsafe "new PIXI.Graphics()"
    newGraphics :: IO Graphics

-- *****************************************************************************
-- * Line Style
-- *****************************************************************************

-- | Sets the line style for subsequent drawing operations
--
-- @param graphics The graphics object
-- @param width The line width
-- @param color The line color (as a hex string or number)
foreign import javascript unsafe "$1.lineStyle($2, $3)"
    lineStyle :: Graphics -> Float -> JSString -> IO ()

-- | Sets the line style with full options
--
-- @param graphics The graphics object
-- @param width The line width
-- @param color The line color (as a number, e.g., 0xFF0000)
-- @param alpha The line alpha (0.0 to 1.0)
-- @param alignment The alignment (0.0 = inner, 0.5 = center, 1.0 = outer)
foreign import javascript unsafe "$1.lineStyle({width: $2, color: $3, alpha: $4, alignment: $5})"
    lineStyleWithOptions :: Graphics -> Float -> Int -> Float -> Float -> IO ()

-- | Sets a textured line style
--
-- @param graphics The graphics object
-- @param texture The texture to use for the line
-- @param width The line width
foreign import javascript unsafe "$1.lineTextureStyle({texture: $2, width: $3})"
    lineTextureStyle :: Graphics -> Texture -> Float -> IO ()

-- *****************************************************************************
-- * Fill Style
-- *****************************************************************************

-- | Begins a fill with the specified color
--
-- @param graphics The graphics object
-- @param color The fill color (as a number, e.g., 0xFF0000)
foreign import javascript unsafe "$1.beginFill($2)"
    beginFill :: Graphics -> Int -> IO ()

-- | Begins a fill with the specified color and alpha
--
-- @param graphics The graphics object
-- @param color The fill color (as a number, e.g., 0xFF0000)
-- @param alpha The fill alpha (0.0 to 1.0)
foreign import javascript unsafe "$1.beginFill($2, $3)"
    beginFillWithAlpha :: Graphics -> Int -> Float -> IO ()

-- | Ends the current fill
foreign import javascript unsafe "$1.endFill()"
    endFill :: Graphics -> IO ()

-- | Begins a textured fill
--
-- @param graphics The graphics object
-- @param texture The texture to use for the fill
foreign import javascript unsafe "$1.beginTextureFill({texture: $2})"
    beginTextureFill :: Graphics -> Texture -> IO ()

-- *****************************************************************************
-- * Drawing Shapes
-- *****************************************************************************

-- | Draws a rectangle
--
-- @param graphics The graphics object
-- @param x The x coordinate
-- @param y The y coordinate
-- @param width The width
-- @param height The height
foreign import javascript unsafe "$1.drawRect($2, $3, $4, $5)"
    drawRect :: Graphics -> Float -> Float -> Float -> Float -> IO ()

-- | Draws a rectangle with rounded corners
--
-- @param graphics The graphics object
-- @param x The x coordinate
-- @param y The y coordinate
-- @param width The width
-- @param height The height
-- @param radius The corner radius
foreign import javascript unsafe "$1.drawRoundedRect($2, $3, $4, $5, $6)"
    drawRoundedRect :: Graphics -> Float -> Float -> Float -> Float -> Float -> IO ()

-- | Draws a circle
--
-- @param graphics The graphics object
-- @param x The x coordinate of the center
-- @param y The y coordinate of the center
-- @param radius The radius
foreign import javascript unsafe "$1.drawCircle($2, $3, $4)"
    drawCircle :: Graphics -> Float -> Float -> Float -> IO ()

-- | Draws an ellipse
--
-- @param graphics The graphics object
-- @param x The x coordinate of the center
-- @param y The y coordinate of the center
-- @param width The width
-- @param height The height
foreign import javascript unsafe "$1.drawEllipse($2, $3, $4, $5)"
    drawEllipse :: Graphics -> Float -> Float -> Float -> Float -> IO ()

-- | Draws a polygon from an array of points
--
-- The points should be a JavaScript array of numbers: [x1, y1, x2, y2, ...]
--
-- @param graphics The graphics object
-- @param points An array of point coordinates
foreign import javascript unsafe "$1.drawPolygon($2)"
    drawPolygon :: Graphics -> JSVal -> IO ()

-- | Draws a star shape
--
-- @param graphics The graphics object
-- @param x The x coordinate of the center
-- @param y The y coordinate of the center
-- @param points The number of points on the star
-- @param radius The outer radius
-- @param innerRadius The inner radius (optional)
foreign import javascript unsafe "$1.drawStar($2, $3, $4, $5, $6)"
    drawStar :: Graphics -> Float -> Float -> Int -> Float -> Float -> IO ()

-- *****************************************************************************
-- * Drawing Paths
-- *****************************************************************************

-- | Moves the drawing cursor to a new position without drawing
--
-- @param graphics The graphics object
-- @param x The x coordinate
-- @param y The y coordinate
foreign import javascript unsafe "$1.moveTo($2, $3)"
    moveTo :: Graphics -> Float -> Float -> IO ()

-- | Draws a line from the current position to a new position
--
-- @param graphics The graphics object
-- @param x The x coordinate
-- @param y The y coordinate
foreign import javascript unsafe "$1.lineTo($2, $3)"
    lineTo :: Graphics -> Float -> Float -> IO ()

-- | Draws a cubic Bezier curve
--
-- @param graphics The graphics object
-- @param cpX First control point x
-- @param cpY First control point y
-- @param cpX2 Second control point x
-- @param cpY2 Second control point y
-- @param toX End point x
-- @param toY End point y
foreign import javascript unsafe "$1.bezierCurveTo($2, $3, $4, $5, $6, $7)"
    bezierCurveTo :: Graphics -> Float -> Float -> Float -> Float -> Float -> Float -> IO ()

-- | Draws a quadratic curve
--
-- @param graphics The graphics object
-- @param cpX Control point x
-- @param cpY Control point y
-- @param toX End point x
-- @param toY End point y
foreign import javascript unsafe "$1.quadraticCurveTo($2, $3, $4, $5)"
    quadraticCurveTo :: Graphics -> Float -> Float -> Float -> Float -> IO ()

-- | Draws an arc between two points
--
-- @param graphics The graphics object
-- @param x1 First point x
-- @param y1 First point y
-- @param x2 Second point x
-- @param y2 Second point y
-- @param radius The radius of the arc
foreign import javascript unsafe "$1.arcTo($2, $3, $4, $5, $6)"
    arcTo :: Graphics -> Float -> Float -> Float -> Float -> Float -> IO ()

-- | Draws an arc
--
-- @param graphics The graphics object
-- @param cx Center x
-- @param cy Center y
-- @param radius The radius
-- @param startAngle Start angle in radians
-- @param endAngle End angle in radians
-- @param anticlockwise Whether to draw counterclockwise
foreign import javascript unsafe "$1.arc($2, $3, $4, $5, $6, $7)"
    arc :: Graphics -> Float -> Float -> Float -> Float -> Float -> Bool -> IO ()

-- | Closes the current path
foreign import javascript unsafe "$1.closePath()"
    closePath :: Graphics -> IO ()

-- *****************************************************************************
-- * Clear
-- *****************************************************************************

-- | Clears all graphics drawn to this Graphics object
foreign import javascript unsafe "$1.clear()"
    clear :: Graphics -> IO ()

-- | Clears a rectangular area
--
-- @param graphics The graphics object
-- @param x The x coordinate
-- @param y The y coordinate
-- @param width The width
-- @param height The height
foreign import javascript unsafe "$1.clearRect($2, $3, $4, $5)"
    clearRect :: Graphics -> Float -> Float -> Float -> Float -> IO ()

-- *****************************************************************************
-- * SVG
-- *****************************************************************************

-- | Loads SVG data into a Graphics object
--
-- @param svgString The SVG data as a string
-- @return A new Graphics object with the SVG rendered
foreign import javascript safe
 """
 const graphics = new PIXI.Graphics();
 await graphics.svg($1);
 return graphics;
 """
  loadSVG :: JSString -> IO Graphics

-- | Draws SVG data into an existing Graphics object
--
-- @param graphics The graphics object
-- @param svgString The SVG data as a string
foreign import javascript safe "await $1.svg($2)"
    drawSVG :: Graphics -> JSString -> IO ()

-- *****************************************************************************
-- * Advanced
-- *****************************************************************************

-- | Sets the fill style using a style object
foreign import javascript unsafe "$1.fill = $2"
    setFillStyle :: Graphics -> JSVal -> IO ()

-- | Sets the line style using a style object
foreign import javascript unsafe "$1.line = $2"
    setLineStyle :: Graphics -> JSVal -> IO ()

-- | Creates a clone of the graphics object
foreign import javascript unsafe "$1.clone()"
    clone :: Graphics -> IO Graphics

-- | Checks if a point is contained within this graphics object
--
-- @param graphics The graphics object
-- @param point The point to test
-- @return True if the point is contained
foreign import javascript unsafe "$1.containsPoint($2)"
    containsPoint :: Graphics -> Point -> IO Bool
