{-# LANGUAGE OverloadedStrings #-}

-- | Pixi.js Math bindings
--
-- This module provides functions for working with Pixi.js mathematical
-- primitives like Points, Rectangles, Circles, and transformation matrices.
module Graphics.PixiJS.Math
    ( -- * Point
      newPoint
    , getPointX
    , setPointX
    , getPointY
    , setPointY
    , setPoint
    , copyPoint
    , clonePoint
      -- * Rectangle
    , newRectangle
    , getRectX
    , setRectX
    , getRectY
    , setRectY
    , getRectWidth
    , setRectWidth
    , getRectHeight
    , setRectHeight
    , getRectLeft
    , getRectRight
    , getRectTop
    , getRectBottom
    , containsRect
    , intersectsRect
    , cloneRect
      -- * Circle
    , newCircle
    , getCircleX
    , setCircleX
    , getCircleY
    , setCircleY
    , getCircleRadius
    , setCircleRadius
    , containsCircle
    , cloneCircle
      -- * Ellipse
    , newEllipse
    , getEllipseX
    , setEllipseX
    , getEllipseY
    , setEllipseY
    , getEllipseWidth
    , setEllipseWidth
    , getEllipseHeight
    , setEllipseHeight
    , containsEllipse
    , cloneEllipse
      -- * Polygon
    , newPolygon
    , clonePolygon
    , containsPolygon
      -- * Matrix
    , newMatrix
    , identityMatrix
    , translateMatrix
    , scaleMatrix
    , rotateMatrix
    , applyMatrix
    , invertMatrix
    , cloneMatrix
    ) where

import Graphics.PixiJS.Types

-- *****************************************************************************
-- * Point
-- *****************************************************************************

-- | Creates a new Point
--
-- @param x The x coordinate
-- @param y The y coordinate
foreign import javascript unsafe "new PIXI.Point($1, $2)"
    newPoint :: Float -> Float -> IO Point

-- | Gets the x coordinate of a point
foreign import javascript unsafe "$1.x"
    getPointX :: Point -> IO Float

-- | Sets the x coordinate of a point
foreign import javascript unsafe "$1.x = $2"
    setPointX :: Point -> Float -> IO ()

-- | Gets the y coordinate of a point
foreign import javascript unsafe "$1.y"
    getPointY :: Point -> IO Float

-- | Sets the y coordinate of a point
foreign import javascript unsafe "$1.y = $2"
    setPointY :: Point -> Float -> IO ()

-- | Sets both coordinates of a point
foreign import javascript unsafe "$1.set($2, $3)"
    setPoint :: Point -> Float -> Float -> IO ()

-- | Copies from another point
foreign import javascript unsafe "$1.copyFrom($2)"
    copyPoint :: Point -> Point -> IO ()

-- | Creates a clone of a point
foreign import javascript unsafe "$1.clone()"
    clonePoint :: Point -> IO Point

-- *****************************************************************************
-- * Rectangle
-- *****************************************************************************

-- | Creates a new Rectangle
--
-- @param x The x coordinate
-- @param y The y coordinate
-- @param width The width
-- @param height The height
foreign import javascript unsafe "new PIXI.Rectangle($1, $2, $3, $4)"
    newRectangle :: Float -> Float -> Float -> Float -> IO Rectangle

-- | Gets the x coordinate of a rectangle
foreign import javascript unsafe "$1.x"
    getRectX :: Rectangle -> IO Float

-- | Sets the x coordinate of a rectangle
foreign import javascript unsafe "$1.x = $2"
    setRectX :: Rectangle -> Float -> IO ()

-- | Gets the y coordinate of a rectangle
foreign import javascript unsafe "$1.y"
    getRectY :: Rectangle -> IO Float

-- | Sets the y coordinate of a rectangle
foreign import javascript unsafe "$1.y = $2"
    setRectY :: Rectangle -> Float -> IO ()

-- | Gets the width of a rectangle
foreign import javascript unsafe "$1.width"
    getRectWidth :: Rectangle -> IO Float

-- | Sets the width of a rectangle
foreign import javascript unsafe "$1.width = $2"
    setRectWidth :: Rectangle -> Float -> IO ()

-- | Gets the height of a rectangle
foreign import javascript unsafe "$1.height"
    getRectHeight :: Rectangle -> IO Float

-- | Sets the height of a rectangle
foreign import javascript unsafe "$1.height = $2"
    setRectHeight :: Rectangle -> Float -> IO ()

-- | Gets the left edge of a rectangle
foreign import javascript unsafe "$1.left"
    getRectLeft :: Rectangle -> IO Float

-- | Gets the right edge of a rectangle
foreign import javascript unsafe "$1.right"
    getRectRight :: Rectangle -> IO Float

-- | Gets the top edge of a rectangle
foreign import javascript unsafe "$1.top"
    getRectTop :: Rectangle -> IO Float

-- | Gets the bottom edge of a rectangle
foreign import javascript unsafe "$1.bottom"
    getRectBottom :: Rectangle -> IO Float

-- | Checks if a rectangle contains a point
--
-- @param rect The rectangle
-- @param x The x coordinate
-- @param y The y coordinate
foreign import javascript unsafe "$1.contains($2, $3)"
    containsRect :: Rectangle -> Float -> Float -> IO Bool

-- | Checks if two rectangles intersect
foreign import javascript unsafe "$1.intersects($2)"
    intersectsRect :: Rectangle -> Rectangle -> IO Bool

-- | Creates a clone of a rectangle
foreign import javascript unsafe "$1.clone()"
    cloneRect :: Rectangle -> IO Rectangle

-- *****************************************************************************
-- * Circle
-- *****************************************************************************

-- | Creates a new Circle
--
-- @param x The x coordinate of the center
-- @param y The y coordinate of the center
-- @param radius The radius
foreign import javascript unsafe "new PIXI.Circle($1, $2, $3)"
    newCircle :: Float -> Float -> Float -> IO Circle

-- | Gets the x coordinate of a circle's center
foreign import javascript unsafe "$1.x"
    getCircleX :: Circle -> IO Float

-- | Sets the x coordinate of a circle's center
foreign import javascript unsafe "$1.x = $2"
    setCircleX :: Circle -> Float -> IO ()

-- | Gets the y coordinate of a circle's center
foreign import javascript unsafe "$1.y"
    getCircleY :: Circle -> IO Float

-- | Sets the y coordinate of a circle's center
foreign import javascript unsafe "$1.y = $2"
    setCircleY :: Circle -> Float -> IO ()

-- | Gets the radius of a circle
foreign import javascript unsafe "$1.radius"
    getCircleRadius :: Circle -> IO Float

-- | Sets the radius of a circle
foreign import javascript unsafe "$1.radius = $2"
    setCircleRadius :: Circle -> Float -> IO ()

-- | Checks if a circle contains a point
foreign import javascript unsafe "$1.contains($2, $3)"
    containsCircle :: Circle -> Float -> Float -> IO Bool

-- | Creates a clone of a circle
foreign import javascript unsafe "$1.clone()"
    cloneCircle :: Circle -> IO Circle

-- *****************************************************************************
-- * Ellipse
-- *****************************************************************************

-- | Creates a new Ellipse
--
-- @param x The x coordinate of the center
-- @param y The y coordinate of the center
-- @param width The horizontal radius
-- @param height The vertical radius
foreign import javascript unsafe "new PIXI.Ellipse($1, $2, $3, $4)"
    newEllipse :: Float -> Float -> Float -> Float -> IO Ellipse

-- | Gets the x coordinate of an ellipse's center
foreign import javascript unsafe "$1.x"
    getEllipseX :: Ellipse -> IO Float

-- | Sets the x coordinate of an ellipse's center
foreign import javascript unsafe "$1.x = $2"
    setEllipseX :: Ellipse -> Float -> IO ()

-- | Gets the y coordinate of an ellipse's center
foreign import javascript unsafe "$1.y"
    getEllipseY :: Ellipse -> IO Float

-- | Sets the y coordinate of an ellipse's center
foreign import javascript unsafe "$1.y = $2"
    setEllipseY :: Ellipse -> Float -> IO ()

-- | Gets the horizontal radius of an ellipse (halfWidth in PixiJS v8)
foreign import javascript unsafe "$1.halfWidth"
    getEllipseWidth :: Ellipse -> IO Float

-- | Sets the horizontal radius of an ellipse (halfWidth in PixiJS v8)
foreign import javascript unsafe "$1.halfWidth = $2"
    setEllipseWidth :: Ellipse -> Float -> IO ()

-- | Gets the vertical radius of an ellipse (halfHeight in PixiJS v8)
foreign import javascript unsafe "$1.halfHeight"
    getEllipseHeight :: Ellipse -> IO Float

-- | Sets the vertical radius of an ellipse (halfHeight in PixiJS v8)
foreign import javascript unsafe "$1.halfHeight = $2"
    setEllipseHeight :: Ellipse -> Float -> IO ()

-- | Checks if an ellipse contains a point
foreign import javascript unsafe "$1.contains($2, $3)"
    containsEllipse :: Ellipse -> Float -> Float -> IO Bool

-- | Creates a clone of an ellipse
foreign import javascript unsafe "$1.clone()"
    cloneEllipse :: Ellipse -> IO Ellipse

-- *****************************************************************************
-- * Polygon
-- *****************************************************************************

-- | Creates a new Polygon from an array of points
--
-- The points should be a JavaScript array of numbers: [x1, y1, x2, y2, ...]
-- or an array of Point objects
foreign import javascript unsafe "new PIXI.Polygon($1)"
    newPolygon :: JSVal -> IO Polygon

-- | Creates a clone of a polygon
foreign import javascript unsafe "$1.clone()"
    clonePolygon :: Polygon -> IO Polygon

-- | Checks if a polygon contains a point
foreign import javascript unsafe "$1.contains($2, $3)"
    containsPolygon :: Polygon -> Float -> Float -> IO Bool

-- *****************************************************************************
-- * Matrix
-- *****************************************************************************

-- | Creates a new transformation matrix
foreign import javascript unsafe "new PIXI.Matrix()"
    newMatrix :: IO Matrix

-- | Resets a matrix to the identity matrix
foreign import javascript unsafe "$1.identity()"
    identityMatrix :: Matrix -> IO ()

-- | Translates a matrix
--
-- @param matrix The matrix to translate
-- @param x The x translation
-- @param y The y translation
foreign import javascript unsafe "$1.translate($2, $3)"
    translateMatrix :: Matrix -> Float -> Float -> IO ()

-- | Scales a matrix
--
-- @param matrix The matrix to scale
-- @param x The x scale factor
-- @param y The y scale factor
foreign import javascript unsafe "$1.scale($2, $3)"
    scaleMatrix :: Matrix -> Float -> Float -> IO ()

-- | Rotates a matrix
--
-- @param matrix The matrix to rotate
-- @param angle The rotation angle in radians
foreign import javascript unsafe "$1.rotate($2)"
    rotateMatrix :: Matrix -> Float -> IO ()

-- | Applies a matrix transformation to a point
--
-- @param matrix The transformation matrix
-- @param point The point to transform
-- @return The transformed point
foreign import javascript unsafe "$1.apply($2)"
    applyMatrix :: Matrix -> Point -> IO Point

-- | Inverts a matrix
foreign import javascript unsafe "$1.invert()"
    invertMatrix :: Matrix -> IO Matrix

-- | Creates a clone of a matrix
foreign import javascript unsafe "$1.clone()"
    cloneMatrix :: Matrix -> IO Matrix
