{-# LANGUAGE OverloadedStrings #-}

-- | Pixi.js Display Object bindings
--
-- This module provides functions for working with Pixi.js display objects,
-- including Containers, Sprites, and AnimatedSprites. Display objects are
-- the building blocks of any Pixi.js scene.
module Graphics.PixiJS.Display
    ( -- * Container
      newContainer
    , addChild
    , addChildAt
    , removeChild
    , removeChildAt
    , getChildAt
    , getChildIndex
    , setChildIndex
    , getNumChildren
      -- * Sprite
    , newSprite
    , newSpriteFromTexture
      -- * AnimatedSprite
    , newAnimatedSprite
    , playAnimatedSprite
    , stopAnimatedSprite
    , gotoAndPlay
    , gotoAndStop
    , getAnimationSpeed
    , setAnimationSpeed
    , getLoop
    , setLoop
      -- * Display Object Properties
      -- ** Position
    , getX
    , setX
    , getY
    , setY
    , setPosition
    , getPosition
      -- ** Scale
    , getScaleX
    , setScaleX
    , getScaleY
    , setScaleY
    , setScale
      -- ** Rotation
    , getRotation
    , setRotation
      -- ** Anchor
    , setAnchor
    , setAnchorX
    , setAnchorY
      -- ** Visibility
    , getVisible
    , setVisible
      -- ** Alpha
    , getAlpha
    , setAlpha
      -- ** Tint
    , getTint
    , setTint
      -- ** Dimensions
    , getWidth
    , setWidth
    , getHeight
    , setHeight
      -- ** Pivot
    , setPivot
    , setPivotX
    , setPivotY
      -- ** Interactive
    , setInteractive
    , setButtonMode
      -- ** Masks
    , setMask
    , getMask
      -- ** Parent
    , getParent
      -- * Bounds
    , getBounds
    , getLocalBounds
    ) where

import Graphics.PixiJS.Types
import GHC.Wasm.Prim

-- *****************************************************************************
-- * Container
-- *****************************************************************************

-- | Creates a new Pixi.js Container
-- Containers can hold multiple display objects and be moved together
foreign import javascript unsafe "new PIXI.Container()"
    newContainer :: IO Container

-- | Adds a child display object to a container
--
-- @param container The container to add the child to
-- @param child The child object to add
-- @return The added child
foreign import javascript unsafe "$1.addChild($2)"
    addChild :: Container -> JSVal -> IO JSVal

-- | Adds a child display object to a container at a specific index
--
-- @param container The container to add the child to
-- @param child The child object to add
-- @param index The index at which to add the child
-- @return The added child
foreign import javascript unsafe "$1.addChildAt($2, $3)"
    addChildAt :: Container -> JSVal -> Int -> IO JSVal

-- | Removes a child display object from a container
--
-- @param container The container to remove the child from
-- @param child The child object to remove
-- @return The removed child
foreign import javascript unsafe "$1.removeChild($2)"
    removeChild :: Container -> JSVal -> IO JSVal

-- | Removes a child at a specific index from a container
--
-- @param container The container to remove the child from
-- @param index The index of the child to remove
-- @return The removed child
foreign import javascript unsafe "$1.removeChildAt($2)"
    removeChildAt :: Container -> Int -> IO JSVal

-- | Gets a child at a specific index
--
-- @param container The container
-- @param index The index of the child
-- @return The child at the index
foreign import javascript unsafe "$1.getChildAt($2)"
    getChildAt :: Container -> Int -> IO JSVal

-- | Gets the index of a child in the container
--
-- @param container The container
-- @param child The child object
-- @return The index of the child
foreign import javascript unsafe "$1.getChildIndex($2)"
    getChildIndex :: Container -> JSVal -> IO Int

-- | Sets the index of a child in the container
--
-- @param container The container
-- @param child The child object
-- @param index The new index
foreign import javascript unsafe "$1.setChildIndex($2, $3)"
    setChildIndex :: Container -> JSVal -> Int -> IO ()

-- | Gets the number of children in a container
--
-- @param container The container
-- @return The number of children
foreign import javascript unsafe "$1.children.length"
    getNumChildren :: Container -> IO Int

-- *****************************************************************************
-- * Sprite
-- *****************************************************************************

-- | Creates a new empty Pixi.js Sprite
foreign import javascript unsafe "new PIXI.Sprite()"
    newSprite :: IO Sprite

-- | Creates a new Pixi.js Sprite from a texture
--
-- @param texture The texture to use for the sprite
-- @return A new Sprite object
foreign import javascript unsafe "new PIXI.Sprite($1)"
    newSpriteFromTexture :: Texture -> IO Sprite

-- *****************************************************************************
-- * AnimatedSprite
-- *****************************************************************************

-- | Creates a new Pixi.js AnimatedSprite from an array of textures
--
-- @param textures The array of textures for animation
-- @return A new AnimatedSprite object
foreign import javascript unsafe "new PIXI.AnimatedSprite($1)"
    newAnimatedSprite :: JSVal -> IO AnimatedSprite

-- | Plays the animated sprite
foreign import javascript unsafe "$1.play()"
    playAnimatedSprite :: AnimatedSprite -> IO ()

-- | Stops the animated sprite
foreign import javascript unsafe "$1.stop()"
    stopAnimatedSprite :: AnimatedSprite -> IO ()

-- | Goes to a specific frame and plays from there
--
-- @param sprite The animated sprite
-- @param frame The frame number to go to
foreign import javascript unsafe "$1.gotoAndPlay($2)"
    gotoAndPlay :: AnimatedSprite -> Int -> IO ()

-- | Goes to a specific frame and stops
--
-- @param sprite The animated sprite
-- @param frame The frame number to go to
foreign import javascript unsafe "$1.gotoAndStop($2)"
    gotoAndStop :: AnimatedSprite -> Int -> IO ()

-- | Gets the animation speed
foreign import javascript unsafe "$1.animationSpeed"
    getAnimationSpeed :: AnimatedSprite -> IO Float

-- | Sets the animation speed (1.0 = normal speed)
foreign import javascript unsafe "$1.animationSpeed = $2"
    setAnimationSpeed :: AnimatedSprite -> Float -> IO ()

-- | Gets whether the animation loops
foreign import javascript unsafe "$1.loop"
    getLoop :: AnimatedSprite -> IO Bool

-- | Sets whether the animation loops
foreign import javascript unsafe "$1.loop = $2"
    setLoop :: AnimatedSprite -> Bool -> IO ()

-- *****************************************************************************
-- * Display Object Properties - Position
-- *****************************************************************************

-- | Gets the x position of a display object
foreign import javascript unsafe "$1.x"
    getX :: JSVal -> IO Float

-- | Sets the x position of a display object
foreign import javascript unsafe "$1.x = $2"
    setX :: JSVal -> Float -> IO ()

-- | Gets the y position of a display object
foreign import javascript unsafe "$1.y"
    getY :: JSVal -> IO Float

-- | Sets the y position of a display object
foreign import javascript unsafe "$1.y = $2"
    setY :: JSVal -> Float -> IO ()

-- | Sets the position of a display object
--
-- @param obj The display object
-- @param x The x coordinate
-- @param y The y coordinate
foreign import javascript unsafe "$1.position.set($2, $3)"
    setPosition :: JSVal -> Float -> Float -> IO ()

-- | Gets the position of a display object as a Point
foreign import javascript unsafe "$1.position"
    getPosition :: JSVal -> IO Point

-- *****************************************************************************
-- * Display Object Properties - Scale
-- *****************************************************************************

-- | Gets the x scale of a display object
foreign import javascript unsafe "$1.scale.x"
    getScaleX :: JSVal -> IO Float

-- | Sets the x scale of a display object
foreign import javascript unsafe "$1.scale.x = $2"
    setScaleX :: JSVal -> Float -> IO ()

-- | Gets the y scale of a display object
foreign import javascript unsafe "$1.scale.y"
    getScaleY :: JSVal -> IO Float

-- | Sets the y scale of a display object
foreign import javascript unsafe "$1.scale.y = $2"
    setScaleY :: JSVal -> Float -> IO ()

-- | Sets the uniform scale of a display object
--
-- @param obj The display object
-- @param scale The scale value (1.0 = normal size)
foreign import javascript unsafe "$1.scale.set($2)"
    setScale :: JSVal -> Float -> IO ()

-- *****************************************************************************
-- * Display Object Properties - Rotation
-- *****************************************************************************

-- | Gets the rotation of a display object (in radians)
foreign import javascript unsafe "$1.rotation"
    getRotation :: JSVal -> IO Float

-- | Sets the rotation of a display object (in radians)
foreign import javascript unsafe "$1.rotation = $2"
    setRotation :: JSVal -> Float -> IO ()

-- *****************************************************************************
-- * Display Object Properties - Anchor
-- *****************************************************************************

-- | Sets the anchor point of a sprite
-- The anchor determines the point around which transformations are applied
-- A value of 0.5 means the anchor is at the center
--
-- @param sprite The sprite whose anchor should be set
-- @param anchorValue The anchor value (typically 0.0 to 1.0)
foreign import javascript unsafe "$1.anchor.set($2)"
    setAnchor :: Sprite -> Float -> IO ()

-- | Sets the x anchor of a sprite
foreign import javascript unsafe "$1.anchor.x = $2"
    setAnchorX :: Sprite -> Float -> IO ()

-- | Sets the y anchor of a sprite
foreign import javascript unsafe "$1.anchor.y = $2"
    setAnchorY :: Sprite -> Float -> IO ()

-- *****************************************************************************
-- * Display Object Properties - Visibility
-- *****************************************************************************

-- | Gets the visibility of a display object
foreign import javascript unsafe "$1.visible"
    getVisible :: JSVal -> IO Bool

-- | Sets the visibility of a display object
foreign import javascript unsafe "$1.visible = $2"
    setVisible :: JSVal -> Bool -> IO ()

-- *****************************************************************************
-- * Display Object Properties - Alpha
-- *****************************************************************************

-- | Gets the alpha (opacity) of a display object (0.0 to 1.0)
foreign import javascript unsafe "$1.alpha"
    getAlpha :: JSVal -> IO Float

-- | Sets the alpha (opacity) of a display object (0.0 to 1.0)
foreign import javascript unsafe "$1.alpha = $2"
    setAlpha :: JSVal -> Float -> IO ()

-- *****************************************************************************
-- * Display Object Properties - Tint
-- *****************************************************************************

-- | Gets the tint color of a sprite (as a number)
foreign import javascript unsafe "$1.tint"
    getTint :: Sprite -> IO Int

-- | Sets the tint color of a sprite
-- The tint is a hex color value (e.g., 0xFFFFFF for white)
foreign import javascript unsafe "$1.tint = $2"
    setTint :: Sprite -> Int -> IO ()

-- *****************************************************************************
-- * Display Object Properties - Dimensions
-- *****************************************************************************

-- | Gets the width of a display object
foreign import javascript unsafe "$1.width"
    getWidth :: JSVal -> IO Float

-- | Sets the width of a display object
foreign import javascript unsafe "$1.width = $2"
    setWidth :: JSVal -> Float -> IO ()

-- | Gets the height of a display object
foreign import javascript unsafe "$1.height"
    getHeight :: JSVal -> IO Float

-- | Sets the height of a display object
foreign import javascript unsafe "$1.height = $2"
    setHeight :: JSVal -> Float -> IO ()

-- *****************************************************************************
-- * Display Object Properties - Pivot
-- *****************************************************************************

-- | Sets the pivot point of a display object
--
-- @param obj The display object
-- @param x The x coordinate of the pivot
-- @param y The y coordinate of the pivot
foreign import javascript unsafe "$1.pivot.set($2, $3)"
    setPivot :: JSVal -> Float -> Float -> IO ()

-- | Sets the x pivot of a display object
foreign import javascript unsafe "$1.pivot.x = $2"
    setPivotX :: JSVal -> Float -> IO ()

-- | Sets the y pivot of a display object
foreign import javascript unsafe "$1.pivot.y = $2"
    setPivotY :: JSVal -> Float -> IO ()

-- *****************************************************************************
-- * Display Object Properties - Interactive
-- *****************************************************************************

-- | Sets whether a display object is interactive (can receive events)
foreign import javascript unsafe "$1.interactive = $2"
    setInteractive :: JSVal -> Bool -> IO ()

-- | Sets whether a display object acts as a button (changes cursor on hover)
foreign import javascript unsafe "$1.buttonMode = $2"
    setButtonMode :: JSVal -> Bool -> IO ()

-- *****************************************************************************
-- * Display Object Properties - Masks
-- *****************************************************************************

-- | Sets a mask for a display object
foreign import javascript unsafe "$1.mask = $2"
    setMask :: JSVal -> JSVal -> IO ()

-- | Gets the mask of a display object
foreign import javascript unsafe "$1.mask"
    getMask :: JSVal -> IO JSVal

-- *****************************************************************************
-- * Display Object Properties - Parent
-- *****************************************************************************

-- | Gets the parent of a display object
foreign import javascript unsafe "$1.parent"
    getParent :: JSVal -> IO Container

-- *****************************************************************************
-- * Bounds
-- *****************************************************************************

-- | Gets the bounds of a display object in global coordinates
foreign import javascript unsafe "$1.getBounds()"
    getBounds :: JSVal -> IO Rectangle

-- | Gets the bounds of a display object in local coordinates
foreign import javascript unsafe "$1.getLocalBounds()"
    getLocalBounds :: JSVal -> IO Rectangle
