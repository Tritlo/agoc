{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

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
import Data.Coerce (coerce)

-- *****************************************************************************
-- * Container
-- *****************************************************************************

foreign import javascript unsafe "new PIXI.Container()"
    js_newContainer :: IO JSVal

-- | Creates a new Pixi.js Container
-- Containers can hold multiple display objects and be moved together
newContainer :: IO Container
newContainer = fromJSVal <$> js_newContainer

foreign import javascript unsafe "$1.addChild($2)"
    js_addChild :: JSVal -> JSVal -> IO JSVal

-- | Adds a child display object to a container
--
-- @param container The container to add the child to
-- @param child The child object to add
-- @return The added child
addChild :: (IsContainer parent, IsContainer child) => parent -> child -> IO child
addChild parent child = do
    _ <- js_addChild (toJSVal parent) (toJSVal child)
    return child

foreign import javascript unsafe "$1.addChildAt($2, $3)"
    js_addChildAt :: JSVal -> JSVal -> Int -> IO JSVal

-- | Adds a child display object to a container at a specific index
--
-- @param container The container to add the child to
-- @param child The child object to add
-- @param index The index at which to add the child
-- @return The added child
addChildAt :: (IsContainer parent, IsContainer child) => parent -> child -> Int -> IO child
addChildAt parent child index = do
    _ <- js_addChildAt (toJSVal parent) (toJSVal child) index
    return child

foreign import javascript unsafe "$1.removeChild($2)"
    js_removeChild :: JSVal -> JSVal -> IO JSVal

-- | Removes a child display object from a container
--
-- @param container The container to remove the child from
-- @param child The child object to remove
-- @return The removed child
removeChild :: (IsContainer parent, IsContainer child) => parent -> child -> IO child
removeChild parent child = do
    _ <- js_removeChild (toJSVal parent) (toJSVal child)
    return child

foreign import javascript unsafe "$1.removeChildAt($2)"
    js_removeChildAt :: JSVal -> Int -> IO JSVal

-- | Removes a child at a specific index from a container
--
-- @param container The container to remove the child from
-- @param index The index of the child to remove
-- @return The removed child (as a generic Container, since we don't know the type)
removeChildAt :: (IsContainer parent) => parent -> Int -> IO Container
removeChildAt parent index = fromJSVal <$> js_removeChildAt (toJSVal parent) index

foreign import javascript unsafe "$1.getChildAt($2)"
    js_getChildAt :: JSVal -> Int -> IO JSVal

-- | Gets a child at a specific index
--
-- @param container The container
-- @param index The index of the child
-- @return The child at the index (as a generic Container)
getChildAt :: (IsContainer parent) => parent -> Int -> IO Container
getChildAt parent index = fromJSVal <$> js_getChildAt (toJSVal parent) index

foreign import javascript unsafe "$1.getChildIndex($2)"
    js_getChildIndex :: JSVal -> JSVal -> IO Int

-- | Gets the index of a child in the container
--
-- @param container The container
-- @param child The child to find the index of
-- @return The index of the child
getChildIndex :: (IsContainer parent, IsContainer child) => parent -> child -> IO Int
getChildIndex parent child = js_getChildIndex (toJSVal parent) (toJSVal child)

foreign import javascript unsafe "$1.setChildIndex($2, $3)"
    js_setChildIndex :: JSVal -> JSVal -> Int -> IO ()

-- | Sets the index of a child in the container
setChildIndex :: (IsContainer parent, IsContainer child) => parent -> child -> Int -> IO ()
setChildIndex parent child index = js_setChildIndex (toJSVal parent) (toJSVal child) index

foreign import javascript unsafe "$1.children.length"
    js_getNumChildren :: JSVal -> IO Int

-- | Gets the number of children in the container
getNumChildren :: (IsContainer parent) => parent -> IO Int
getNumChildren parent = js_getNumChildren (toJSVal parent)



-- | Creates a new Container




-- *****************************************************************************
-- * Sprite Operations
-- *****************************************************************************

-- | Creates a new empty Sprite
newSprite :: IO Sprite
newSprite = do
    val <- js_newSprite
    return $ fromJSVal val

foreign import javascript unsafe "new PIXI.Sprite()"
    js_newSprite :: IO JSVal

-- | Creates a new Sprite from a Texture
newSpriteFromTexture :: Texture -> IO Sprite
newSpriteFromTexture tex = do
    val <- js_newSpriteFromTexture (toJSVal tex)
    return $ fromJSVal val

foreign import javascript unsafe "new PIXI.Sprite($1)"
    js_newSpriteFromTexture :: JSVal -> IO JSVal

-- | Creates a new Sprite from an image URL
fromImage :: JSString -> IO Sprite
fromImage url = do
    val <- js_fromImage url
    return $ fromJSVal val

foreign import javascript unsafe "PIXI.Sprite.from($1)"
    js_fromImage :: JSString -> IO JSVal

-- *****************************************************************************
-- * AnimatedSprite Operations
-- *****************************************************************************

-- | Creates a new AnimatedSprite from a list of Textures
newAnimatedSprite :: [Texture] -> IO AnimatedSprite
newAnimatedSprite textures = do
    -- Convert list of textures to JS array (not implemented yet, using placeholder)
    -- For now, we assume textures is a JSVal representing an array
    -- TODO: Implement array conversion
    val <- js_newAnimatedSprite (error "Array conversion not implemented")
    return $ fromJSVal val

foreign import javascript unsafe "new PIXI.AnimatedSprite($1)"
    js_newAnimatedSprite :: JSVal -> IO JSVal

-- | Plays the animation
playAnimatedSprite :: IsAnimatedSprite s => s -> IO ()
playAnimatedSprite sprite = js_play (toJSVal sprite)

foreign import javascript unsafe "$1.play()"
    js_play :: JSVal -> IO ()

-- | Stops the animation
stopAnimatedSprite :: IsAnimatedSprite s => s -> IO ()
stopAnimatedSprite sprite = js_stop (toJSVal sprite)

foreign import javascript unsafe "$1.stop()"
    js_stop :: JSVal -> IO ()

-- | Sets the animation speed
setAnimationSpeed :: IsAnimatedSprite s => s -> Float -> IO ()
setAnimationSpeed sprite speed = js_setAnimationSpeed (toJSVal sprite) speed

foreign import javascript unsafe "$1.animationSpeed = $2"
    js_setAnimationSpeed :: JSVal -> Float -> IO ()

foreign import javascript unsafe "$1.animationSpeed"
    js_getAnimationSpeed :: JSVal -> IO Float

-- | Gets the animation speed
getAnimationSpeed :: IsAnimatedSprite s => s -> IO Float
getAnimationSpeed sprite = js_getAnimationSpeed (toJSVal sprite)

-- | Sets whether the animation should loop
setLoop :: IsAnimatedSprite s => s -> Bool -> IO ()
setLoop sprite loop = js_setLoop (toJSVal sprite) loop

foreign import javascript unsafe "$1.loop = $2"
    js_setLoop :: JSVal -> Bool -> IO ()

foreign import javascript unsafe "$1.loop"
    js_getLoop :: JSVal -> IO Bool

-- | Gets whether the animation loops
getLoop :: IsAnimatedSprite s => s -> IO Bool
getLoop sprite = js_getLoop (toJSVal sprite)

-- | Goes to a specific frame and plays
gotoAndPlay :: IsAnimatedSprite s => s -> Int -> IO ()
gotoAndPlay sprite frameNumber = js_gotoAndPlay (toJSVal sprite) frameNumber

foreign import javascript unsafe "$1.gotoAndPlay($2)"
    js_gotoAndPlay :: JSVal -> Int -> IO ()

-- | Goes to a specific frame and stops
gotoAndStop :: IsAnimatedSprite s => s -> Int -> IO ()
gotoAndStop sprite frameNumber = js_gotoAndStop (toJSVal sprite) frameNumber

foreign import javascript unsafe "$1.gotoAndStop($2)"
    js_gotoAndStop :: JSVal -> Int -> IO ()

-- *****************************************************************************
-- * Transform Operations
-- *****************************************************************************

-- | Sets the x coordinate
setX :: IsContainer c => c -> Float -> IO ()
setX obj x = js_setX (toJSVal obj) x

foreign import javascript unsafe "$1.x = $2"
    js_setX :: JSVal -> Float -> IO ()

-- | Sets the y coordinate
setY :: IsContainer c => c -> Float -> IO ()
setY obj y = js_setY (toJSVal obj) y

foreign import javascript unsafe "$1.y = $2"
    js_setY :: JSVal -> Float -> IO ()

-- | Gets the x coordinate
getX :: IsContainer c => c -> IO Float
getX obj = js_getX (toJSVal obj)

foreign import javascript unsafe "$1.x"
    js_getX :: JSVal -> IO Float

-- | Gets the y coordinate
getY :: IsContainer c => c -> IO Float
getY obj = js_getY (toJSVal obj)

foreign import javascript unsafe "$1.y"
    js_getY :: JSVal -> IO Float

-- | Sets the position
setPosition :: IsContainer c => c -> Float -> Float -> IO ()
setPosition obj x y = js_setPosition (toJSVal obj) x y

foreign import javascript unsafe "$1.position.set($2, $3)"
    js_setPosition :: JSVal -> Float -> Float -> IO ()

foreign import javascript unsafe "$1.position"
    js_getPosition :: JSVal -> IO JSVal

-- | Gets the position as a Point
getPosition :: IsContainer c => c -> IO Point
getPosition obj = Point <$> js_getPosition (toJSVal obj)

-- | Sets the rotation (in radians)
setRotation :: IsContainer c => c -> Float -> IO ()
setRotation obj rotation = js_setRotation (toJSVal obj) rotation

foreign import javascript unsafe "$1.rotation = $2"
    js_setRotation :: JSVal -> Float -> IO ()

-- | Gets the rotation (in radians)
getRotation :: IsContainer c => c -> IO Float
getRotation obj = js_getRotation (toJSVal obj)

foreign import javascript unsafe "$1.rotation"
    js_getRotation :: JSVal -> IO Float

-- | Sets the scale
setScale :: IsContainer c => c -> Float -> Float -> IO ()
setScale obj x y = js_setScale (toJSVal obj) x y

foreign import javascript unsafe "$1.scale.set($2, $3)"
    js_setScale :: JSVal -> Float -> Float -> IO ()

-- | Sets the x scale
setScaleX :: IsContainer c => c -> Float -> IO ()
setScaleX obj x = js_setScaleX (toJSVal obj) x

foreign import javascript unsafe "$1.scale.x = $2"
    js_setScaleX :: JSVal -> Float -> IO ()

foreign import javascript unsafe "$1.scale.x"
    js_getScaleX :: JSVal -> IO Float

-- | Gets the x scale
getScaleX :: IsContainer c => c -> IO Float
getScaleX obj = js_getScaleX (toJSVal obj)

-- | Sets the y scale
setScaleY :: IsContainer c => c -> Float -> IO ()
setScaleY obj y = js_setScaleY (toJSVal obj) y

foreign import javascript unsafe "$1.scale.y = $2"
    js_setScaleY :: JSVal -> Float -> IO ()

foreign import javascript unsafe "$1.scale.y"
    js_getScaleY :: JSVal -> IO Float

-- | Gets the y scale
getScaleY :: IsContainer c => c -> IO Float
getScaleY obj = js_getScaleY (toJSVal obj)

-- | Sets the anchor (for Sprites and Text)
setAnchor :: IsContainer c => c -> Float -> Float -> IO ()
setAnchor obj x y = js_setAnchor (toJSVal obj) x y

foreign import javascript unsafe "$1.anchor.set($2, $3)"
    js_setAnchor :: JSVal -> Float -> Float -> IO ()

-- | Sets the x anchor
setAnchorX :: IsContainer c => c -> Float -> IO ()
setAnchorX obj x = js_setAnchorX (toJSVal obj) x

foreign import javascript unsafe "$1.anchor.x = $2"
    js_setAnchorX :: JSVal -> Float -> IO ()

-- | Sets the y anchor
setAnchorY :: IsContainer c => c -> Float -> IO ()
setAnchorY obj y = js_setAnchorY (toJSVal obj) y

foreign import javascript unsafe "$1.anchor.y = $2"
    js_setAnchorY :: JSVal -> Float -> IO ()

foreign import javascript unsafe "$1.visible"
    js_getVisible :: JSVal -> IO Bool

-- | Gets whether the object is visible
getVisible :: IsContainer c => c -> IO Bool
getVisible obj = js_getVisible (toJSVal obj)

foreign import javascript unsafe "$1.visible = $2"
    js_setVisible :: JSVal -> Bool -> IO ()

-- | Sets whether the object is visible
setVisible :: IsContainer c => c -> Bool -> IO ()
setVisible obj visible = js_setVisible (toJSVal obj) visible

-- *****************************************************************************

foreign import javascript unsafe "$1.alpha"
    js_getAlpha :: JSVal -> IO Float

-- | Gets the alpha (opacity) of a display object (0.0 to 1.0)
getAlpha :: (IsContainer obj) => obj -> IO Float
getAlpha obj = js_getAlpha (toJSVal obj)

foreign import javascript unsafe "$1.alpha = $2"
    js_setAlpha :: JSVal -> Float -> IO ()

-- | Sets the alpha (opacity) of a display object (0.0 to 1.0)
setAlpha :: (IsContainer obj) => obj -> Float -> IO ()
setAlpha obj alpha = js_setAlpha (toJSVal obj) alpha

-- *****************************************************************************
-- * Display Object Properties - Tint
-- *****************************************************************************

foreign import javascript unsafe "$1.tint"
    js_getTint :: JSVal -> IO Int

-- | Gets the tint color of a sprite (as a number)
getTint :: (IsSprite sprite) => sprite -> IO Int
getTint sprite = js_getTint (toJSVal sprite)

foreign import javascript unsafe "$1.tint = $2"
    js_setTint :: JSVal -> Int -> IO ()

-- | Sets the tint color of a sprite
-- The tint is a hex color value (e.g., 0xFFFFFF for white)
setTint :: (IsSprite sprite) => sprite -> Int -> IO ()
setTint sprite tint = js_setTint (toJSVal sprite) tint

-- *****************************************************************************
-- * Display Object Properties - Dimensions
-- *****************************************************************************

foreign import javascript unsafe "$1.width"
    js_getWidth :: JSVal -> IO Float

-- | Gets the width of a display object
getWidth :: (IsContainer obj) => obj -> IO Float
getWidth obj = js_getWidth (toJSVal obj)

foreign import javascript unsafe "$1.width = $2"
    js_setWidth :: JSVal -> Float -> IO ()

-- | Sets the width of a display object
setWidth :: (IsContainer obj) => obj -> Float -> IO ()
setWidth obj width = js_setWidth (toJSVal obj) width

foreign import javascript unsafe "$1.height"
    js_getHeight :: JSVal -> IO Float

-- | Gets the height of a display object
getHeight :: (IsContainer obj) => obj -> IO Float
getHeight obj = js_getHeight (toJSVal obj)

foreign import javascript unsafe "$1.height = $2"
    js_setHeight :: JSVal -> Float -> IO ()

-- | Sets the height of a display object
setHeight :: (IsContainer obj) => obj -> Float -> IO ()
setHeight obj height = js_setHeight (toJSVal obj) height

-- *****************************************************************************
-- * Display Object Properties - Pivot
-- *****************************************************************************

foreign import javascript unsafe "$1.pivot.set($2, $3)"
    js_setPivot :: JSVal -> Float -> Float -> IO ()

-- | Sets the pivot point of a display object
--
-- @param obj The display object
-- @param x The x coordinate of the pivot
-- @param y The y coordinate of the pivot
setPivot :: (IsContainer obj) => obj -> Float -> Float -> IO ()
setPivot obj x y = js_setPivot (toJSVal obj) x y

foreign import javascript unsafe "$1.pivot.x = $2"
    js_setPivotX :: JSVal -> Float -> IO ()

-- | Sets the x pivot of a display object
setPivotX :: (IsContainer obj) => obj -> Float -> IO ()
setPivotX obj x = js_setPivotX (toJSVal obj) x

foreign import javascript unsafe "$1.pivot.y = $2"
    js_setPivotY :: JSVal -> Float -> IO ()

-- | Sets the y pivot of a display object
setPivotY :: (IsContainer obj) => obj -> Float -> IO ()
setPivotY obj y = js_setPivotY (toJSVal obj) y

-- *****************************************************************************
-- * Display Object Properties - Interactive
-- *****************************************************************************

foreign import javascript unsafe "$1.interactive = $2"
    js_setInteractive :: JSVal -> Bool -> IO ()

-- | Sets whether a display object is interactive (can receive events)
setInteractive :: (IsContainer obj) => obj -> Bool -> IO ()
setInteractive obj interactive = js_setInteractive (toJSVal obj) interactive

foreign import javascript unsafe "$1.buttonMode = $2"
    js_setButtonMode :: JSVal -> Bool -> IO ()

-- | Sets whether a display object acts as a button (changes cursor on hover)
setButtonMode :: (IsContainer obj) => obj -> Bool -> IO ()
setButtonMode obj buttonMode = js_setButtonMode (toJSVal obj) buttonMode

-- *****************************************************************************
-- * Display Object Properties - Masks
-- *****************************************************************************

foreign import javascript unsafe "$1.mask = $2"
    js_setMask :: JSVal -> JSVal -> IO ()

-- | Sets a mask for a display object
setMask :: (IsContainer obj, IsContainer maskObj) => obj -> maskObj -> IO ()
setMask obj maskObj = js_setMask (toJSVal obj) (toJSVal maskObj)

foreign import javascript unsafe "$1.mask"
    js_getMask :: JSVal -> IO JSVal

-- | Gets the mask of a display object
getMask :: (IsContainer obj) => obj -> IO Container
getMask obj = fromJSVal <$> js_getMask (toJSVal obj)

-- *****************************************************************************
-- * Display Object Properties - Parent
-- *****************************************************************************

foreign import javascript unsafe "$1.parent"
    js_getParent :: JSVal -> IO JSVal

-- | Gets the parent of a display object
getParent :: (IsContainer obj) => obj -> IO Container
getParent obj = fromJSVal <$> js_getParent (toJSVal obj)

-- *****************************************************************************
-- * Bounds
-- *****************************************************************************

foreign import javascript unsafe "$1.getBounds()"
    js_getBounds :: JSVal -> IO JSVal

-- | Gets the bounds of a display object in global coordinates
getBounds :: (IsContainer obj) => obj -> IO Rectangle
getBounds obj = Rectangle <$> js_getBounds (toJSVal obj)

foreign import javascript unsafe "$1.getLocalBounds()"
    js_getLocalBounds :: JSVal -> IO JSVal

-- | Gets the bounds of a display object in local coordinates
getLocalBounds :: (IsContainer obj) => obj -> IO Rectangle
getLocalBounds obj = Rectangle <$> js_getLocalBounds (toJSVal obj)
