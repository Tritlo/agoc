{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeFamilies #-}

-- | Core types for Pixi.js FFI bindings
--
-- This module defines the fundamental types used throughout the Pixi.js FFI.
-- All Pixi.js objects are represented as newtypes around 'JSVal' for type safety.
-- Type classes are used to model the inheritance hierarchy.
module Graphics.PixiJS.Types
    ( -- * Core Types
      JSVal
    , JSString
    , JSFunction
      -- * Type Classes
    , IsPixiObject(..)
    , IsEventEmitter
    , IsContainer
    , IsSprite
    , IsAnimatedSprite
    , IsGraphics
    , IsText
    , IsBitmapText
    , IsApplication
    , IsTexture
    , IsRenderTexture
      -- * Application Types
    , Application(..)
      -- * Display Types
    , Container(..)
    , Sprite(..)
    , AnimatedSprite(..)
      -- * Graphics Types
    , Graphics(..)
      -- * Text Types
    , Text(..)
    , BitmapText(..)
      -- * Texture Types
    , Texture(..)
    , RenderTexture(..)
      -- * Math Types
    , Point(..)
    , Rectangle(..)
    , Circle(..)
    , Ellipse(..)
    , Polygon(..)
    , Matrix(..)
    , ObservablePoint(..)
      -- * Event Types
    , EventEmitter(..)
    , FederatedEvent(..)
      -- * Ticker Types
    , Ticker(..)
    , TickerCallback
      -- * Filter Types
    , Filter(..)
    , BlurFilter(..)
    , ColorMatrixFilter(..)
      -- * Other Types
    , Renderer(..)
    , Canvas(..)
      -- * Casting
    , safeCast
    , unsafeCast
    ) where

import GHC.Wasm.Prim (JSVal, JSString)
import Data.Coerce (coerce)

-- | A JavaScript function represented as a JSVal
type JSFunction = JSVal

-- *****************************************************************************
-- * Type Classes
-- *****************************************************************************

-- | Base class for all PixiJS objects that wrap a JSVal
class IsPixiObject a where
    toJSVal :: a -> JSVal
    fromJSVal :: JSVal -> a

-- | Class for objects that can emit events
class IsPixiObject a => IsEventEmitter a

-- | Class for objects that are Containers (can hold children)
class IsEventEmitter a => IsContainer a

-- | Class for objects that are Sprites
class IsContainer a => IsSprite a

-- | Class for objects that are AnimatedSprites
class IsSprite a => IsAnimatedSprite a

-- | Class for objects that are Graphics
class IsContainer a => IsGraphics a

-- | Class for objects that are Text
class IsContainer a => IsText a

-- | Class for objects that are BitmapText
class IsContainer a => IsBitmapText a

-- | Class for objects that are Applications
class IsApplication a

-- | Class for objects that are Textures
class IsEventEmitter a => IsTexture a

-- | Class for objects that are RenderTextures
class IsTexture a => IsRenderTexture a

-- *****************************************************************************
-- * Event Types (Base for Display Objects)
-- *****************************************************************************

-- | An event emitter
newtype EventEmitter = EventEmitter JSVal


instance IsPixiObject EventEmitter where
    toJSVal = coerce
    fromJSVal = coerce

instance IsEventEmitter EventEmitter

-- | A federated event
newtype FederatedEvent = FederatedEvent JSVal


instance IsPixiObject FederatedEvent where
    toJSVal = coerce
    fromJSVal = coerce

-- *****************************************************************************
-- * Display Types
-- *****************************************************************************

-- | A Pixi.js Container - can hold multiple display objects
newtype Container = Container EventEmitter

    deriving (IsPixiObject, IsEventEmitter) via EventEmitter

instance IsContainer Container

-- | A Pixi.js Sprite - displays a texture
newtype Sprite = Sprite Container

    deriving (IsPixiObject, IsEventEmitter, IsContainer) via Container

instance IsSprite Sprite

-- | A Pixi.js AnimatedSprite - plays animated sequences
newtype AnimatedSprite = AnimatedSprite Sprite

    deriving (IsPixiObject, IsEventEmitter, IsContainer, IsSprite) via Sprite

instance IsAnimatedSprite AnimatedSprite

-- *****************************************************************************
-- * Graphics Types
-- *****************************************************************************

-- | A Pixi.js Graphics object for vector drawing
newtype Graphics = Graphics Container

    deriving (IsPixiObject, IsEventEmitter, IsContainer) via Container

instance IsGraphics Graphics

-- *****************************************************************************
-- * Text Types
-- *****************************************************************************

-- | A Pixi.js Text object for rendering text
newtype Text = Text Container

    deriving (IsPixiObject, IsEventEmitter, IsContainer) via Container

instance IsText Text

-- | A Pixi.js BitmapText object for bitmap font rendering
newtype BitmapText = BitmapText Container

    deriving (IsPixiObject, IsEventEmitter, IsContainer) via Container

instance IsBitmapText BitmapText

-- *****************************************************************************
-- * Application Types
-- *****************************************************************************

-- | A Pixi.js Application instance
newtype Application = Application JSVal


instance IsPixiObject Application where
    toJSVal = coerce
    fromJSVal = coerce

instance IsApplication Application

-- *****************************************************************************
-- * Texture Types
-- *****************************************************************************

-- | A Pixi.js Texture
newtype Texture = Texture EventEmitter

    deriving (IsPixiObject, IsEventEmitter) via EventEmitter

instance IsTexture Texture

-- | A Pixi.js RenderTexture - a texture that can be rendered to
newtype RenderTexture = RenderTexture Texture

    deriving (IsPixiObject, IsEventEmitter, IsTexture) via Texture

instance IsRenderTexture RenderTexture

-- *****************************************************************************
-- * Math Types
-- *****************************************************************************

-- | A 2D point
newtype Point = Point JSVal


instance IsPixiObject Point where
    toJSVal = coerce
    fromJSVal = coerce

-- | A rectangle
newtype Rectangle = Rectangle JSVal


instance IsPixiObject Rectangle where
    toJSVal = coerce
    fromJSVal = coerce

-- | A circle
newtype Circle = Circle JSVal


instance IsPixiObject Circle where
    toJSVal = coerce
    fromJSVal = coerce

-- | An ellipse
newtype Ellipse = Ellipse JSVal


instance IsPixiObject Ellipse where
    toJSVal = coerce
    fromJSVal = coerce

-- | A polygon
newtype Polygon = Polygon JSVal


instance IsPixiObject Polygon where
    toJSVal = coerce
    fromJSVal = coerce

-- | A 2D transformation matrix
newtype Matrix = Matrix JSVal


instance IsPixiObject Matrix where
    toJSVal = coerce
    fromJSVal = coerce

-- | An observable point that emits events when changed
newtype ObservablePoint = ObservablePoint JSVal


instance IsPixiObject ObservablePoint where
    toJSVal = coerce
    fromJSVal = coerce

-- *****************************************************************************
-- * Ticker Types
-- *****************************************************************************

-- | A Pixi.js Ticker for animation loops
newtype Ticker = Ticker JSVal


instance IsPixiObject Ticker where
    toJSVal = coerce
    fromJSVal = coerce

-- | A callback function for ticker updates
type TickerCallback = JSFunction

-- *****************************************************************************
-- * Filter Types
-- *****************************************************************************

-- | A Pixi.js Filter
newtype Filter = Filter JSVal


instance IsPixiObject Filter where
    toJSVal = coerce
    fromJSVal = coerce

-- | A blur filter
newtype BlurFilter = BlurFilter Filter

    deriving (IsPixiObject) via Filter

-- | A color matrix filter
newtype ColorMatrixFilter = ColorMatrixFilter Filter

    deriving (IsPixiObject) via Filter

-- *****************************************************************************
-- * Other Types
-- *****************************************************************************

-- | A Pixi.js Renderer
newtype Renderer = Renderer JSVal


instance IsPixiObject Renderer where
    toJSVal = coerce
    fromJSVal = coerce

-- | A HTML Canvas element
newtype Canvas = Canvas JSVal


instance IsPixiObject Canvas where
    toJSVal = coerce
    fromJSVal = coerce

-- *****************************************************************************
-- * Casting
-- *****************************************************************************

-- | Safe cast between types that share a type class relationship
-- This is a no-op at runtime
safeCast :: (IsPixiObject a, IsPixiObject b) => a -> b
safeCast = fromJSVal . toJSVal

-- | Unsafe cast between any two types wrapping JSVal
-- Use with caution!
unsafeCast :: (IsPixiObject a, IsPixiObject b) => a -> b
unsafeCast = fromJSVal . toJSVal
