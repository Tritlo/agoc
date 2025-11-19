# pixijs-ffi

Comprehensive Haskell FFI bindings for [Pixi.js v8](https://pixijs.com/), a fast 2D WebGL/WebGPU rendering library.

## Overview

This library provides type-safe Haskell bindings for Pixi.js, designed for use with the GHC WebAssembly backend. It enables you to create high-performance 2D graphics applications in Haskell that run in the browser.

## Features

- **Application**: Create and manage Pixi.js applications
- **Display Objects**: Sprites, Containers, AnimatedSprites
- **Graphics**: Vector graphics with fills, strokes, and shapes
- **Text**: Rich text rendering with customizable styles
- **Assets**: Asynchronous asset loading
- **Textures**: Texture management and manipulation
- **Events**: Interactive event handling
- **Ticker**: Animation loop management
- **Math**: Points, Rectangles, and geometric utilities
- **Filters**: Visual effects and filters

## Usage

```haskell
import Graphics.PixiJS

main :: IO ()
main = do
    -- Create and initialize a Pixi application
    app <- newApp
    initApp app "#1099bb"
    appendCanvas app

    -- Load and display a sprite
    texture <- loadAsset "path/to/image.png"
    sprite <- newSprite texture
    addChild app sprite
```

## Requirements

- GHC 9.6+ with WebAssembly backend support
- Pixi.js v8 loaded in your HTML page

## License

MIT License - see LICENSE file for details
