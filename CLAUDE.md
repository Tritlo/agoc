# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

AGOC (A Game of Chance) is a dice game written in Haskell and compiled to WebAssembly using the GHC WASM backend. It uses PIXI.js for 2D rendering and Three.js for 3D dice rendering, with optional Electron packaging for desktop builds.

## Quick Agent Workflow

1) Enter the nix dev shell for the wasm toolchain and Playwright deps (first entry installs them):  
`nix develop`  (flake; comes with wasm32-wasi-{ghc,cabal}, node, python).  
2) Build the WASM + JS FFI and copy to `public/`:  
`./locally.sh` (preferred) or `./build-and-copy.sh` (Docker fallback).  
4) Run Playwright tests:  
`npx playwright test` (needs `public/agoc.wasm` present; nix shell installs @playwright/test and chromium).  
5) Package Electron (if needed):  
`./build-electron.sh` (uses Docker, writes to `dist/`).

## Build Commands (details)

### Local Build (Preferred)
```bash
nix develop            # Enter nix shell with wasm32-wasi toolchain
./locally.sh           # Build WASM, copy artifacts to public/
```

### Docker Build (Alternative)
```bash
./build-and-copy.sh    # Build WASM via Docker, copy artifacts to public/
```

### Electron Desktop Build
```bash
./build-electron.sh    # Build Electron packages for Linux/Windows (uses Docker)
```

### Dependencies and Environment
- Nix flake (`flake.nix`) provides ghc-wasm-meta, Node.js, and Python. The shellHook auto-installs `@playwright/test` and downloads Chromium on first entry.
- Without Nix: Use Docker. 
- Vendoring CDN deps for offline use: `./scripts/vendor-dependencies.sh` populates `public/vendor/…`.

## Architecture

### Package Structure
- **agoc** (root) - Main game executable and library
- **pixijs-ffi** (local package) - Reusable Haskell FFI bindings for PIXI.js v8

### Key Source Files
- `app/App.hs` - Main entry point, game logic, screen management, dice rolling mechanics
- `src/Lib.hs` - Application-specific FFI: sound effects, histogram plotting, dice spritesheet generation, Three.js dice rendering
- `src/Histogram.hs` - Histogram data structures
- `pixijs-ffi/src/Graphics/PixiJS.hs` - Re-exports all PIXI.js bindings
- `pixijs-ffi/src/Graphics/PixiJS/*.hs` - Individual PIXI.js modules

### Build Artifacts
- `public/agoc.wasm` - Compiled WebAssembly binary
- `public/ghc_wasm_jsffi.js` - Generated JavaScript FFI bindings (created by post-link.mjs)

### GHC WASM Specifics
- Uses reactor execution model (`-optl-mexec-model=reactor`)
- Main function exported via `foreign export javascript "main"`
- JavaScript FFI via `foreign import javascript` syntax
- Requires post-link.mjs to generate JS FFI glue code from WASM binary

### Testing
- End-to-end tests live in `test/` and run via Playwright (`playwright.config.ts` serves `public/` on :8000).
- Ensure `public/agoc.wasm` + `public/ghc_wasm_jsffi.js` exist before running `npx playwright test`.
- pixijs-ffi bindings are exercised through the same runtime; keep JS-visible names stable when changing FFI exports.

## pixijs-ffi Package

The `pixijs-ffi` package provides typed Haskell bindings for PIXI.js v8. It uses newtypes and type classes to model PIXI.js's inheritance hierarchy.

### Type Hierarchy
```
IsPixiObject (base)
  └─ IsEventEmitter
       └─ IsContainer (Container)
            ├─ IsSprite (Sprite)
            │    └─ IsAnimatedSprite (AnimatedSprite)
            ├─ IsGraphics (Graphics)
            ├─ IsText (Text)
            └─ IsBitmapText (BitmapText)
       └─ IsTexture (Texture)
            └─ IsRenderTexture (RenderTexture)
```

All types are newtypes wrapping `JSVal` with `deriving via` for type class instances:
```haskell
newtype Container = Container EventEmitter
    deriving (IsPixiObject, IsEventEmitter) via EventEmitter

newtype Sprite = Sprite Container
    deriving (IsPixiObject, IsEventEmitter, IsContainer) via Container
```

### Module Structure
- **Types** - Core newtypes and type classes (`Container`, `Sprite`, `Texture`, etc.)
- **Application** - `newApp`, `initApp`, `appendCanvas`, `getStage`, `getScreen`
- **Display** - Container/Sprite operations (`addChild`, `setX`, `setY`, `setScale`, `setAnchor`, etc.)
- **Graphics** - Vector drawing (`newGraphics`, `beginFill`, `drawRect`, `drawCircle`, etc.)
- **Text** - Text rendering (`newTextWithStyle`, `setText`)
- **Events** - Event handling (`on`, `setEventMode`, `setCursor`)
- **Ticker** - Animation loops (`newTicker`, `add`, `start`, `stop`, `getDeltaMS`)
- **Filters** - Visual effects (`newColorMatrixFilter`, `colorMatrixNegative`, `addFilter`)
- **Interop** - JS interop utilities (`jsFuncFromHs_`, `toJSString`, `toJSVal`, `fromJSVal`)

### Usage in App.hs
```haskell
import Graphics.PixiJS  -- Single import gets everything

main = do
    app <- newApp >>= flip initApp "white"
    appendCanvas app
    stage <- getStage app

    container <- newContainer
    addChild stage container

    sprite <- newSpriteFromTexture texture
    setX sprite 100.0
    setY sprite 200.0
    setAnchor sprite 0.5 0.5
    addChild container sprite

    -- Event handling
    on "pointerdown" sprite =<< jsFuncFromHs_ (\_ -> doSomething)
```

## Game Architecture (App.hs)

### Screen Management
The game uses a screen-based architecture with mutual recursion via IORefs:
```haskell
data Screen = StartScreen | OptionsScreen | GameScreen

-- IORefs hold screen transition functions
showStartScreenRef <- newIORef (return () :: IO ())
showGameScreenRef <- newIORef (return () :: IO ())

-- Indirection allows mutual recursion
let showStartScreen = readIORef showStartScreenRef >>= id
let showGameScreen = readIORef showGameScreenRef >>= id

-- Wire up the actual implementations
writeIORef showStartScreenRef $
    renderStartScreen app container width height showGameScreen showOptions
```

### GameState
```haskell
data GameState = GameState {
    gs_score              :: Int,           -- Current accumulated score
    gs_target             :: Int,           -- Target to reach
    gs_additiveDice       :: Int,           -- Number of additive dice
    gs_multiplicativeDice :: Int,           -- Number of multiplicative dice
    gs_persistentSprites  :: [JSVal],       -- Sprites from last roll
    gs_rollState          :: Maybe RollState,  -- Active roll tracking
    gs_round              :: Int            -- Current round number
}
```

### Dice Mechanics
- **Additive dice**: Contribute to X (sum of face values)
- **Multiplicative dice**: Contribute to Y (sum of face values, defaults to 1 if none)
- **Final score**: X × Y
- **Combo system**: 2+ matching dice of same type trigger bonus rolls (1 per combo)
- **Progression**: Reaching target lets player choose a new die type; target increases

### Menu System
Reusable menu abstraction:
```haskell
data MenuItem = MenuItem { menuItem_text :: String, menuItem_action :: IO () }
data Menu = Menu { menu_items :: [MenuItem], menu_x :: Float, ... }

renderMenu :: Menu -> IO Container
```

## Application-Specific FFI (src/Lib.hs)

### Sound Effects
```haskell
foreign import javascript unsafe "blip($1)"
    blipWithFreq :: Float -> IO ()

foreign import javascript unsafe "rollDice($1)"
    playDiceRollSound :: Int -> IO ()
```

### Dice Spritesheet Generation
Pre-generates 540 animation frames (6 faces × 3 variants × 30 frames) at startup:
```haskell
generateDiceSpritesheet :: IO JSVal  -- Returns spritesheet context

getAnimationFrames :: JSVal -> Int -> Int -> IO JSVal  -- face (1-6), variant (0-2)

newAnimatedSpriteFromJSArray :: JSVal -> IO JSVal  -- Create AnimatedSprite
```

### Three.js Dice Rendering (Legacy)
Slot-based texture atlas system for concurrent dice animations:
- `initDiceRenderer` - Creates persistent Three.js context
- `acquireDiceSlot` / `releaseDiceSlot` - Pool management
- `renderDiceFrame` - Renders single frame to slot
- `getDiceSlotTexture` - Gets PIXI texture for slot

### Other FFI
- `closeWindow` - Electron quit / browser close
- `localStorageGet` / `localStorageSet` - Browser storage
- `histogram_plot` - D3.js histogram rendering (for debugging)

## FFI Patterns and Conventions

- Add a `js_*` raw import that speaks `JSVal`, then wrap it in a typed function using the relevant `Is…` constraint to maintain the inheritance model.
- Use `unsafe` for synchronous property/method calls; use `safe` for anything that awaits promises, allocates callbacks, or otherwise yields (see `initApp`).
- Keep newtypes aligned with Pixi inheritance (`Container` -> `Sprite` -> `AnimatedSprite`, etc.) and prefer `safeCast`/`unsafeCast` from `Graphics.PixiJS.Types` when bridging types.
- When adding bindings that return arrays/objects, expose a typed wrapper that converts to/from JS arrays rather than leaking raw `JSVal` where possible.

### JavaScript FFI Syntax
```haskell
-- Inline unsafe (synchronous, no GC)
foreign import javascript unsafe "$1.property = $2"
    js_setProperty :: JSVal -> Int -> IO ()

-- Multiline safe (can allocate, allows callbacks)
foreign import javascript safe
  """
  (() => {
    // Complex JS code
    return result;
  })()
  """
    complexFunction :: Int -> IO JSVal
```

### Creating JS Callbacks
```haskell
callback <- jsFuncFromHs_ $ \event -> do
    -- Haskell code
    return ()
on "eventName" object callback
```

### Type Conversions
```haskell
toJSString :: String -> JSString
toJSVal :: IsPixiObject a => a -> JSVal
fromJSVal :: IsPixiObject a => JSVal -> a
```
