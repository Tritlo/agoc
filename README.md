# GHC WASM PIXI Sample

A minimal PIXI.js demo written in Haskell and compiled to WebAssembly using the
[GHC WASM backend](https://gitlab.haskell.org/haskell-wasm/ghc-wasm-meta). It renders a score
label and a clickable "Sample" button; each click updates the score by sampling from
a normal distribution. The project also includes FFI helpers for PIXI.js and utility bindings.

## Features

- PIXI.js rendering from Haskell via JavaScript FFI
- Clickable button with event handler entirely in Haskell
- Score updated from a normal distribution sample
- Optional vendored JS dependencies for offline builds
- Electron desktop builds (Linux AppImage/Deb, Windows NSIS/Portable)

## Run in the browser

1. Build the WebAssembly and copy artifacts into `public/`:
   ```bash
   ./build-and-copy.sh
   ```

2. (Optional) Vendor the CDN dependencies for offline use:
   ```bash
   ./scripts/vendor-dependencies.sh
   ```

3. Start a local web server (any static server works):
   ```bash
   python -m http.server 8001
   ```

4. Open in your browser:
   ```
   http://localhost:8001
   ```

## Electron builds

To build standalone Electron applications:

```bash
./build-electron.sh
```

This will:
1. Build the WASM files (if needed)
2. Vendor CDN dependencies
3. Build Electron apps for Linux and Windows using Docker

Built packages will be available in the `dist/` directory:
- **Linux**: AppImage and Debian package (.deb)
- **Windows**: NSIS installer and portable executable

## Interaction

- Click the "Sample" button to update the score shown on screen.

## Technical details

- Built with the GHC WASM backend targeting `wasm32-wasi`
- PIXI.js for 2D rendering
- JavaScript FFI via `foreign import javascript`
- Electron for desktop packaging
- Optional vendored JavaScript dependencies for offline operation

## Project structure

- `app/App.hs` — Entry point and demo logic
- `src/Lib.hs` — PIXI.js FFI bindings and JS interop utilities
- `public/` — `index.html`, `agoc.wasm`, `ghc_wasm_jsffi.js`
- `electron/` — Electron main process and preload scripts
- `scripts/` — Helper scripts (e.g. `vendor-dependencies.sh`)
- `Dockerfile` — Build environment for WASM compilation
- `Dockerfile.electron` — Build environment for Electron packaging
