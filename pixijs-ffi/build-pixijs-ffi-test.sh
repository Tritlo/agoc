#!/bin/bash
# Script to build the pixijs-ffi test WASM module
# Run from the pixijs-ffi directory or repository root

set -e  # Exit on error

# Determine if we're in pixijs-ffi directory or parent
if [ -f "pixijs-ffi.cabal" ]; then
    # We're in pixijs-ffi directory
    ROOT_DIR=".."
    PIXIJS_DIR="pixijs-ffi"
else
    # We're in repository root
    ROOT_DIR="."
    PIXIJS_DIR="pixijs-ffi"
fi

echo "Updating package index..."
wasm32-wasi-cabal update

echo "Building pixijs-ffi-test with wasm32-wasi-cabal..."
cd "$ROOT_DIR"
wasm32-wasi-cabal build exe:pixijs-ffi-test

echo "Finding pixijs-ffi-test.wasm in dist-newstyle..."
WASM_PATH=$(find dist-newstyle -name "pixijs-ffi-test.wasm" -type f | head -n 1)

if [ -z "$WASM_PATH" ]; then
    echo "Error: Could not find pixijs-ffi-test.wasm in dist-newstyle"
    exit 1
fi

echo "Found wasm file at: $WASM_PATH"

# The jsffi file is in the same directory as the wasm file
WASM_DIR=$(dirname "$WASM_PATH")
JSFFI_PATH="$WASM_DIR/ghc_wasm_jsffi.js"

echo "Generating JavaScript FFI bindings..."
"$(wasm32-wasi-ghc --print-libdir)/post-link.mjs" -i "$WASM_PATH" -o "$JSFFI_PATH"

# Create public directory in pixijs-ffi for test assets
echo "Creating public directory if it doesn't exist..."
mkdir -p "$PIXIJS_DIR/public"

echo "Copying pixijs-ffi-test.wasm to $PIXIJS_DIR/public/..."
cp "$WASM_PATH" "$PIXIJS_DIR/public/pixijs-ffi-test.wasm"

echo "Copying ghc_wasm_jsffi.js to $PIXIJS_DIR/public/..."
cp "$JSFFI_PATH" "$PIXIJS_DIR/public/ghc_wasm_jsffi.js"

echo "Done! Test files copied to $PIXIJS_DIR/public/:"
ls -lh "$PIXIJS_DIR/public/pixijs-ffi-test.wasm" "$PIXIJS_DIR/public/ghc_wasm_jsffi.js"
