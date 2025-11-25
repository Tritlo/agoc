#!/bin/bash
# Script to build locally and copy the compiled wasm and jsffi files

set -e  # Exit on error

echo "Building with wasm32-wasi-cabal..."
wasm32-wasi-cabal build exe:agoc

echo "Finding agoc.wasm in dist-newstyle..."
WASM_PATH=$(find dist-newstyle -name "agoc.wasm" -type f | head -n 1)

if [ -z "$WASM_PATH" ]; then
    echo "Error: Could not find agoc.wasm in dist-newstyle"
    exit 1
fi

echo "Found wasm file at: $WASM_PATH"

# The jsffi file is in the same directory as the wasm file
WASM_DIR=$(dirname "$WASM_PATH")
JSFFI_PATH="$WASM_DIR/ghc_wasm_jsffi.js"

echo "Generating JavaScript FFI bindings..."
"$(wasm32-wasi-ghc --print-libdir)/post-link.mjs" -i "$WASM_PATH" -o "$JSFFI_PATH"

echo "Creating public directory if it doesn't exist..."
mkdir -p public

echo "Copying agoc.wasm to public/..."
cp "$WASM_PATH" public/agoc.wasm

echo "Copying ghc_wasm_jsffi.js to public/..."
cp "$JSFFI_PATH" public/ghc_wasm_jsffi.js

echo "Done! Files copied to public directory:"
ls -lh public/agoc.wasm public/ghc_wasm_jsffi.js
