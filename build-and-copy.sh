#!/bin/bash
# Script to build the Docker image and copy the compiled wasm and jsffi files

set -e  # Exit on error

echo "Building Docker image..."
docker build -t agoc .

echo "Finding agoc.wasm in container..."
# Find the wasm file dynamically (handles different GHC versions)
WASM_PATH=$(docker run --rm agoc find dist-newstyle -name "agoc.wasm" -type f | head -n 1)

if [ -z "$WASM_PATH" ]; then
    echo "Error: Could not find agoc.wasm in container"
    exit 1
fi

echo "Found wasm file at: $WASM_PATH"
echo "Creating public directory if it doesn't exist..."
mkdir -p public

echo "Copying agoc.wasm from container..."
docker run -v $(pwd):/pwd --rm agoc cp "$WASM_PATH" /pwd/public/agoc.wasm

echo "Copying ghc_wasm_jsffi.js from container..."
docker run -v $(pwd):/pwd --rm agoc cp /app/ghc_wasm_jsffi.js /pwd/public

echo "Done! Files copied to public directory:"
ls -lh public/agoc.wasm public/ghc_wasm_jsffi.js 2>/dev/null || echo "Warning: Some files may not exist"
