#!/bin/bash
# Script to vendor dependencies for pixijs-ffi tests
# Downloads PixiJS and @runno/wasi from CDN

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
VENDOR_DIR="$SCRIPT_DIR/public/vendor"

echo "Vendoring dependencies to $VENDOR_DIR..."

# Create vendor directories
mkdir -p "$VENDOR_DIR/pixi.js@8.0.0/dist"
mkdir -p "$VENDOR_DIR/@runno/wasi@0.7.0/dist"

# Download PixiJS v8.0.0
echo "Downloading PixiJS v8.0.0..."
curl -sL "https://cdn.jsdelivr.net/npm/pixi.js@8.0.0/dist/pixi.min.js" \
    -o "$VENDOR_DIR/pixi.js@8.0.0/dist/pixi.min.js"

# Download @runno/wasi v0.7.0
echo "Downloading @runno/wasi v0.7.0..."
curl -sL "https://cdn.jsdelivr.net/npm/@runno/wasi@0.7.0/dist/wasi.js" \
    -o "$VENDOR_DIR/@runno/wasi@0.7.0/dist/wasi.js"

# Verify downloads succeeded
if [ ! -s "$VENDOR_DIR/pixi.js@8.0.0/dist/pixi.min.js" ]; then
    echo "Error: Failed to download PixiJS"
    exit 1
fi

if [ ! -s "$VENDOR_DIR/@runno/wasi@0.7.0/dist/wasi.js" ]; then
    echo "Error: Failed to download @runno/wasi"
    exit 1
fi

echo "Done! Vendored files:"
ls -lh "$VENDOR_DIR/pixi.js@8.0.0/dist/pixi.min.js"
ls -lh "$VENDOR_DIR/@runno/wasi@0.7.0/dist/wasi.js"
