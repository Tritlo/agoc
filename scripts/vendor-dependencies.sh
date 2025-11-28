#!/bin/bash
# Script to download and vendor CDN dependencies using curl

set -e  # Exit on error

echo "Vendoring CDN dependencies..."

# Create vendor directory structure
mkdir -p public/vendor/pixi.js@8.0.0/dist
mkdir -p public/vendor/@runno/wasi@0.7.0/dist
mkdir -p public/vendor/plot/
mkdir -p public/vendor/d3/
mkdir -p public/vendor/three@0.181.0/build

# Download PIXI.js
echo "Downloading PIXI.js..."
curl -f -L https://cdn.jsdelivr.net/npm/pixi.js@8.0.0/dist/pixi.min.js -o public/vendor/pixi.js@8.0.0/dist/pixi.min.js

# Download @runno/wasi
echo "Downloading @runno/wasi..."
curl -f -L https://cdn.jsdelivr.net/npm/@runno/wasi@0.7.0/dist/wasi.js -o public/vendor/@runno/wasi@0.7.0/dist/wasi.js

# Download Plot
curl -f -L https://observablehq.com/plot/plot.min.js -o public/vendor/plot/plot.min.js

# Download D3
curl -f -L https://observablehq.com/plot/d3.min.js -o public/vendor/d3/d3.min.js

# Download Three.js (ES module builds)
echo "Downloading Three.js..."
curl -f -L https://cdn.jsdelivr.net/npm/three@0.181.0/build/three.module.min.js -o public/vendor/three@0.181.0/build/three.module.min.js
curl -f -L https://cdn.jsdelivr.net/npm/three@0.181.0/build/three.core.min.js -o public/vendor/three@0.181.0/build/three.core.min.js
curl -f -L https://cdn.jsdelivr.net/npm/three@0.181.0/build/three.webgpu.min.js -o public/vendor/three@0.181.0/build/three.webgpu.min.js

# Verify downloads
if [ ! -f "public/vendor/pixi.js@8.0.0/dist/pixi.min.js" ]; then
    echo "Error: Failed to download PIXI.js"
    exit 1
fi

if [ ! -f "public/vendor/@runno/wasi@0.7.0/dist/wasi.js" ]; then
    echo "Error: Failed to download @runno/wasi"
    exit 1
fi

if [ ! -f "public/vendor/plot/plot.min.js" ]; then
    echo "Error: Failed to download Plot"
    exit 1
fi

if [ ! -f "public/vendor/d3/d3.min.js" ]; then
    echo "Error: Failed to download D3"
    exit 1
fi

if [ ! -f "public/vendor/three@0.181.0/build/three.module.min.js" ]; then
    echo "Error: Failed to download Three.js module"
    exit 1
fi

if [ ! -f "public/vendor/three@0.181.0/build/three.core.min.js" ]; then
    echo "Error: Failed to download Three.js core"
    exit 1
fi

if [ ! -f "public/vendor/three@0.181.0/build/three.webgpu.min.js" ]; then
    echo "Error: Failed to download Three.js webgpu"
    exit 1
fi

echo "✓ Successfully vendored all dependencies"
ls -lh public/vendor/pixi.js@8.0.0/dist/pixi.min.js public/vendor/@runno/wasi@0.7.0/dist/wasi.js
ls -lh public/vendor/plot/plot.min.js public/vendor/d3/d3.min.js
ls -lh public/vendor/three@0.181.0/build/