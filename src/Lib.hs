{-# LANGUAGE MultilineStrings #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Application-specific FFI bindings
--
-- This module provides custom FFI bindings specific to this application,
-- such as histogram plotting and sound effects.
module Lib
    ( -- * Sound Effects
      blipWithFreq
      -- * Histogram Plotting
    , histogram_plot
    , HistogramOptions(..)
    , defaultHistogramOptions
      -- * Gamepad
    , getFirstGamepad
    , getGamepadAxis
    , isGamepadButtonPressed
      -- * Dice Rendering
    , initDiceRenderer
    , createD6Materials
    , renderDiceFrame
    , acquireDiceSlot
    , releaseDiceSlot
    , getDiceSlotTexture
      -- * Window Management
    , closeWindow
    ) where

import GHC.Wasm.Prim
import Graphics.PixiJS.Interop () -- For IsString JSString instance

-- *****************************************************************************
-- * Sound Effects
-- *****************************************************************************

-- | Plays a blip sound effect with a custom frequency.
--
-- Uses default duration and volume settings.
--
-- @param frequency Frequency in Hz
foreign import javascript unsafe "blip($1)"
    blipWithFreq :: Float -> IO ()

-- *****************************************************************************
-- * Histogram Plotting
-- *****************************************************************************

foreign import javascript safe
 """
 const offscreen = new OffscreenCanvas($1, $2);
 const context = offscreen.getContext("2d");

 const data = $18;

 // Calculate min/max from data for better scaling
 let minX = Infinity, maxX = -Infinity;
 let maxY = 0;
 for (const [count, cutoff] of data) {
   if (cutoff < minX) minX = cutoff;
   if (cutoff > maxX) maxX = cutoff;
   if (count > maxY) maxY = count;
 }

 // Add padding to domains using configurable factors
 const xPadding = (maxX - minX) * $11;
 const yPadding = maxY * $12;
 const xMin = Math.max(0, minX - xPadding);
 const xMax = maxX + xPadding;
 const yMin = 0;
 const yMax = maxY + yPadding;

 // Use configurable margins
 const marginLeft = $7;
 const marginRight = $8;
 const marginTop = $9;
 const marginBottom = $10;
 const plotWidth = $1 - marginLeft - marginRight;
 const plotHeight = $2 - marginTop - marginBottom;

 // d3 scales: x (cutoff), y (count)
 const xScale = d3.scaleLinear().domain([xMin, xMax]).range([marginLeft, $1 - marginRight]);
 const yScale = d3.scaleLinear().domain([yMin, yMax]).range([$2 - marginBottom, marginTop]);

 // Fill background with configurable color
 context.fillStyle = $4;
 context.fillRect(0, 0, $1, $2);

 // Axes styling with configurable color
 context.strokeStyle = $5;
 context.lineWidth = 1.5;
 context.beginPath();
 // Y axis
 context.moveTo(marginLeft, marginTop);
 context.lineTo(marginLeft, $2 - marginBottom);
 // X axis
 context.lineTo($1 - marginRight, $2 - marginBottom);
 context.stroke();

 // Draw y axis ticks and labels
 context.fillStyle = $5;
 context.font = "12px sans-serif";
 context.textAlign = "right";
 context.textBaseline = "middle";
 const numYTicks = $14;
 for (let i = 0; i <= numYTicks; ++i) {
   const yValue = yMin + (i * (yMax - yMin) / numYTicks);
   const y = yScale(yValue);
   context.beginPath();
   context.moveTo(marginLeft - 4, y);
   context.lineTo(marginLeft, y);
   context.stroke();
   context.fillText(Math.round(yValue).toString(), marginLeft - 8, y);
 }

 // Draw x axis ticks and labels
 context.textAlign = "center";
 context.textBaseline = "top";
 const numXTicks = $13;
 for (let i = 0; i <= numXTicks; ++i) {
   const xValue = xMin + (i * (xMax - xMin) / numXTicks);
   const x = xScale(xValue);
   context.beginPath();
   context.moveTo(x, $2 - marginBottom);
   context.lineTo(x, $2 - marginBottom + 4);
   context.stroke();
   context.fillText(Math.round(xValue).toString(), x, $2 - marginBottom + 8);
 }

 // Calculate bin width from first two bins (if available)
 let binWidth = plotWidth / data.length;
 if (data.length > 1) {
   binWidth = xScale(data[1][1]) - xScale(data[0][1]);
 }

 // Draw histogram bars with configurable styling
 context.fillStyle = $3;
 context.strokeStyle = $6;
 context.lineWidth = 0.5;
 const barSpacingFactor = $17;
 for (const [count, cutoff] of data) {
   if (count > 0) {
     const x = xScale(cutoff);
     const y = yScale(count);
     const height = ($2 - marginBottom) - y;
     // Draw bar with configurable spacing
     const barWidth = Math.max(1, binWidth * barSpacingFactor);
     const barX = x - barWidth / 2;
     context.fillRect(barX, y, barWidth, height);
     // Add subtle border for definition
     if (barWidth > 2) {
       context.strokeRect(barX, y, barWidth, height);
     }
   }
 }
 return PIXI.Texture.from(offscreen);
 """
 histogram_plot_ffi :: Int -- ^ width $1
                -> Int -- ^ height $2
                -> JSString -- ^ fill color $3
                -> JSString -- ^ background color $4
                -> JSString -- ^ axis color $5
                -> JSString -- ^ bar border color $6
                -> Int -- ^ margin left $7
                -> Int -- ^ margin right $8
                -> Int -- ^ margin top $9
                -> Int -- ^ margin bottom $10
                -> Double -- ^ x padding factor $11
                -> Double -- ^ y padding factor $12
                -> Int -- ^ num x ticks $13
                -> Int -- ^ num y ticks $14
                -> JSString -- ^ x axis label $15
                -> JSString -- ^ y axis label $16
                -> Double -- ^ bar spacing factor $17
                -> JSVal -- ^ data $18
                -> IO JSVal

-- | Options for customizing histogram plot appearance
data HistogramOptions = HistogramOptions {
    ho_width :: Int,              -- ^ Plot width in pixels
    ho_height :: Int,              -- ^ Plot height in pixels
    ho_fillColor :: JSString,      -- ^ Bar fill color (e.g., "black", "#FF0000")
    ho_backgroundColor :: JSString, -- ^ Background color (e.g., "white", "#FFFFFF")
    ho_axisColor :: JSString,      -- ^ Axis and tick color (default: "#333")
    ho_barBorderColor :: JSString, -- ^ Bar border color (default: "#fff")
    ho_marginLeft :: Int,          -- ^ Left margin in pixels (default: 50)
    ho_marginRight :: Int,         -- ^ Right margin in pixels (default: 20)
    ho_marginTop :: Int,           -- ^ Top margin in pixels (default: 20)
    ho_marginBottom :: Int,        -- ^ Bottom margin in pixels (default: 40)
    ho_xPaddingFactor :: Double,   -- ^ X-axis padding as fraction of range (default: 0.05)
    ho_yPaddingFactor :: Double,   -- ^ Y-axis padding as fraction of max (default: 0.05)
    ho_numXTicks :: Int,           -- ^ Number of x-axis ticks (default: 8)
    ho_numYTicks :: Int,           -- ^ Number of y-axis ticks (default: 6)
    ho_xAxisLabel :: JSString,     -- ^ X-axis label (default: "Cutoff")
    ho_yAxisLabel :: JSString,     -- ^ Y-axis label (default: "Count")
    ho_barSpacingFactor :: Double  -- ^ Bar width as fraction of bin width (default: 0.9)
}

-- | Default histogram options
defaultHistogramOptions :: HistogramOptions
defaultHistogramOptions = HistogramOptions {
    ho_width = 600,
    ho_height = 400,
    ho_fillColor = "black",
    ho_backgroundColor = "white",
    ho_axisColor = "#333",
    ho_barBorderColor = "#fff",
    ho_marginLeft = 50,
    ho_marginRight = 20,
    ho_marginTop = 20,
    ho_marginBottom = 40,
    ho_xPaddingFactor = 0.05,
    ho_yPaddingFactor = 0.05,
    ho_numXTicks = 8,
    ho_numYTicks = 6,
    ho_xAxisLabel = "Cutoff",
    ho_yAxisLabel = "Count",
    ho_barSpacingFactor = 0.9
}

-- | Plot a histogram with the given options
histogram_plot :: JSVal -> HistogramOptions -> IO JSVal
histogram_plot histogramData opts = do
    histogram_plot_ffi
        (ho_width opts)
        (ho_height opts)
        (ho_fillColor opts)
        (ho_backgroundColor opts)
        (ho_axisColor opts)
        (ho_barBorderColor opts)
        (ho_marginLeft opts)
        (ho_marginRight opts)
        (ho_marginTop opts)
        (ho_marginBottom opts)
        (ho_xPaddingFactor opts)
        (ho_yPaddingFactor opts)
        (ho_numXTicks opts)
        (ho_numYTicks opts)
        (ho_xAxisLabel opts)
        (ho_yAxisLabel opts)
        (ho_barSpacingFactor opts)
        histogramData

-- *****************************************************************************
-- * Gamepad Functions (Browser Gamepad API)
-- *****************************************************************************

-- | Gets the first connected gamepad, or null if none connected.
-- Uses browser Gamepad API. Filters for standard mapping gamepads.
foreign import javascript safe
  """
  (() => {
    if (navigator.getGamepads) {
      const pads = navigator.getGamepads();
      for (let i = 0; i < pads.length; i++) {
        if (pads[i] !== null && pads[i].mapping === 'standard') {
          return pads[i];
        }
      }
      // If no standard-mapped pad, return the first non-null pad
      for (let i = 0; i < pads.length; i++) {
        if (pads[i] !== null) {
          return pads[i];
        }
      }
    }
    return null;
  })()
  """
    getFirstGamepad :: IO JSVal

-- | Gets a gamepad axis value using browser Gamepad API.
-- @param gamepadHandle The gamepad object from browser Gamepad API
-- @param axisIndex The axis index (0 = left stick X, 1 = left stick Y, etc.)
-- @return The axis value, or 0.0 if gamepad is null or axis doesn't exist
foreign import javascript unsafe
  """
  (() => {
    const handle = $1;
    const axisIndex = $2;
    if (handle && handle.axes && handle.axes[axisIndex] !== undefined) {
      return handle.axes[axisIndex];
    }
    return 0.0;
  })()
  """
    getGamepadAxis :: JSVal -> Int -> IO Float

-- | Checks if a gamepad button is pressed using browser Gamepad API.
-- @param gamepadHandle The gamepad object from browser Gamepad API
-- @param buttonIndex The button index
-- @return True if button is pressed, False otherwise
foreign import javascript unsafe "($1 && $1.buttons && $1.buttons[$2]) ? $1.buttons[$2].pressed : false"
    isGamepadButtonPressed :: JSVal -> Int -> IO Bool

-- *****************************************************************************
-- * Dice Rendering (Three.js)
-- *****************************************************************************

-- | Initialize a persistent dice renderer context.
-- Returns a renderer handle that should be reused for all dice rendering.
-- Parameters:
--   $1: canvas size (width and height, square)
foreign import javascript safe
  """
  (() => {
    const size = $1;
    const slotsPerRow = 16;   // 2D grid: 16 slots per row
    const numRows = 8;        // 8 rows = 128 slots per canvas
    const slotsPerCanvas = slotsPerRow * numRows;
    const maxCanvases = 8;    // Up to 8 canvases = 1024 total slots
    const slotPadding = 10;   // 10px padding between slots
    const slotStride = size + slotPadding;
    const frameExtra = 5;     // PixiJS frame includes 5px of padding on each side

    const canvasWidth = slotStride * slotsPerRow;
    const canvasHeight = slotStride * numRows;

    // Helper to create a new canvas with renderer and texture
    const createCanvasContext = () => {
      const canvas = new OffscreenCanvas(canvasWidth, canvasHeight);
      const renderer = new THREE.WebGLRenderer({
        canvas: canvas,
        antialias: true,
        alpha: true
      });
      renderer.setSize(canvasWidth, canvasHeight, false);
      renderer.setClearColor(0x000000, 0);

      // Clear to transparent
      renderer.setScissorTest(false);
      renderer.clear();

      const texture = PIXI.Texture.from(canvas);
      texture.source.scaleMode = 'nearest';

      return { canvas, renderer, texture };
    };

    // Create first canvas
    const canvasContexts = [createCanvasContext()];
    const canvas = canvasContexts[0].canvas;
    const renderer = canvasContexts[0].renderer;

    // Create scene (persistent)
    const scene = new THREE.Scene();

    // Create camera (persistent)
    const camera = new THREE.PerspectiveCamera(50, 1, 0.1, 1000);
    camera.position.z = 3;

    // Add lighting (persistent)
    const ambientLight = new THREE.AmbientLight(0x404040, 2);
    scene.add(ambientLight);

    const directionalLight = new THREE.DirectionalLight(0xffffff, 1);
    directionalLight.position.set(1, 1, 1);
    scene.add(directionalLight);

    const directionalLight2 = new THREE.DirectionalLight(0xffffff, 0.5);
    directionalLight2.position.set(-1, -1, 1);
    scene.add(directionalLight2);

    // Helper function to create a texture with a number for D6 faces
    const createD6FaceTexture = (number, bgColor, textColor) => {
      const faceCanvas = new OffscreenCanvas(128, 128);
      const ctx = faceCanvas.getContext('2d');

      // Background
      ctx.fillStyle = bgColor;
      ctx.fillRect(0, 0, 128, 128);

      // Number
      ctx.fillStyle = textColor;
      ctx.font = 'bold 72px Arial';
      ctx.textAlign = 'center';
      ctx.textBaseline = 'middle';
      ctx.fillText(number.toString(), 64, 68);

      return new THREE.CanvasTexture(faceCanvas);
    };

    // Create D6 face materials (numbers 1-6)
    // Face order for BoxGeometry: +X, -X, +Y, -Y, +Z, -Z
    // Standard dice: opposite faces sum to 7 (1/6, 2/5, 3/4)
    const d6FaceNumbers = [3, 4, 1, 6, 2, 5]; // Adjusted for correct opposite faces
    const d6Materials = d6FaceNumbers.map(num =>
      new THREE.MeshPhongMaterial({
        map: createD6FaceTexture(num, '#ffffff', '#333333'),
        shininess: 30
      })
    );

    // Create initial dice mesh with D6 materials
    let currentGeometry = new THREE.BoxGeometry(1, 1, 1);
    const dice = new THREE.Mesh(currentGeometry, d6Materials);
    scene.add(dice);

    // Add dark edge lines for 3D definition
    const edgeGeometry = new THREE.EdgesGeometry(currentGeometry);
    const edgeMaterial = new THREE.LineBasicMaterial({ color: 0x333333 });
    let edgeLines = new THREE.LineSegments(edgeGeometry, edgeMaterial);
    dice.add(edgeLines);

    // Store current dice type to avoid recreating geometry unnecessarily
    let currentDiceType = 6;

    // Slot pool management for concurrent dice animations (2D grid, multi-canvas)
    const slotSize = size;
    const stride = slotStride;
    const paddingOffset = slotPadding / 2;  // 5px offset to center dice in cell

    // Free slots: array of {canvasIndex, localSlot} objects
    // Start with all slots from first canvas
    const freeSlots = Array.from({length: slotsPerCanvas}, (_, i) => ({canvasIndex: 0, localSlot: i}));

    // Return renderer context object
    return {
      canvasContexts,
      createCanvasContext,
      scene,
      camera,
      dice,
      d6Materials,
      currentGeometry,
      currentDiceType,
      edgeLines,
      slotSize,
      stride,
      slotsPerRow,
      slotsPerCanvas,
      maxCanvases,
      numRows,
      freeSlots,
      paddingOffset,
      frameExtra
    };
  })()
  """
    initDiceRenderer :: Int -> IO JSVal

-- | Create D6 materials with the given color.
-- Returns a JSVal containing the array of 6 materials.
-- Call this once per roll, not per frame.
foreign import javascript safe
  """
  (() => {
    const color = parseInt($1);
    const hexColor = color.toString(16).padStart(6, '0');
    const bgColor = '#' + hexColor;
    const d6FaceNumbers = [3, 4, 1, 6, 2, 5];
    return d6FaceNumbers.map(num => {
      const faceCanvas = new OffscreenCanvas(128, 128);
      const faceCtx = faceCanvas.getContext('2d');
      faceCtx.fillStyle = bgColor;
      faceCtx.fillRect(0, 0, 128, 128);
      faceCtx.font = 'bold 72px Arial';
      faceCtx.textAlign = 'center';
      faceCtx.textBaseline = 'middle';
      faceCtx.strokeStyle = '#000000';
      faceCtx.lineWidth = 3;
      faceCtx.strokeText(num.toString(), 64, 68);
      faceCtx.fillStyle = '#ffffff';
      faceCtx.fillText(num.toString(), 64, 68);
      return new THREE.MeshBasicMaterial({
        map: new THREE.CanvasTexture(faceCanvas)
      });
    });
  })()
  """
    createD6Materials :: JSString  -- ^ Dice color (e.g., "0xffffff")
                      -> IO JSVal  -- ^ Returns array of 6 materials

-- | Render a dice frame to a specific slot using an existing renderer context.
-- Parameters:
--   $1: renderer context (from initDiceRenderer)
--   $2: slot index (from acquireDiceSlot)
--   $3: rotation X (radians)
--   $4: rotation Y (radians)
--   $5: rotation Z (radians)
--   $6: dice color (hex string like "0xffffff") - used for non-D6 dice
--   $7: dice type (4=D4, 6=D6, 8=D8, 12=D12, 20=D20)
--   $8: pre-created D6 materials (from createD6Materials), or null for non-D6 dice
foreign import javascript safe
  """
  (() => {
    const ctx = $1;
    const slotIndex = $2;
    const rotX = $3;
    const rotY = $4;
    const rotZ = $5;
    const color = parseInt($6);
    const diceType = $7;
    const d6Materials = $8;  // Pre-created D6 materials (or null for non-D6)

    // Update dice type if changed
    if (ctx.currentDiceType !== diceType) {
      // Remove old geometry
      ctx.currentGeometry.dispose();

      // Create new geometry based on dice type
      let geometry;
      let material;
      switch (diceType) {
        case 4:
          geometry = new THREE.TetrahedronGeometry(1);
          material = new THREE.MeshPhongMaterial({ color: color, flatShading: true, shininess: 30 });
          break;
        case 8:
          geometry = new THREE.OctahedronGeometry(1);
          material = new THREE.MeshPhongMaterial({ color: color, flatShading: true, shininess: 30 });
          break;
        case 12:
          geometry = new THREE.DodecahedronGeometry(1);
          material = new THREE.MeshPhongMaterial({ color: color, flatShading: true, shininess: 30 });
          break;
        case 20:
          geometry = new THREE.IcosahedronGeometry(1);
          material = new THREE.MeshPhongMaterial({ color: color, flatShading: true, shininess: 30 });
          break;
        case 6:
        default:
          geometry = new THREE.BoxGeometry(1, 1, 1);
          // Use pre-created materials for D6
          material = d6Materials;
          break;
      }

      ctx.dice.geometry = geometry;
      ctx.dice.material = material;
      ctx.currentGeometry = geometry;
      ctx.currentDiceType = diceType;

      // Update edge lines for new geometry
      ctx.dice.remove(ctx.edgeLines);
      ctx.edgeLines.geometry.dispose();
      const newEdgeGeometry = new THREE.EdgesGeometry(geometry);
      ctx.edgeLines = new THREE.LineSegments(newEdgeGeometry, new THREE.LineBasicMaterial({ color: 0x333333 }));
      ctx.dice.add(ctx.edgeLines);
    }

    // For D6, use the pre-created materials (no per-frame allocation!)
    if (diceType === 6 && d6Materials) {
      ctx.dice.material = d6Materials;
    }

    // Update rotation
    ctx.dice.rotation.x = rotX;
    ctx.dice.rotation.y = rotY;
    ctx.dice.rotation.z = rotZ;

    // Decode global slot ID into canvas index and local slot
    const canvasIndex = Math.floor(slotIndex / ctx.slotsPerCanvas);
    const localSlot = slotIndex % ctx.slotsPerCanvas;
    const canvasCtx = ctx.canvasContexts[canvasIndex];

    // Calculate 2D position in the grid
    const col = localSlot % ctx.slotsPerRow;
    const row = Math.floor(localSlot / ctx.slotsPerRow);
    // WebGL Y is bottom-up, so flip the row
    const cellX = col * ctx.stride;
    const cellY = (ctx.numRows - 1 - row) * ctx.stride;

    // First clear the entire cell (including padding) to transparent
    canvasCtx.renderer.setViewport(cellX, cellY, ctx.stride, ctx.stride);
    canvasCtx.renderer.setScissor(cellX, cellY, ctx.stride, ctx.stride);
    canvasCtx.renderer.setScissorTest(true);
    canvasCtx.renderer.clear();

    // Then set viewport/scissor to just the dice area (centered with padding)
    const slotX = cellX + ctx.paddingOffset;
    const slotY = cellY + ctx.paddingOffset;
    canvasCtx.renderer.setViewport(slotX, slotY, ctx.slotSize, ctx.slotSize);
    canvasCtx.renderer.setScissor(slotX, slotY, ctx.slotSize, ctx.slotSize);

    // Render Three.js scene to the slot region
    canvasCtx.renderer.render(ctx.scene, ctx.camera);

    // Force PixiJS to re-read the canvas content
    canvasCtx.texture.source.update();
  })()
  """
    renderDiceFrame :: JSVal    -- ^ Renderer context
                    -> Int      -- ^ Slot index
                    -> Float    -- ^ Rotation X (radians)
                    -> Float    -- ^ Rotation Y (radians)
                    -> Float    -- ^ Rotation Z (radians)
                    -> JSString -- ^ Dice color (e.g., "0xffffff") - used for non-D6 dice
                    -> Int      -- ^ Dice type (4, 6, 8, 12, or 20)
                    -> JSVal    -- ^ Pre-created D6 materials (from createD6Materials)
                    -> IO ()

-- | Acquire a dice slot from the pool for a new animation.
-- Returns the global slot index, or -1 if no slots available.
-- Allocates new canvases as needed, up to maxCanvases.
foreign import javascript safe
  """
  (() => {
    const ctx = $1;

    if (ctx.freeSlots.length === 0) {
      // No free slots, try to allocate a new canvas
      if (ctx.canvasContexts.length >= ctx.maxCanvases) {
        // At max canvases, return -1 to signal no slot available
        return -1;
      }

      // Create a new canvas
      const newCanvasIndex = ctx.canvasContexts.length;
      ctx.canvasContexts.push(ctx.createCanvasContext());

      // Add all slots from new canvas to free pool
      for (let i = 0; i < ctx.slotsPerCanvas; i++) {
        ctx.freeSlots.push({canvasIndex: newCanvasIndex, localSlot: i});
      }
    }

    // Pop a free slot and convert to global slot ID
    const slot = ctx.freeSlots.pop();
    return slot.canvasIndex * ctx.slotsPerCanvas + slot.localSlot;
  })()
  """
    acquireDiceSlot :: JSVal -> IO Int

-- | Release a dice slot back to the pool after animation completes.
foreign import javascript unsafe
  """
  (() => {
    const ctx = $1;
    const globalSlot = $2;
    const canvasIndex = Math.floor(globalSlot / ctx.slotsPerCanvas);
    const localSlot = globalSlot % ctx.slotsPerCanvas;
    ctx.freeSlots.push({canvasIndex, localSlot});
  })()
  """
    releaseDiceSlot :: JSVal -> Int -> IO ()

-- | Get a PixiJS texture for a specific dice slot.
-- Returns a texture with a frame rectangle that views only that slot's region.
-- The frame includes frameExtra (5px) of padding on each side of the dice.
foreign import javascript safe
  """
  (() => {
    const ctx = $1;
    const globalSlot = $2;

    // Decode global slot ID into canvas index and local slot
    const canvasIndex = Math.floor(globalSlot / ctx.slotsPerCanvas);
    const localSlot = globalSlot % ctx.slotsPerCanvas;
    const canvasCtx = ctx.canvasContexts[canvasIndex];

    // Calculate 2D position in the grid
    const col = localSlot % ctx.slotsPerRow;
    const row = Math.floor(localSlot / ctx.slotsPerRow);
    // PixiJS Y is top-down (0,0 at top-left)
    // Frame includes 5px padding on each side (frameExtra on each side)
    const frameX = col * ctx.stride;
    const frameY = row * ctx.stride;
    const frameSize = ctx.slotSize + ctx.frameExtra * 2;  // slotSize + 10 (5 on each side)

    const frame = new PIXI.Rectangle(
      frameX, frameY,
      frameSize, frameSize
    );

    return new PIXI.Texture({ source: canvasCtx.texture.source, frame: frame });
  })()
  """
    getDiceSlotTexture :: JSVal -> Int -> IO JSVal

-- *****************************************************************************
-- * Window Management
-- *****************************************************************************

-- | Closes the application.
-- In Electron, this uses the electronApp.quit() IPC call.
-- In a browser, this falls back to window.close() which may be restricted.
foreign import javascript unsafe
  """
  if (window.electronApp && window.electronApp.quit) {
    window.electronApp.quit();
  } else {
    window.close();
  }
  """
    closeWindow :: IO ()
