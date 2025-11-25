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
