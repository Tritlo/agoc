/**
 * Playwright Test Suite for pixijs-ffi
 *
 * This test suite validates the Haskell FFI bindings to PixiJS v8 by:
 * 1. Loading a test WASM module that exports test functions
 * 2. Calling each test function from Playwright
 * 3. Verifying the results stored in window.TEST_RESULTS
 *
 * Each test function in Haskell exercises specific FFI bindings and stores
 * results that we can verify from JavaScript.
 */

import { test, expect, Page } from '@playwright/test';

// Helper to wait for the test WASM module to be ready
async function waitForTestReady(page: Page, timeout = 30000): Promise<void> {
  await page.waitForFunction(
    () => (window as any).PIXIJS_FFI_TEST_READY === true,
    { timeout }
  );
}

// Helper to get test results
async function getTestResults(page: Page): Promise<Record<string, any>> {
  return await page.evaluate(() => (window as any).TEST_RESULTS || {});
}

// Helper to get a specific test result
async function getTestResult(page: Page, key: string): Promise<any> {
  return await page.evaluate((k) => (window as any).TEST_RESULTS?.[k], key);
}

// Helper to run a Haskell test function and wait for results
async function runHaskellTest(page: Page, testName: string): Promise<void> {
  await page.evaluate((name) => {
    // Reset TEST_RESULTS before each test to avoid false positives
    (window as any).TEST_RESULTS = {};
    const fn = (window as any)[name];
    if (typeof fn === 'function') {
      fn();
    } else {
      throw new Error(`Test function ${name} not found`);
    }
  }, testName);
  // Give the test a moment to complete
  await page.waitForTimeout(100);
}

// =============================================================================
// Test Setup
// =============================================================================

test.describe('pixijs-ffi Library Tests', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/test/pixijs-ffi-test.html');
    await waitForTestReady(page);
  });

  // ===========================================================================
  // Application Module Tests
  // ===========================================================================

  test.describe('Graphics.PixiJS.Application', () => {
    test.beforeEach(async ({ page }) => {
      await runHaskellTest(page, 'testApplication');
    });

    test('newApp creates an application', async ({ page }) => {
      const result = await getTestResult(page, 'newApp');
      expect(result).toBe(true);
    });

    test('initApp initializes the application', async ({ page }) => {
      const result = await getTestResult(page, 'initApp');
      expect(result).toBe(true);
    });

    test('appendCanvas adds canvas to DOM', async ({ page }) => {
      const result = await getTestResult(page, 'appendCanvas');
      expect(result).toBe(true);

      // Verify canvas actually exists in DOM
      const canvasCount = await page.locator('canvas').count();
      expect(canvasCount).toBeGreaterThanOrEqual(1);
    });

    test('getStage returns a container', async ({ page }) => {
      const result = await getTestResult(page, 'getStage');
      expect(result).toBe(true);

      // Verify stage was exported
      const hasStage = await page.evaluate(() => (window as any).TEST_STAGE !== undefined);
      expect(hasStage).toBe(true);
    });

    test('getScreen returns valid dimensions', async ({ page }) => {
      const result = await getTestResult(page, 'getScreen');
      expect(result).toBe(true);

      const width = await getTestResult(page, 'screen_width');
      const height = await getTestResult(page, 'screen_height');
      expect(width).toBeGreaterThan(0);
      expect(height).toBeGreaterThan(0);
    });

    test('getTicker returns a ticker', async ({ page }) => {
      const result = await getTestResult(page, 'getTicker');
      expect(result).toBe(true);
    });

    test('getCanvas returns the canvas element', async ({ page }) => {
      const result = await getTestResult(page, 'getCanvas');
      expect(result).toBe(true);
    });
  });

  // ===========================================================================
  // Container Tests
  // ===========================================================================

  test.describe('Graphics.PixiJS.Display - Container', () => {
    test.beforeEach(async ({ page }) => {
      await runHaskellTest(page, 'testContainer');
    });

    test('newContainer creates a container', async ({ page }) => {
      const result = await getTestResult(page, 'newContainer');
      expect(result).toBe(true);
    });

    test('setX/getX works correctly', async ({ page }) => {
      const result = await getTestResult(page, 'setX_getX');
      expect(result).toBe(true);

      const x = await getTestResult(page, 'container_x');
      expect(x).toBeCloseTo(100, 1);
    });

    test('setY/getY works correctly', async ({ page }) => {
      const result = await getTestResult(page, 'setY_getY');
      expect(result).toBe(true);

      const y = await getTestResult(page, 'container_y');
      expect(y).toBeCloseTo(200, 1);
    });

    test('setPosition works correctly', async ({ page }) => {
      const result = await getTestResult(page, 'setPosition');
      expect(result).toBe(true);
    });

    test('setScale/getScaleX/getScaleY works correctly', async ({ page }) => {
      const result = await getTestResult(page, 'setScale');
      expect(result).toBe(true);

      const sx = await getTestResult(page, 'scale_x');
      const sy = await getTestResult(page, 'scale_y');
      expect(sx).toBeCloseTo(2.0, 1);
      expect(sy).toBeCloseTo(3.0, 1);
    });

    test('setRotation/getRotation works correctly', async ({ page }) => {
      const result = await getTestResult(page, 'setRotation_getRotation');
      expect(result).toBe(true);

      const rotation = await getTestResult(page, 'rotation');
      expect(rotation).toBeCloseTo(1.57, 1);
    });

    test('setVisible/getVisible works correctly', async ({ page }) => {
      const result = await getTestResult(page, 'setVisible_getVisible');
      expect(result).toBe(true);
    });

    test('setAlpha/getAlpha works correctly', async ({ page }) => {
      const result = await getTestResult(page, 'setAlpha_getAlpha');
      expect(result).toBe(true);

      const alpha = await getTestResult(page, 'alpha');
      expect(alpha).toBeCloseTo(0.5, 1);
    });

    test('addChild/removeChild/getNumChildren works correctly', async ({ page }) => {
      const result = await getTestResult(page, 'addChild_removeChild');
      expect(result).toBe(true);

      const n1 = await getTestResult(page, 'children_after_add1');
      const n2 = await getTestResult(page, 'children_after_add2');
      const n3 = await getTestResult(page, 'children_after_remove');
      expect(n1).toBe(1);
      expect(n2).toBe(2);
      expect(n3).toBe(1);
    });

    test('getChildAt works correctly', async ({ page }) => {
      const result = await getTestResult(page, 'getChildAt');
      expect(result).toBe(true);
    });

    test('setChildIndex/getChildIndex works correctly', async ({ page }) => {
      const result = await getTestResult(page, 'setChildIndex_getChildIndex');
      expect(result).toBe(true);
    });

    test('getBounds works correctly', async ({ page }) => {
      const result = await getTestResult(page, 'getBounds');
      expect(result).toBe(true);
    });

    test('getLocalBounds works correctly', async ({ page }) => {
      const result = await getTestResult(page, 'getLocalBounds');
      expect(result).toBe(true);
    });

    test('setPivot works correctly', async ({ page }) => {
      const result = await getTestResult(page, 'setPivot');
      expect(result).toBe(true);
    });

    test('getParent works correctly', async ({ page }) => {
      const result = await getTestResult(page, 'getParent');
      expect(result).toBe(true);
    });
  });

  // ===========================================================================
  // Sprite Tests
  // ===========================================================================

  test.describe('Graphics.PixiJS.Display - Sprite', () => {
    test.beforeEach(async ({ page }) => {
      await runHaskellTest(page, 'testSprite');
    });

    test('newSprite creates an empty sprite', async ({ page }) => {
      const result = await getTestResult(page, 'newSprite');
      expect(result).toBe(true);
    });

    test('setAnchor works correctly', async ({ page }) => {
      const result = await getTestResult(page, 'setAnchor');
      expect(result).toBe(true);
    });

    test('setAnchorX/setAnchorY works correctly', async ({ page }) => {
      const result = await getTestResult(page, 'setAnchorX_setAnchorY');
      expect(result).toBe(true);
    });

    test('setTint/getTint works correctly', async ({ page }) => {
      const result = await getTestResult(page, 'setTint_getTint');
      expect(result).toBe(true);

      const tint = await getTestResult(page, 'tint');
      expect(tint).toBe(0xFF0000);
    });

    test('setWidth/getWidth works', async ({ page }) => {
      const result = await getTestResult(page, 'setWidth_getWidth');
      expect(result).toBe(true);
    });

    test('setHeight/getHeight works', async ({ page }) => {
      const result = await getTestResult(page, 'setHeight_getHeight');
      expect(result).toBe(true);
    });
  });

  // ===========================================================================
  // Text Tests
  // ===========================================================================

  test.describe('Graphics.PixiJS.Text', () => {
    test.beforeEach(async ({ page }) => {
      await runHaskellTest(page, 'testText');
    });

    test('newText creates text', async ({ page }) => {
      const result = await getTestResult(page, 'newText');
      expect(result).toBe(true);
    });

    test('newTextWithStyle creates styled text', async ({ page }) => {
      const result = await getTestResult(page, 'newTextWithStyle');
      expect(result).toBe(true);
    });

    test('setText/getText works correctly', async ({ page }) => {
      const result = await getTestResult(page, 'setText_getText');
      expect(result).toBe(true);

      const content = await getTestResult(page, 'text_content');
      expect(content).toBe('New Text');
    });

    test('text position can be set', async ({ page }) => {
      const result = await getTestResult(page, 'text_position');
      expect(result).toBe(true);
    });

    test('text setAnchor works', async ({ page }) => {
      const result = await getTestResult(page, 'text_setAnchor');
      expect(result).toBe(true);
    });

    test('text setAlpha works', async ({ page }) => {
      const result = await getTestResult(page, 'text_alpha');
      expect(result).toBe(true);
    });
  });

  // ===========================================================================
  // Graphics Tests
  // ===========================================================================

  test.describe('Graphics.PixiJS.Graphics', () => {
    test.beforeEach(async ({ page }) => {
      await runHaskellTest(page, 'testGraphics');
    });

    test('newGraphics creates a graphics object', async ({ page }) => {
      const result = await getTestResult(page, 'newGraphics');
      expect(result).toBe(true);
    });

    test('drawRect works', async ({ page }) => {
      const result = await getTestResult(page, 'drawRect');
      expect(result).toBe(true);
    });

    test('fill works', async ({ page }) => {
      const result = await getTestResult(page, 'fill');
      expect(result).toBe(true);
    });

    test('clear works', async ({ page }) => {
      const result = await getTestResult(page, 'clear');
      expect(result).toBe(true);
    });

    test('drawCircle works', async ({ page }) => {
      const result = await getTestResult(page, 'drawCircle');
      expect(result).toBe(true);
    });

    test('drawEllipse works', async ({ page }) => {
      const result = await getTestResult(page, 'drawEllipse');
      expect(result).toBe(true);
    });

    test('drawRoundedRect works', async ({ page }) => {
      const result = await getTestResult(page, 'drawRoundedRect');
      expect(result).toBe(true);
    });

    test('moveTo/lineTo works', async ({ page }) => {
      const result = await getTestResult(page, 'moveTo_lineTo');
      expect(result).toBe(true);
    });

    test('drawPolygon works', async ({ page }) => {
      const result = await getTestResult(page, 'drawPolygon');
      expect(result).toBe(true);
    });

    test('bezierCurveTo works', async ({ page }) => {
      const result = await getTestResult(page, 'bezierCurveTo');
      expect(result).toBe(true);
    });

    test('quadraticCurveTo works', async ({ page }) => {
      const result = await getTestResult(page, 'quadraticCurveTo');
      expect(result).toBe(true);
    });

    test('arc works', async ({ page }) => {
      const result = await getTestResult(page, 'arc');
      expect(result).toBe(true);
    });

    test('closePath works', async ({ page }) => {
      const result = await getTestResult(page, 'closePath');
      expect(result).toBe(true);
    });

    test('fillWithAlpha works', async ({ page }) => {
      const result = await getTestResult(page, 'fillWithAlpha');
      expect(result).toBe(true);
    });

    test('graphics position can be set', async ({ page }) => {
      const result = await getTestResult(page, 'graphics_position');
      expect(result).toBe(true);
    });
  });

  // ===========================================================================
  // Math Tests
  // ===========================================================================

  test.describe('Graphics.PixiJS.Math', () => {
    test.beforeEach(async ({ page }) => {
      await runHaskellTest(page, 'testMath');
    });

    test('newPoint creates a point with correct coordinates', async ({ page }) => {
      const result = await getTestResult(page, 'newPoint');
      expect(result).toBe(true);

      const x = await getTestResult(page, 'point_x');
      const y = await getTestResult(page, 'point_y');
      expect(x).toBeCloseTo(10, 1);
      expect(y).toBeCloseTo(20, 1);
    });

    test('setPointX/setPointY works', async ({ page }) => {
      const result = await getTestResult(page, 'setPoint');
      expect(result).toBe(true);
    });

    test('newRectangle creates a rectangle with correct properties', async ({ page }) => {
      const result = await getTestResult(page, 'newRectangle');
      expect(result).toBe(true);

      expect(await getTestResult(page, 'rect_x')).toBeCloseTo(10, 1);
      expect(await getTestResult(page, 'rect_y')).toBeCloseTo(20, 1);
      expect(await getTestResult(page, 'rect_width')).toBeCloseTo(100, 1);
      expect(await getTestResult(page, 'rect_height')).toBeCloseTo(200, 1);
    });

    test('getRectLeft/Right/Top/Bottom works', async ({ page }) => {
      const result = await getTestResult(page, 'rect_edges');
      expect(result).toBe(true);
    });

    test('containsRect works', async ({ page }) => {
      const result = await getTestResult(page, 'containsRect');
      expect(result).toBe(true);
    });

    test('newCircle creates a circle with correct properties', async ({ page }) => {
      const result = await getTestResult(page, 'newCircle');
      expect(result).toBe(true);

      expect(await getTestResult(page, 'circle_x')).toBeCloseTo(50, 1);
      expect(await getTestResult(page, 'circle_y')).toBeCloseTo(50, 1);
      expect(await getTestResult(page, 'circle_radius')).toBeCloseTo(25, 1);
    });

    test('containsCircle works', async ({ page }) => {
      const result = await getTestResult(page, 'containsCircle');
      expect(result).toBe(true);
    });

    test('newEllipse creates an ellipse with correct properties', async ({ page }) => {
      const result = await getTestResult(page, 'newEllipse');
      expect(result).toBe(true);
    });

    test('newMatrix creates a matrix', async ({ page }) => {
      const result = await getTestResult(page, 'newMatrix');
      expect(result).toBe(true);
    });

    test('translateMatrix works', async ({ page }) => {
      const result = await getTestResult(page, 'translateMatrix');
      expect(result).toBe(true);
    });

    test('scaleMatrix works', async ({ page }) => {
      const result = await getTestResult(page, 'scaleMatrix');
      expect(result).toBe(true);
    });

    test('rotateMatrix works', async ({ page }) => {
      const result = await getTestResult(page, 'rotateMatrix');
      expect(result).toBe(true);
    });

    test('cloneRect works', async ({ page }) => {
      const result = await getTestResult(page, 'cloneRect');
      expect(result).toBe(true);
    });

    test('cloneCircle works', async ({ page }) => {
      const result = await getTestResult(page, 'cloneCircle');
      expect(result).toBe(true);
    });
  });

  // ===========================================================================
  // Events Tests
  // ===========================================================================

  test.describe('Graphics.PixiJS.Events', () => {
    test.beforeEach(async ({ page }) => {
      await runHaskellTest(page, 'testEvents');
    });

    test('setEventMode works', async ({ page }) => {
      const result = await getTestResult(page, 'setEventMode');
      expect(result).toBe(true);
    });

    test('setCursor works', async ({ page }) => {
      const result = await getTestResult(page, 'setCursor');
      expect(result).toBe(true);
    });

    test('on (event listener) works', async ({ page }) => {
      const result = await getTestResult(page, 'on');
      expect(result).toBe(true);
    });

    test('once works', async ({ page }) => {
      const result = await getTestResult(page, 'once');
      expect(result).toBe(true);
    });

    test('off (remove listener) works', async ({ page }) => {
      const result = await getTestResult(page, 'off');
      expect(result).toBe(true);
    });

    test('setHitArea works', async ({ page }) => {
      const result = await getTestResult(page, 'setHitArea');
      expect(result).toBe(true);
    });
  });

  // ===========================================================================
  // Filters Tests
  // ===========================================================================

  test.describe('Graphics.PixiJS.Filters', () => {
    test.beforeEach(async ({ page }) => {
      await runHaskellTest(page, 'testFilters');
    });

    test('newBlurFilter creates a blur filter', async ({ page }) => {
      const result = await getTestResult(page, 'newBlurFilter');
      expect(result).toBe(true);
    });

    test('setBlurStrength/getBlurStrength works', async ({ page }) => {
      const result = await getTestResult(page, 'setBlurStrength_getBlurStrength');
      expect(result).toBe(true);

      const strength = await getTestResult(page, 'blur_strength');
      expect(strength).toBeCloseTo(12, 1);
    });

    test('setBlurQuality/getBlurQuality works', async ({ page }) => {
      const result = await getTestResult(page, 'setBlurQuality_getBlurQuality');
      expect(result).toBe(true);
    });

    test('newColorMatrixFilter creates a color matrix filter', async ({ page }) => {
      const result = await getTestResult(page, 'newColorMatrixFilter');
      expect(result).toBe(true);
    });

    test('colorMatrixBrightness works', async ({ page }) => {
      const result = await getTestResult(page, 'colorMatrixBrightness');
      expect(result).toBe(true);
    });

    test('colorMatrixContrast works', async ({ page }) => {
      const result = await getTestResult(page, 'colorMatrixContrast');
      expect(result).toBe(true);
    });

    test('colorMatrixSaturate works', async ({ page }) => {
      const result = await getTestResult(page, 'colorMatrixSaturate');
      expect(result).toBe(true);
    });

    test('colorMatrixHue works', async ({ page }) => {
      const result = await getTestResult(page, 'colorMatrixHue');
      expect(result).toBe(true);
    });

    test('colorMatrixGrayscale works', async ({ page }) => {
      const result = await getTestResult(page, 'colorMatrixGrayscale');
      expect(result).toBe(true);
    });

    test('colorMatrixSepia works', async ({ page }) => {
      const result = await getTestResult(page, 'colorMatrixSepia');
      expect(result).toBe(true);
    });

    test('colorMatrixNegative works', async ({ page }) => {
      const result = await getTestResult(page, 'colorMatrixNegative');
      expect(result).toBe(true);
    });

    test('colorMatrixReset works', async ({ page }) => {
      const result = await getTestResult(page, 'colorMatrixReset');
      expect(result).toBe(true);
    });

    test('newAlphaFilter creates an alpha filter', async ({ page }) => {
      const result = await getTestResult(page, 'newAlphaFilter');
      expect(result).toBe(true);
    });

    test('newNoiseFilter creates a noise filter', async ({ page }) => {
      const result = await getTestResult(page, 'newNoiseFilter');
      expect(result).toBe(true);
    });

    test('addFilter works', async ({ page }) => {
      const result = await getTestResult(page, 'addFilter');
      expect(result).toBe(true);
    });

    test('getFilters works', async ({ page }) => {
      const result = await getTestResult(page, 'getFilters');
      expect(result).toBe(true);
    });

    test('removeFilter works', async ({ page }) => {
      const result = await getTestResult(page, 'removeFilter');
      expect(result).toBe(true);
    });
  });

  // ===========================================================================
  // Ticker Tests
  // ===========================================================================

  test.describe('Graphics.PixiJS.Ticker', () => {
    test.beforeEach(async ({ page }) => {
      await runHaskellTest(page, 'testTicker');
    });

    test('newTicker creates a ticker', async ({ page }) => {
      const result = await getTestResult(page, 'newTicker');
      expect(result).toBe(true);
    });

    test('getSharedTicker returns the shared ticker', async ({ page }) => {
      const result = await getTestResult(page, 'getSharedTicker');
      expect(result).toBe(true);
    });

    test('start works', async ({ page }) => {
      const result = await getTestResult(page, 'start');
      expect(result).toBe(true);
    });

    test('stop works', async ({ page }) => {
      const result = await getTestResult(page, 'stop');
      expect(result).toBe(true);
    });

    test('getFPS works', async ({ page }) => {
      const result = await getTestResult(page, 'getFPS');
      expect(result).toBe(true);
    });

    test('getDeltaTime works', async ({ page }) => {
      const result = await getTestResult(page, 'getDeltaTime');
      expect(result).toBe(true);
    });

    test('getDeltaMS works', async ({ page }) => {
      const result = await getTestResult(page, 'getDeltaMS');
      expect(result).toBe(true);
    });

    test('setSpeed/getSpeed works', async ({ page }) => {
      const result = await getTestResult(page, 'setSpeed_getSpeed');
      expect(result).toBe(true);
    });

    test('setMaxFPS/getMaxFPS works', async ({ page }) => {
      const result = await getTestResult(page, 'setMaxFPS_getMaxFPS');
      expect(result).toBe(true);
    });

    test('setMinFPS/getMinFPS works', async ({ page }) => {
      const result = await getTestResult(page, 'setMinFPS_getMinFPS');
      expect(result).toBe(true);
    });

    test('add callback works', async ({ page }) => {
      const result = await getTestResult(page, 'add');
      expect(result).toBe(true);
    });

    test('remove callback works', async ({ page }) => {
      const result = await getTestResult(page, 'remove');
      expect(result).toBe(true);
    });
  });

  // ===========================================================================
  // Interop Tests
  // ===========================================================================

  test.describe('Graphics.PixiJS.Interop', () => {
    test.beforeEach(async ({ page }) => {
      await runHaskellTest(page, 'testInterop');
    });

    test('floatAsVal/valAsFloat roundtrip works', async ({ page }) => {
      const result = await getTestResult(page, 'floatAsVal_valAsFloat');
      expect(result).toBe(true);

      const value = await getTestResult(page, 'float_roundtrip');
      expect(value).toBeCloseTo(3.14, 2);
    });

    test('intAsVal/valAsInt roundtrip works', async ({ page }) => {
      const result = await getTestResult(page, 'intAsVal_valAsInt');
      expect(result).toBe(true);

      const value = await getTestResult(page, 'int_roundtrip');
      expect(value).toBe(42);
    });

    test('boolAsVal/valAsBool roundtrip works', async ({ page }) => {
      const result = await getTestResult(page, 'boolAsVal_valAsBool');
      expect(result).toBe(true);
    });

    test('stringAsVal works', async ({ page }) => {
      const result = await getTestResult(page, 'stringAsVal');
      expect(result).toBe(true);
    });

    test('toJSString/fromJSString works', async ({ page }) => {
      const result = await getTestResult(page, 'toJSString_fromJSString');
      expect(result).toBe(true);
    });

    test('exportValue works', async ({ page }) => {
      const result = await getTestResult(page, 'exportValue');
      expect(result).toBe(true);

      // Verify the value was exported
      const exported = await page.evaluate(() => (window as any).INTEROP_TEST_VALUE);
      expect(exported).toBe(999);
    });

    test('setGlobalVariable works', async ({ page }) => {
      const result = await getTestResult(page, 'setGlobalVariable');
      expect(result).toBe(true);

      // Verify the global was set
      const global = await page.evaluate(() => (window as any).INTEROP_GLOBAL);
      expect(global).toBe('global value');
    });

    test('parseJSON works', async ({ page }) => {
      const result = await getTestResult(page, 'parseJSON');
      expect(result).toBe(true);
    });

    test('getProperty works', async ({ page }) => {
      const result = await getTestResult(page, 'getProperty');
      expect(result).toBe(true);
    });

    test('setProperty works', async ({ page }) => {
      const result = await getTestResult(page, 'setProperty');
      expect(result).toBe(true);
    });

    test('consoleLogShow works', async ({ page }) => {
      const result = await getTestResult(page, 'consoleLogShow');
      expect(result).toBe(true);
    });

    test('consoleLogVal works', async ({ page }) => {
      const result = await getTestResult(page, 'consoleLogVal');
      expect(result).toBe(true);
    });

    test('jsFuncFromHs works', async ({ page }) => {
      const result = await getTestResult(page, 'jsFuncFromHs');
      expect(result).toBe(true);
    });

    test('jsFuncFromHs_ works', async ({ page }) => {
      const result = await getTestResult(page, 'jsFuncFromHs_');
      expect(result).toBe(true);
    });
  });

  // ===========================================================================
  // Run All Tests
  // ===========================================================================

  test.describe('Full Test Suite', () => {
    test('runAllTests completes successfully', async ({ page }) => {
      await runHaskellTest(page, 'runAllTests');

      const result = await getTestResult(page, 'ALL_TESTS_COMPLETE');
      expect(result).toBe(true);

      // Log all results for debugging (only boolean results to avoid circular refs)
      const results = await getTestResults(page);
      const boolResults = Object.entries(results).filter(([_, v]) => typeof v === 'boolean');
      console.log('Boolean test results:', Object.fromEntries(boolResults));

      // Count passed/failed
      const passed = boolResults.filter(([_, v]) => v === true).length;
      const failed = boolResults.filter(([_, v]) => v === false).length;

      console.log(`Total: ${passed} passed, ${failed} failed`);
      expect(failed).toBe(0);
    });
  });
});
