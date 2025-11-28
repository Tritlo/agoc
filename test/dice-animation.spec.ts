import { test, expect } from '@playwright/test';

test.describe('Dice Animation', () => {
  test('THREE.js is loaded and available globally', async ({ page }) => {
    // Listen for console errors
    const consoleErrors: string[] = [];
    page.on('console', msg => {
      if (msg.type() === 'error') {
        consoleErrors.push(msg.text());
      }
    });

    // Navigate to the app
    await page.goto('/');

    // Wait for page to load
    await page.waitForTimeout(500);

    // Check that THREE is defined
    const threeIsDefined = await page.evaluate(() => {
      return typeof (window as any).THREE !== 'undefined';
    });
    expect(threeIsDefined).toBe(true);

    // Check that THREE has the expected classes for dice rendering
    const hasRequiredClasses = await page.evaluate(() => {
      const THREE = (window as any).THREE;
      return THREE &&
             typeof THREE.WebGLRenderer === 'function' &&
             typeof THREE.Scene === 'function' &&
             typeof THREE.PerspectiveCamera === 'function' &&
             typeof THREE.BoxGeometry === 'function' &&
             typeof THREE.MeshPhongMaterial === 'function' &&
             typeof THREE.AmbientLight === 'function' &&
             typeof THREE.DirectionalLight === 'function';
    });
    expect(hasRequiredClasses).toBe(true);
  });

  test('clicking sample button shows dice animation without errors', async ({ page }) => {
    // Listen for console messages
    const consoleErrors: string[] = [];
    const consoleLogs: string[] = [];
    const pageErrors: string[] = [];

    page.on('console', msg => {
      if (msg.type() === 'error') {
        consoleErrors.push(msg.text());
      } else {
        consoleLogs.push(`[${msg.type()}] ${msg.text()}`);
      }
    });

    page.on('pageerror', error => {
      pageErrors.push(error.message);
    });

    // Navigate to the app
    await page.goto('/');

    // Wait for WASM to initialize
    const canvas = await page.waitForSelector('canvas', { timeout: 30000 });
    expect(canvas).toBeTruthy();

    // Wait for app to fully render
    await page.waitForTimeout(1000);

    // Click "Start Game" to get to game screen
    const boundingBox = await canvas.boundingBox();
    if (!boundingBox) throw new Error('Canvas has no bounding box');

    const centerX = boundingBox.x + boundingBox.width / 2;
    const menuY = boundingBox.y + boundingBox.height / 2;

    await page.mouse.click(centerX, menuY);

    // Wait for game screen to load
    await page.waitForTimeout(1500);

    // Click the Sample button
    const clickX = boundingBox.x + boundingBox.width / 2;
    const clickY = boundingBox.y + boundingBox.height - 150;

    await page.mouse.click(clickX, clickY);

    // Wait for the dice animation to complete (fixed 2 seconds + buffer)
    await page.waitForTimeout(2500);

    // Log any errors for debugging
    if (consoleErrors.length > 0) {
      console.log('Console errors:', consoleErrors);
    }

    // Check that no THREE.js related errors occurred
    const threeErrors = consoleErrors.filter(e =>
      e.includes('THREE') || e.includes('WebGL') || e.includes('is not defined')
    );
    expect(threeErrors).toHaveLength(0);

    // Verify the score changed (animation completed and score updated)
    const scoreText = await page.evaluate(() => {
      const app = (window as any).__PIXI_APP__;
      if (!app?.stage?.children) return null;

      const findScore = (container: any): string | null => {
        if (container.text && container.text.startsWith('Score:')) {
          return container.text;
        }
        if (container.children) {
          for (const child of container.children) {
            const result = findScore(child);
            if (result) return result;
          }
        }
        return null;
      };
      return findScore(app.stage);
    });

    console.log('Score text:', scoreText);
    console.log('Console errors:', consoleErrors);
    console.log('Page errors:', pageErrors);

    // Score should have changed from 0
    expect(scoreText).not.toBe('Score: 0');
    expect(scoreText).toMatch(/^Score: \d+$/);
  });

  test('clicking roll 256 times rapidly does not break', async ({ page }) => {
    test.setTimeout(20000); // 20 second timeout
    // Listen for page errors
    const pageErrors: string[] = [];
    const consoleErrors: string[] = [];
    page.on('pageerror', error => {
      pageErrors.push(error.message);
    });
    page.on('console', msg => {
      if (msg.type() === 'error') {
        consoleErrors.push(msg.text());
      }
    });

    // Navigate to the app
    await page.goto('/');

    // Wait for WASM to initialize
    const canvas = await page.waitForSelector('canvas', { timeout: 30000 });
    expect(canvas).toBeTruthy();

    // Wait for app to fully render
    await page.waitForTimeout(1000);

    // Click "Start Game" to get to game screen
    const boundingBox = await canvas.boundingBox();
    if (!boundingBox) throw new Error('Canvas has no bounding box');

    const centerX = boundingBox.x + boundingBox.width / 2;
    const menuY = boundingBox.y + boundingBox.height / 2;

    await page.mouse.click(centerX, menuY);

    // Wait for game screen to load
    await page.waitForTimeout(1000);

    const clickX = boundingBox.x + boundingBox.width / 2;
    const clickY = boundingBox.y + boundingBox.height - 150;

    // Click 256 times as fast as possible using direct DOM events
    await page.evaluate(({x, y}) => {
      const canvas = document.querySelector('canvas');
      if (!canvas) return;
      for (let i = 0; i < 256; i++) {
        canvas.dispatchEvent(new MouseEvent('pointerdown', {
          bubbles: true, clientX: x, clientY: y, button: 0
        }));
        canvas.dispatchEvent(new MouseEvent('pointerup', {
          bubbles: true, clientX: x, clientY: y, button: 0
        }));
      }
    }, {x: clickX, y: clickY});

    // Brief wait for any errors to surface
    await page.waitForTimeout(1000);

    // Check for WebGL/texture errors
    const criticalErrors = [...pageErrors, ...consoleErrors].filter(e =>
      e.includes('WebGL') || e.includes('GL_INVALID') || e.includes('texture') || e.includes('overflow')
    );

    console.log('Page errors after 256 rapid clicks:', pageErrors);
    console.log('Console errors after 256 rapid clicks:', consoleErrors);
    expect(criticalErrors).toHaveLength(0);
  });

  test('clicking sample button multiple times does not exhaust WebGL contexts', async ({ page }) => {
    // Listen for page errors (WebGL context errors will show up here)
    const pageErrors: string[] = [];
    page.on('pageerror', error => {
      pageErrors.push(error.message);
    });

    // Navigate to the app
    await page.goto('/');

    // Wait for WASM to initialize
    const canvas = await page.waitForSelector('canvas', { timeout: 30000 });
    expect(canvas).toBeTruthy();

    // Wait for app to fully render
    await page.waitForTimeout(1000);

    // Click "Start Game" to get to game screen
    const boundingBox = await canvas.boundingBox();
    if (!boundingBox) throw new Error('Canvas has no bounding box');

    const centerX = boundingBox.x + boundingBox.width / 2;
    const menuY = boundingBox.y + boundingBox.height / 2;

    await page.mouse.click(centerX, menuY);

    // Wait for game screen to load
    await page.waitForTimeout(1500);

    // Click the Sample button multiple times rapidly
    // This would exhaust WebGL contexts if we create a new one each frame
    const clickX = boundingBox.x + boundingBox.width / 2;
    const clickY = boundingBox.y + boundingBox.height - 150;

    // Click 20 times with small delays (enough to trigger many animations)
    for (let i = 0; i < 20; i++) {
      await page.mouse.click(clickX, clickY);
      await page.waitForTimeout(100);
    }

    // Wait for animations to complete
    // Animation: fixed 2 seconds. After 20 clicks over ~2s, last animation finishes at ~4s
    await page.waitForTimeout(4500);

    // Check for WebGL context errors
    const webglErrors = pageErrors.filter(e =>
      e.includes('WebGL') || e.includes('context') || e.includes('Too many')
    );

    console.log('Page errors after 20 clicks:', pageErrors);
    expect(webglErrors).toHaveLength(0);

    // Verify the app is still functional (score should have increased)
    const scoreText = await page.evaluate(() => {
      const app = (window as any).__PIXI_APP__;
      if (!app?.stage?.children) return null;

      const findScore = (container: any): string | null => {
        if (container.text && container.text.startsWith('Score:')) {
          return container.text;
        }
        if (container.children) {
          for (const child of container.children) {
            const result = findScore(child);
            if (result) return result;
          }
        }
        return null;
      };
      return findScore(app.stage);
    });

    console.log('Score after 20 clicks:', scoreText);
    expect(scoreText).not.toBe('Score: 0');
    expect(scoreText).toMatch(/^Score: \d+$/);
  });
});
