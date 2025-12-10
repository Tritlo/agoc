import { test, expect } from './fixtures';
import { navigateToGame, selectDiceAndRoll, getScoreText } from './test-helpers';

test.describe('Dice Animation', () => {
  test('THREE.js is loaded and available globally', async ({ page }, testInfo) => {
    // Listen for console errors
    const consoleErrors: string[] = [];
    page.on('console', msg => {
      if (msg.type() === 'error') {
        consoleErrors.push(msg.text());
      }
    });

    // Navigate to the app
    await page.goto('/');
    await page.screenshot({ path: testInfo.outputPath('after-goto.png'), fullPage: true });

    // Wait for spritesheet generation and page to load (~2-4 seconds)
    await page.waitForTimeout(5000);
    await page.screenshot({ path: testInfo.outputPath('after-wait.png'), fullPage: true });

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

  test('selecting dice and rolling shows animation without errors', async ({ page }, testInfo) => {
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

    const { canvas, boundingBox } = await navigateToGame(page, testInfo);

    // Select dice and roll
    await selectDiceAndRoll(page, boundingBox, 2);

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
    const scoreText = await getScoreText(page);

    console.log('Score text:', scoreText);
    console.log('Console errors:', consoleErrors);
    console.log('Page errors:', pageErrors);

    // Score should have changed from 0
    expect(scoreText).not.toBe('Score: 0');
    expect(scoreText).toMatch(/^Score: \d+$/);
  });

  test('clicking roll 256 times rapidly does not break', async ({ page }, testInfo) => {
    test.setTimeout(30000); // 30 second timeout
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

    const { canvas, boundingBox } = await navigateToGame(page, testInfo);

    const clickX = boundingBox.x + boundingBox.width / 2;
    const clickY = boundingBox.y + boundingBox.height - 150;

    // Click 256 times as fast as possible using direct DOM events
    // This tests the Roll button area - with the new mechanics, rolls without selection won't do much
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

  test('selecting dice and rolling multiple times does not exhaust WebGL contexts', async ({ page }) => {
    test.setTimeout(60000); // 60 second timeout for multiple rolls
    // Listen for page errors (WebGL context errors will show up here)
    const pageErrors: string[] = [];
    page.on('pageerror', error => {
      pageErrors.push(error.message);
    });

    const { canvas, boundingBox } = await navigateToGame(page);

    // Roll 5 times with proper dice selection each time
    for (let i = 0; i < 5; i++) {
      await selectDiceAndRoll(page, boundingBox, 2);
      await page.waitForTimeout(500); // Small buffer between rounds
    }

    // Check for WebGL context errors
    const webglErrors = pageErrors.filter(e =>
      e.includes('WebGL') || e.includes('context') || e.includes('Too many')
    );

    console.log('Page errors after 5 rolls:', pageErrors);
    expect(webglErrors).toHaveLength(0);

    // Verify the app is still functional (score should have increased)
    const scoreText = await getScoreText(page);

    console.log('Score after 5 rolls:', scoreText);
    expect(scoreText).not.toBe('Score: 0');
    expect(scoreText).toMatch(/^Score: \d+$/);
  });
});
