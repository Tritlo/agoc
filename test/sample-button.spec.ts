import { test, expect } from '@playwright/test';

test.describe('Sample Button', () => {
  test('clicking sample button changes the score', async ({ page }) => {
    // Navigate to the app
    await page.goto('/');

    // Wait for WASM to initialize (canvas should be added to the page)
    const canvas = await page.waitForSelector('canvas', { timeout: 8000 });
    expect(canvas).toBeTruthy();

    // Give the app a moment to fully render the start screen
    await page.waitForTimeout(1000);

    // First, we need to navigate from the start screen to the game screen
    // Click "Start Game" which is the first menu item at center of screen
    const boundingBox = await canvas.boundingBox();
    if (!boundingBox) throw new Error('Canvas has no bounding box');

    const centerX = boundingBox.x + boundingBox.width / 2;
    const menuY = boundingBox.y + boundingBox.height / 2;

    // Click "Start Game"
    await page.mouse.click(centerX, menuY);

    // Wait for the game screen to load (histogram takes time to render)
    await page.waitForTimeout(1500);

    // Helper function to get score text from the stage
    const getScoreText = async () => {
      return await page.evaluate(() => {
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
    };

    // Get initial score
    const initialScore = await getScoreText();
    console.log(`Initial score: ${initialScore}`);
    expect(initialScore).toBe('Score: 0');

    // Click the Sample button (center-bottom of canvas, at height - 150)
    const clickX = boundingBox.x + boundingBox.width / 2;
    const clickY = boundingBox.y + boundingBox.height - 150;

    await page.mouse.click(clickX, clickY);

    // Wait for the dice animation to complete and state to update
    // Animation: fixed 2 seconds total (1.5s roll + 0.5s hold)
    await page.waitForTimeout(2500);

    // Get the new score
    const newScore = await getScoreText();
    console.log(`New score: ${newScore}`);

    // The score should have changed (sampling from N(500, 100) means it will be non-zero)
    expect(newScore).not.toEqual(initialScore);
    expect(newScore).toMatch(/^Score: \d+$/);
  });
});
