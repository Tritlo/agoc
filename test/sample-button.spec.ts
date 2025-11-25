import { test, expect } from '@playwright/test';

test.describe('Sample Button', () => {
  test('clicking sample button changes the score', async ({ page }) => {
    // Navigate to the app
    await page.goto('/');

    // Wait for WASM to initialize (canvas should be added to the page)
    const canvas = await page.waitForSelector('canvas', { timeout: 10000 });
    expect(canvas).toBeTruthy();

    // Wait for the SAMPLE button to be exported (indicates WASM is ready)
    await page.waitForFunction(() => (window as any).SAMPLE !== undefined, { timeout: 10000 });

    // Give the app a moment to fully render
    await page.waitForTimeout(500);

    // Helper function to get score text from the stage
    const getScoreText = async () => {
      return await page.evaluate(() => {
        const sample = (window as any).SAMPLE;
        if (!sample?.parent?.children) return null;
        for (const child of sample.parent.children) {
          if (child.text && child.text.startsWith('Score:')) {
            return child.text;
          }
        }
        return null;
      });
    };

    // Get initial score
    const initialScore = await getScoreText();
    console.log(`Initial score: ${initialScore}`);
    expect(initialScore).toBe('Score: 0');

    // Click the Sample button (center-bottom of canvas)
    const boundingBox = await canvas.boundingBox();
    if (!boundingBox) throw new Error('Canvas has no bounding box');

    const clickX = boundingBox.x + boundingBox.width / 2;
    const clickY = boundingBox.y + boundingBox.height - 200;

    await page.mouse.click(clickX, clickY);

    // Wait for the state to update
    await page.waitForTimeout(500);

    // Get the new score
    const newScore = await getScoreText();
    console.log(`New score: ${newScore}`);

    // The score should have changed (sampling from N(500, 100) means it will be non-zero)
    expect(newScore).not.toEqual(initialScore);
    expect(newScore).toMatch(/^Score: \d+$/);
  });
});
