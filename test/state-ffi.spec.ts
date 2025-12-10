import { test, expect } from './fixtures';
import { clickText } from './test-helpers';

test.describe('State FFI (window.GAMESTATE)', () => {
  test.beforeEach(async ({ page }, testInfo) => {
    await page.goto('/');
    await page.screenshot({ path: testInfo.outputPath('after-goto.png'), fullPage: true });
    const canvas = await page.waitForSelector('canvas', { timeout: 8000 });
    expect(canvas).toBeTruthy();
    await page.screenshot({ path: testInfo.outputPath('after-canvas.png'), fullPage: true });
    // Wait for spritesheet/build to finish
    await page.waitForTimeout(5000);
    await page.screenshot({ path: testInfo.outputPath('after-wait.png'), fullPage: true });
  });

test('window.GAMESTATE updates through start → pause → quit → start', async ({ page }, testInfo) => {
    const canvas = await page.locator('canvas');
    const boundingBox = await canvas.boundingBox();
    if (!boundingBox) throw new Error('Canvas has no bounding box');

    const centerX = boundingBox.x + boundingBox.width / 2;
    const menuStartY = boundingBox.y + boundingBox.height / 2;
    const menuButtonY = boundingBox.y + boundingBox.height - 55; // Menu button on game screen
    const pauseQuitY = boundingBox.y + boundingBox.height / 2 + 70; // Second pause item (Quit)

    const readState = async () => {
      return await page.evaluate(() => (window as any).GAMESTATE);
    };

    // Initial state should be start screen
    let state = await readState();
    expect(state?.gss_screen).toBe('StartScreen');

    // Start game
    await clickText(page, 'Start Game', testInfo, 'click-start-game');
    await page.waitForTimeout(800);
    await page.screenshot({ path: testInfo.outputPath('after-start.png'), fullPage: true });
    state = await readState();
    expect(state?.gss_screen).toBe('GameScreen');

    // Pause via Menu button
    await clickText(page, 'Menu', testInfo, 'click-menu');
    await page.waitForTimeout(500);
    await page.screenshot({ path: testInfo.outputPath('after-pause.png'), fullPage: true });
    state = await readState();
    expect(state?.gss_screen).toBe('PauseScreen');

    // Quit to start (second pause menu item)
    await clickText(page, 'Quit', testInfo, 'click-quit');
    await page.waitForTimeout(800);
    await page.screenshot({ path: testInfo.outputPath('after-quit.png'), fullPage: true });
    state = await readState();
    expect(state?.gss_screen).toBe('StartScreen');
    expect(state?.gss_score).toBe(0);

    // Start again
    await clickText(page, 'Start Game', testInfo, 'click-start-again');
    await page.waitForTimeout(800);
    await page.screenshot({ path: testInfo.outputPath('after-restart.png'), fullPage: true });
    state = await readState();
    expect(state?.gss_screen).toBe('GameScreen');
    expect(state?.gss_score).toBe(0);
    expect(state?.gss_round).toBe(1);
  });
});

