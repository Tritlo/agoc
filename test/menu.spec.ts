import { test, expect } from './fixtures';
import { selectDiceAndRoll, clickText, waitForText } from './test-helpers';

test.describe('Menu System', () => {
  test.beforeEach(async ({ page }, testInfo) => {
    // Navigate to the app
    await page.goto('/');
    await page.screenshot({ path: testInfo.outputPath('after-goto.png'), fullPage: true });

    // Wait for WASM to initialize (canvas should be added to the page)
    const canvas = await page.waitForSelector('canvas', { timeout: 8000 });
    expect(canvas).toBeTruthy();
    await page.screenshot({ path: testInfo.outputPath('after-canvas.png'), fullPage: true });

    // Wait for spritesheet generation (loading screen) and app to fully initialize
    // Spritesheet generation takes ~2-4 seconds
    await page.waitForTimeout(5000);
    await page.screenshot({ path: testInfo.outputPath('after-wait.png'), fullPage: true });
  });

  test('start screen displays menu items', async ({ page }, testInfo) => {
    // Check that the start screen has the expected menu items by looking at the PixiJS stage
    const menuItems = await page.evaluate(() => {
      const app = (window as any).__PIXI_APP__;
      if (!app?.stage?.children) return [];

      const texts: string[] = [];
      const findTexts = (container: any) => {
        if (container.text) {
          texts.push(container.text);
        }
        if (container.children) {
          for (const child of container.children) {
            findTexts(child);
          }
        }
      };
      findTexts(app.stage);
      return texts;
    });

    console.log('Found texts:', menuItems);

    // Should have title and menu items
    expect(menuItems).toContain('A Game of Chance');
    expect(menuItems).toContain('Start Game');
    expect(menuItems).toContain('Options');
    expect(menuItems).toContain('Exit');
  });

  test('clicking Options navigates to options screen', async ({ page }, testInfo) => {
    const canvas = await page.locator('canvas');
    const boundingBox = await canvas.boundingBox();
    if (!boundingBox) throw new Error('Canvas has no bounding box');

    // Options: click by text for reliability
    await clickText(page, 'Options', testInfo, 'click-options');
    await page.waitForTimeout(500);
    const state = await page.evaluate(() => (window as any).GAMESTATE);
    expect(state?.gss_screen).toBe('OptionsScreen');

    // Check that we're now on the options screen
    const texts = await page.evaluate(() => {
      const app = (window as any).__PIXI_APP__;
      if (!app?.stage?.children) return [];

      const texts: string[] = [];
      const findTexts = (container: any) => {
        if (container.text) {
          texts.push(container.text);
        }
        if (container.children) {
          for (const child of container.children) {
            findTexts(child);
          }
        }
      };
      findTexts(app.stage);
      return texts;
    });

    console.log('Options screen texts:', texts);

    expect(texts).toContain('Options');
    expect(texts).toContain('(No settings available)');
    expect(texts).toContain('Back');
    expect(texts).not.toContain('Start Game');
  });

  test('clicking Back from Options returns to start screen', async ({ page }, testInfo) => {
    const canvas = await page.locator('canvas');
    const boundingBox = await canvas.boundingBox();
    if (!boundingBox) throw new Error('Canvas has no bounding box');

    // First, go to Options
    await clickText(page, 'Options', testInfo, 'click-options');
    await page.waitForTimeout(500);

    // Now click Back
    await clickText(page, 'Back', testInfo, 'click-back');
    await page.waitForTimeout(500);
    const state = await page.evaluate(() => (window as any).GAMESTATE);
    expect(state?.gss_screen).toBe('StartScreen');

    // Check that we're back on the start screen
    const texts = await page.evaluate(() => {
      const app = (window as any).__PIXI_APP__;
      if (!app?.stage?.children) return [];

      const texts: string[] = [];
      const findTexts = (container: any) => {
        if (container.text) {
          texts.push(container.text);
        }
        if (container.children) {
          for (const child of container.children) {
            findTexts(child);
          }
        }
      };
      findTexts(app.stage);
      return texts;
    });

    console.log('Back to start screen texts:', texts);

    expect(texts).toContain('A Game of Chance');
    expect(texts).toContain('Start Game');
    expect(texts).toContain('Options');
  });

  test('clicking Start Game navigates to game screen', async ({ page }, testInfo) => {
    const canvas = await page.locator('canvas');
    const boundingBox = await canvas.boundingBox();
    if (!boundingBox) throw new Error('Canvas has no bounding box');

    // Start Game is the first menu item (index 0)
    const centerX = boundingBox.x + boundingBox.width / 2;
    const menuStartY = boundingBox.y + boundingBox.height / 2;

    await page.mouse.click(centerX, menuStartY);
    await page.waitForTimeout(1000); // Give time for histogram to render

    // Check that we're now on the game screen
    const texts = await page.evaluate(() => {
      const app = (window as any).__PIXI_APP__;
      if (!app?.stage?.children) return [];

      const texts: string[] = [];
      const findTexts = (container: any) => {
        if (container.text) {
          texts.push(container.text);
        }
        if (container.children) {
          for (const child of container.children) {
            findTexts(child);
          }
        }
      };
      findTexts(app.stage);
      return texts;
    });

    console.log('Game screen texts:', texts);

    // Game screen should have score, target, and roll button
    expect(texts.some(t => t.startsWith('Score:'))).toBe(true);
    expect(texts.some(t => t.startsWith('Target:'))).toBe(true);
    expect(texts).toContain('Roll');

    // Should not have start screen items
    expect(texts).not.toContain('A Game of Chance');
    expect(texts).not.toContain('Start Game');
  });

  test('game screen has Menu button', async ({ page }, testInfo) => {
    const canvas = await page.locator('canvas');
    const boundingBox = await canvas.boundingBox();
    if (!boundingBox) throw new Error('Canvas has no bounding box');

    // Navigate to game screen
    await clickText(page, 'Start Game', testInfo, 'click-start-game');
    await page.waitForTimeout(1000);

    // Check that Menu button exists
    const texts = await page.evaluate(() => {
      const app = (window as any).__PIXI_APP__;
      if (!app?.stage?.children) return [];

      const texts: string[] = [];
      const findTexts = (container: any) => {
        if (container.text) {
          texts.push(container.text);
        }
        if (container.children) {
          for (const child of container.children) {
            findTexts(child);
          }
        }
      };
      findTexts(app.stage);
      return texts;
    });

    expect(texts).toContain('Menu');
  });

  test('clicking Menu button shows pause menu', async ({ page }, testInfo) => {
    const canvas = await page.locator('canvas');
    const boundingBox = await canvas.boundingBox();
    if (!boundingBox) throw new Error('Canvas has no bounding box');

    const centerX = boundingBox.x + boundingBox.width / 2;

    // Navigate to game screen
    await clickText(page, 'Start Game', testInfo, 'click-start-game');
    await page.waitForTimeout(1000);

    // Click Menu button
    await clickText(page, 'Menu', testInfo, 'click-menu');
    await page.waitForTimeout(500);
    const state = await page.evaluate(() => (window as any).GAMESTATE);
    expect(state?.gss_screen).toBe('PauseScreen');

    // Check that pause menu is shown
    const texts = await page.evaluate(() => {
      const app = (window as any).__PIXI_APP__;
      if (!app?.stage?.children) return [];

      const texts: string[] = [];
      const findTexts = (container: any) => {
        if (container.text) {
          texts.push(container.text);
        }
        if (container.children) {
          for (const child of container.children) {
            findTexts(child);
          }
        }
      };
      findTexts(app.stage);
      return texts;
    });

    console.log('Pause menu texts:', texts);

    expect(texts).toContain('Paused');
    expect(texts).toContain('Continue');
    expect(texts).toContain('Quit');
    expect(texts).not.toContain('Framerate:');
  });

  test('clicking Continue returns to game', async ({ page }, testInfo) => {
    const canvas = await page.locator('canvas');
    const boundingBox = await canvas.boundingBox();
    if (!boundingBox) throw new Error('Canvas has no bounding box');

    const centerX = boundingBox.x + boundingBox.width / 2;

    // Navigate to game screen
    await clickText(page, 'Start Game', testInfo, 'click-start-game');
    await page.waitForTimeout(1000);

    // Click Menu button
    await clickText(page, 'Menu', testInfo, 'click-menu');
    await page.waitForTimeout(500);

    // Click Continue (first menu item, pause menu is at screen_height/2)
    await clickText(page, 'Continue', testInfo, 'click-continue');
    await page.waitForTimeout(1000);
    const state = await page.evaluate(() => (window as any).GAMESTATE);
    expect(state?.gss_screen).toBe('GameScreen');

    // Check that we're back on the game screen
    const texts = await page.evaluate(() => {
      const app = (window as any).__PIXI_APP__;
      if (!app?.stage?.children) return [];

      const texts: string[] = [];
      const findTexts = (container: any) => {
        if (container.text) {
          texts.push(container.text);
        }
        if (container.children) {
          for (const child of container.children) {
            findTexts(child);
          }
        }
      };
      findTexts(app.stage);
      return texts;
    });

    console.log('After Continue texts:', texts);

    expect(texts).toContain('Roll');
    expect(texts).toContain('Menu');
    expect(texts).not.toContain('Paused');
  });

  test('clicking Quit returns to start screen and resets game', async ({ page }, testInfo) => {
    const canvas = await page.locator('canvas');
    const boundingBox = await canvas.boundingBox();
    if (!boundingBox) throw new Error('Canvas has no bounding box');

    const centerX = boundingBox.x + boundingBox.width / 2;

    // Navigate to game screen
    await clickText(page, 'Start Game', testInfo, 'click-start-game');
    await page.waitForTimeout(2000); // Wait for game screen and hand to render

    // Check GAMESTATE to confirm we're on the game screen
    let state = await page.evaluate(() => (window as any).GAMESTATE);
    expect(state?.gss_screen).toBe('GameScreen');

    // Helper to dismiss round complete dialogs and return to game screen
    const dismissRoundCompleteIfNeeded = async () => {
      const getScreenTexts = async () => {
        return await page.evaluate(() => {
          const app = (window as any).__PIXI_APP__;
          if (!app?.stage?.children) return [];
          const texts: string[] = [];
          const findTexts = (container: any) => {
            if (container.text) texts.push(container.text);
            if (container.children) for (const child of container.children) findTexts(child);
          };
          findTexts(app.stage);
          return texts;
        });
      };

      // Check for round complete dialog
      let screenTexts = await getScreenTexts();
      if (screenTexts.some(t => t.includes('Complete!'))) {
        await clickText(page, 'Skip', testInfo, 'click-skip');
        await page.waitForTimeout(500);
        // Re-check screen texts after clicking Skip
        screenTexts = await getScreenTexts();
      }

      // If in shop, click Next Round to return to game
      if (screenTexts.some(t => t === 'SHOP')) {
        await clickText(page, 'Next Round', testInfo, 'click-next-round');
        await page.waitForTimeout(1000);
      }
    };

    // Select 1 die and roll to change score - use 1 to minimize chance of completing round
    await selectDiceAndRoll(page, boundingBox, 1);

    // Dismiss any round complete dialogs
    await dismissRoundCompleteIfNeeded();

    // Now we should be on game screen - click Menu button
    await clickText(page, 'Menu', testInfo, 'click-menu');
    await page.waitForTimeout(500);
    await waitForText(page, 'Quit', 4000);

    // Click Quit (pause menu)
    await clickText(page, 'Quit', testInfo, 'click-quit');
    await page.waitForTimeout(500);
    const stateAfterQuit = await page.evaluate(() => (window as any).GAMESTATE);
    expect(stateAfterQuit?.gss_screen).toBe('StartScreen');
    expect(stateAfterQuit?.gss_score).toBe(0);

    // Check that we're back on the start screen
    const texts = await page.evaluate(() => {
      const app = (window as any).__PIXI_APP__;
      if (!app?.stage?.children) return [];

      const texts: string[] = [];
      const findTexts = (container: any) => {
        if (container.text) {
          texts.push(container.text);
        }
        if (container.children) {
          for (const child of container.children) {
            findTexts(child);
          }
        }
      };
      findTexts(app.stage);
      return texts;
    });

    console.log('After Quit texts:', texts);

    expect(texts).toContain('A Game of Chance');
    expect(texts).toContain('Start Game');

    // Now start a new game and verify score is reset
    await clickText(page, 'Start Game', testInfo, 'click-restart');
    await page.waitForTimeout(1000);
    const stateAfterRestart = await page.evaluate(() => (window as any).GAMESTATE);
    expect(stateAfterRestart?.gss_screen).toBe('GameScreen');

    const gameTexts = await page.evaluate(() => {
      const app = (window as any).__PIXI_APP__;
      if (!app?.stage?.children) return [];

      const texts: string[] = [];
      const findTexts = (container: any) => {
        if (container.text) {
          texts.push(container.text);
        }
        if (container.children) {
          for (const child of container.children) {
            findTexts(child);
          }
        }
      };
      findTexts(app.stage);
      return texts;
    });

    console.log('New game texts:', gameTexts);

    // Score should be reset to 0
    expect(gameTexts.some(t => t === 'Score: 0')).toBe(true);
  });
});
