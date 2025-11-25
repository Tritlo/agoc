import { test, expect } from '@playwright/test';

test.describe('Menu System', () => {
  test.beforeEach(async ({ page }) => {
    // Navigate to the app
    await page.goto('/');

    // Wait for WASM to initialize (canvas should be added to the page)
    const canvas = await page.waitForSelector('canvas', { timeout: 10000 });
    expect(canvas).toBeTruthy();

    // Wait for the app to fully initialize
    await page.waitForTimeout(1000);
  });

  test('start screen displays menu items', async ({ page }) => {
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

  test('clicking Options navigates to options screen', async ({ page }) => {
    const canvas = await page.locator('canvas');
    const boundingBox = await canvas.boundingBox();
    if (!boundingBox) throw new Error('Canvas has no bounding box');

    // Options is the second menu item (index 1), positioned at center + 60px spacing
    const centerX = boundingBox.x + boundingBox.width / 2;
    const menuStartY = boundingBox.y + boundingBox.height / 2;
    const optionsY = menuStartY + 60; // Second item with 60px spacing

    await page.mouse.click(centerX, optionsY);
    await page.waitForTimeout(500);

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
    expect(texts).toContain('Back');
    expect(texts).not.toContain('Start Game');
  });

  test('clicking Back from Options returns to start screen', async ({ page }) => {
    const canvas = await page.locator('canvas');
    const boundingBox = await canvas.boundingBox();
    if (!boundingBox) throw new Error('Canvas has no bounding box');

    const centerX = boundingBox.x + boundingBox.width / 2;
    const menuStartY = boundingBox.y + boundingBox.height / 2;

    // First, go to Options (second menu item)
    await page.mouse.click(centerX, menuStartY + 60);
    await page.waitForTimeout(500);

    // Now click Back (at menu_y + 50 from options screen)
    const backY = boundingBox.y + boundingBox.height / 2 + 50;
    await page.mouse.click(centerX, backY);
    await page.waitForTimeout(500);

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

  test('clicking Start Game navigates to game screen', async ({ page }) => {
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

    // Game screen should have score, target, distribution, and sample button
    expect(texts.some(t => t.startsWith('Score:'))).toBe(true);
    expect(texts.some(t => t.startsWith('Target:'))).toBe(true);
    expect(texts.some(t => t.includes('𝒩'))).toBe(true); // Normal distribution
    expect(texts).toContain('Sample');

    // Should not have start screen items
    expect(texts).not.toContain('A Game of Chance');
    expect(texts).not.toContain('Start Game');
  });

  test('game screen has Menu button', async ({ page }) => {
    const canvas = await page.locator('canvas');
    const boundingBox = await canvas.boundingBox();
    if (!boundingBox) throw new Error('Canvas has no bounding box');

    // Navigate to game screen
    const centerX = boundingBox.x + boundingBox.width / 2;
    const menuStartY = boundingBox.y + boundingBox.height / 2;
    await page.mouse.click(centerX, menuStartY);
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

  test('clicking Menu button shows pause menu', async ({ page }) => {
    const canvas = await page.locator('canvas');
    const boundingBox = await canvas.boundingBox();
    if (!boundingBox) throw new Error('Canvas has no bounding box');

    const centerX = boundingBox.x + boundingBox.width / 2;
    const menuStartY = boundingBox.y + boundingBox.height / 2;

    // Navigate to game screen
    await page.mouse.click(centerX, menuStartY);
    await page.waitForTimeout(1000);

    // Click Menu button (at height - 80)
    const menuButtonY = boundingBox.y + boundingBox.height - 80;
    await page.mouse.click(centerX, menuButtonY);
    await page.waitForTimeout(500);

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
  });

  test('clicking Continue returns to game', async ({ page }) => {
    const canvas = await page.locator('canvas');
    const boundingBox = await canvas.boundingBox();
    if (!boundingBox) throw new Error('Canvas has no bounding box');

    const centerX = boundingBox.x + boundingBox.width / 2;
    const menuStartY = boundingBox.y + boundingBox.height / 2;

    // Navigate to game screen
    await page.mouse.click(centerX, menuStartY);
    await page.waitForTimeout(1000);

    // Click Menu button
    const menuButtonY = boundingBox.y + boundingBox.height - 80;
    await page.mouse.click(centerX, menuButtonY);
    await page.waitForTimeout(500);

    // Click Continue (first menu item at center)
    await page.mouse.click(centerX, menuStartY);
    await page.waitForTimeout(1000);

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

    expect(texts).toContain('Sample');
    expect(texts).toContain('Menu');
    expect(texts).not.toContain('Paused');
  });

  test('clicking Quit returns to start screen and resets game', async ({ page }) => {
    const canvas = await page.locator('canvas');
    const boundingBox = await canvas.boundingBox();
    if (!boundingBox) throw new Error('Canvas has no bounding box');

    const centerX = boundingBox.x + boundingBox.width / 2;
    const menuStartY = boundingBox.y + boundingBox.height / 2;

    // Navigate to game screen
    await page.mouse.click(centerX, menuStartY);
    await page.waitForTimeout(1000);

    // Click Sample button to change score
    const sampleButtonY = boundingBox.y + boundingBox.height - 150;
    await page.mouse.click(centerX, sampleButtonY);
    await page.waitForTimeout(500);

    // Click Menu button
    const menuButtonY = boundingBox.y + boundingBox.height - 80;
    await page.mouse.click(centerX, menuButtonY);
    await page.waitForTimeout(500);

    // Click Quit (second menu item, 60px below Continue)
    const quitY = menuStartY + 60;
    await page.mouse.click(centerX, quitY);
    await page.waitForTimeout(500);

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
    await page.mouse.click(centerX, menuStartY);
    await page.waitForTimeout(1000);

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
