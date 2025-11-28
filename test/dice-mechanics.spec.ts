import { test, expect } from '@playwright/test';

test.describe('Dice Mechanics', () => {
  // Helper function to navigate to game screen
  async function navigateToGame(page: any) {
    await page.goto('/');
    const canvas = await page.waitForSelector('canvas', { timeout: 8000 });
    await page.waitForTimeout(5000); // Wait for spritesheet

    const boundingBox = await canvas.boundingBox();
    if (!boundingBox) throw new Error('Canvas has no bounding box');

    const centerX = boundingBox.x + boundingBox.width / 2;
    const menuY = boundingBox.y + boundingBox.height / 2;
    await page.mouse.click(centerX, menuY);
    await page.waitForTimeout(1500);

    return { canvas, boundingBox };
  }

  test('displays dice count showing additive and multiplicative dice', async ({ page }) => {
    const { canvas, boundingBox } = await navigateToGame(page);

    // Check for dice count text
    const diceCountText = await page.evaluate(() => {
      const app = (window as any).__PIXI_APP__;
      if (!app?.stage?.children) return null;

      const findDiceCount = (container: any): string | null => {
        if (container.text && container.text.includes('additive')) {
          return container.text;
        }
        if (container.children) {
          for (const child of container.children) {
            const result = findDiceCount(child);
            if (result) return result;
          }
        }
        return null;
      };
      return findDiceCount(app.stage);
    });

    console.log('Dice count text:', diceCountText);
    expect(diceCountText).toContain('additive');
    expect(diceCountText).toContain('multiply');
    // Should start with 1 additive, 0 multiplicative
    expect(diceCountText).toMatch(/1 additive/);
    expect(diceCountText).toMatch(/0 multiply/);
  });

  test('dice persist on screen after animation completes', async ({ page }) => {
    const { canvas, boundingBox } = await navigateToGame(page);

    // Click roll
    const clickX = boundingBox.x + boundingBox.width / 2;
    const clickY = boundingBox.y + boundingBox.height - 150;
    await page.mouse.click(clickX, clickY);

    // Wait for animation to complete
    await page.waitForTimeout(3000);

    // Check that there are AnimatedSprites still on screen
    const spriteCount = await page.evaluate(() => {
      const app = (window as any).__PIXI_APP__;
      if (!app?.stage?.children) return 0;

      let count = 0;
      const countSprites = (container: any) => {
        // AnimatedSprites have textures array
        if (container._textures && container._textures.length > 0) {
          count++;
        }
        if (container.children) {
          for (const child of container.children) {
            countSprites(child);
          }
        }
      };
      countSprites(app.stage);
      return count;
    });

    console.log('Sprite count after roll:', spriteCount);
    expect(spriteCount).toBeGreaterThanOrEqual(1);
  });

  test('choice dialog appears when target reached', async ({ page }) => {
    test.setTimeout(60000); // 60 second timeout
    const { canvas, boundingBox } = await navigateToGame(page);

    const clickX = boundingBox.x + boundingBox.width / 2;
    const clickY = boundingBox.y + boundingBox.height - 150;

    // Keep rolling until score reaches target or we hit a limit
    let dialogFound = false;
    for (let i = 0; i < 20 && !dialogFound; i++) {
      await page.mouse.click(clickX, clickY);
      await page.waitForTimeout(3000);

      // Check for choice dialog
      dialogFound = await page.evaluate(() => {
        const app = (window as any).__PIXI_APP__;
        if (!app?.stage?.children) return false;

        const findDialogText = (container: any): boolean => {
          if (container.text && container.text.includes('Target Reached')) {
            return true;
          }
          if (container.children) {
            for (const child of container.children) {
              if (findDialogText(child)) return true;
            }
          }
          return false;
        };
        return findDialogText(app.stage);
      });

      if (dialogFound) {
        console.log('Choice dialog found after', i + 1, 'rolls');
        break;
      }
    }

    expect(dialogFound).toBe(true);
  });

  test('clicking additive button in choice dialog increases additive dice', async ({ page }) => {
    test.setTimeout(60000);
    const { canvas, boundingBox } = await navigateToGame(page);

    const clickX = boundingBox.x + boundingBox.width / 2;
    const clickY = boundingBox.y + boundingBox.height - 150;

    // Keep rolling until dialog appears
    let dialogFound = false;
    for (let i = 0; i < 20 && !dialogFound; i++) {
      await page.mouse.click(clickX, clickY);
      await page.waitForTimeout(3000);

      dialogFound = await page.evaluate(() => {
        const app = (window as any).__PIXI_APP__;
        if (!app?.stage?.children) return false;

        const findDialogText = (container: any): boolean => {
          if (container.text && container.text.includes('Target Reached')) {
            return true;
          }
          if (container.children) {
            for (const child of container.children) {
              if (findDialogText(child)) return true;
            }
          }
          return false;
        };
        return findDialogText(app.stage);
      });
    }

    expect(dialogFound).toBe(true);

    // Click the additive button (left button in dialog)
    const dialogLeftX = boundingBox.x + boundingBox.width / 2 - 95; // Left button position
    const dialogButtonY = boundingBox.y + boundingBox.height / 2 + 30; // Button Y position
    await page.mouse.click(dialogLeftX, dialogButtonY);
    await page.waitForTimeout(500);

    // Check dice count updated
    const diceCountText = await page.evaluate(() => {
      const app = (window as any).__PIXI_APP__;
      if (!app?.stage?.children) return null;

      const findDiceCount = (container: any): string | null => {
        if (container.text && container.text.includes('additive')) {
          return container.text;
        }
        if (container.children) {
          for (const child of container.children) {
            const result = findDiceCount(child);
            if (result) return result;
          }
        }
        return null;
      };
      return findDiceCount(app.stage);
    });

    console.log('Dice count after additive choice:', diceCountText);
    expect(diceCountText).toMatch(/2 additive/);
  });
});
