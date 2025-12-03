import { test, expect } from '@playwright/test';
import { navigateToGame, selectDiceAndRoll, getEntropyText, getDeckText, getSelectionText, isChoiceDialogVisible, isGameOverVisible, getAllTexts } from './test-helpers';

test.describe('Dice Mechanics', () => {
  test('displays entropy and deck counts', async ({ page }) => {
    const { canvas, boundingBox } = await navigateToGame(page);

    // Check for entropy text
    const entropyText = await getEntropyText(page);
    console.log('Entropy text:', entropyText);
    expect(entropyText).toContain('Entropy:');
    expect(entropyText).toMatch(/Entropy: 4/); // Starts at 4 (currency)

    // Check for deck counts
    const deckText = await getDeckText(page);
    console.log('Deck text:', deckText);
    expect(deckText).toContain('Bag:');
    expect(deckText).toContain('Discard:');
  });

  test('hand is populated with dice to select', async ({ page }) => {
    const { canvas, boundingBox } = await navigateToGame(page);

    // Check that hand dice are present by looking for clickable dice containers
    const diceCount = await page.evaluate(() => {
      const app = (window as any).__PIXI_APP__;
      if (!app?.stage?.children) return 0;

      let count = 0;
      const findClickableDice = (container: any) => {
        // Die containers have eventMode 'static' and cursor 'pointer'
        if (container.eventMode === 'static' && container.cursor === 'pointer') {
          count++;
        }
        if (container.children) {
          for (const child of container.children) {
            findClickableDice(child);
          }
        }
      };
      findClickableDice(app.stage);
      return count;
    });

    console.log('Clickable dice count:', diceCount);
    // Should have 8 dice in hand (fixed draw of 8)
    expect(diceCount).toBeGreaterThanOrEqual(8);
  });

  test('dice persist on screen after animation completes', async ({ page }) => {
    const { canvas, boundingBox } = await navigateToGame(page);

    // Select dice and roll
    await selectDiceAndRoll(page, boundingBox, 2);

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

  test('clicking dice in hand selects them', async ({ page }) => {
    const { canvas, boundingBox } = await navigateToGame(page);

    // Find dice positions (filter to only hand area at bottom of screen)
    const dicePositions = await page.evaluate(() => {
      const app = (window as any).__PIXI_APP__;
      if (!app?.stage?.children) return [];

      const positions: {x: number, y: number}[] = [];
      const findHandDice = (container: any): void => {
        if (container.eventMode === 'static' && container.cursor === 'pointer') {
          const globalPos = container.getGlobalPosition ? container.getGlobalPosition() : null;
          // Filter to only hand area (y > 300)
          if (globalPos && globalPos.y > 300) {
            positions.push({ x: globalPos.x, y: globalPos.y });
          }
        }
        if (container.children) {
          for (const child of container.children) {
            findHandDice(child);
          }
        }
      };
      findHandDice(app.stage);
      return positions;
    });

    console.log('Found dice positions:', dicePositions);
    expect(dicePositions.length).toBeGreaterThan(0);

    // Click on first die
    const firstDie = dicePositions[0];
    await page.mouse.click(boundingBox.x + firstDie.x, boundingBox.y + firstDie.y);
    await page.waitForTimeout(200);

    // Check selection text
    const selectionText = await getSelectionText(page);
    console.log('Selection text after click:', selectionText);
    expect(selectionText).toContain('Selected:');
    expect(selectionText).toMatch(/Selected: 1/);
  });

  test('blind complete or game over appears after rolling', async ({ page }) => {
    test.setTimeout(120000); // 2 minute timeout
    const { canvas, boundingBox } = await navigateToGame(page);

    // Keep rolling until blind complete or game over (max 4 rolls per blind)
    let blindComplete = false;
    let gameOver = false;
    for (let i = 0; i < 10 && !blindComplete && !gameOver; i++) {
      // Select 5 dice (max) to maximize score chance
      await selectDiceAndRoll(page, boundingBox, 5);
      await page.waitForTimeout(500);

      blindComplete = await isChoiceDialogVisible(page);
      gameOver = await isGameOverVisible(page);

      if (blindComplete) {
        console.log('Blind complete dialog found after', i + 1, 'rolls');
        break;
      }
      if (gameOver) {
        console.log('Game over dialog found after', i + 1, 'rolls');
        break;
      }
    }

    // Either blind complete or game over should appear
    expect(blindComplete || gameOver).toBe(true);
  });

  test('completing blind awards entropy and progresses to next blind', async ({ page }) => {
    test.setTimeout(120000);
    const { canvas, boundingBox } = await navigateToGame(page);

    // Keep rolling until blind complete (max 4 rolls per blind, select 5 dice for max score)
    let blindComplete = false;
    let gameOver = false;
    for (let i = 0; i < 10 && !blindComplete && !gameOver; i++) {
      await selectDiceAndRoll(page, boundingBox, 5);
      await page.waitForTimeout(500);

      blindComplete = await isChoiceDialogVisible(page);
      gameOver = await isGameOverVisible(page);
    }

    // Skip test if game over (score wasn't high enough)
    if (gameOver) {
      console.log('Game over - skipping entropy progression test');
      test.skip();
      return;
    }

    expect(blindComplete).toBe(true);

    // Get entropy before clicking Skip on die reward screen
    const entropyBefore = await getEntropyText(page);
    console.log('Entropy when blind complete:', entropyBefore);

    const centerX = boundingBox.x + boundingBox.width / 2;

    // New flow: Die reward screen appears first - click Skip button at bottom
    // Skip button: skipY = 280 + 100 + 40 = 420, center = 420 + 20 = 440
    // Relative to screen center (360): 440 - 360 = 80
    const skipButtonY = boundingBox.y + boundingBox.height / 2 + 80;
    await page.mouse.click(centerX, skipButtonY);
    await page.waitForTimeout(500);

    // Check entropy increased (Small Blind reward is +3, so 4 + 3 = 7)
    const entropyAfter = await getEntropyText(page);
    console.log('Entropy after die reward:', entropyAfter);
    expect(entropyAfter).toMatch(/Entropy: 7/);

    // Verify shop is showing
    const shopTexts = await getAllTexts(page);
    console.log('Shop texts:', shopTexts);
    expect(shopTexts.some(t => t === 'SHOP')).toBe(true);
    expect(shopTexts.some(t => t === 'Next Blind')).toBe(true);

    // Click Next Blind button - it's at the bottom of the shop panel
    // panelY = (720-350)/2 = 185, button Y = 185 + 350 - 50 = 485, center = 485 + 20 = 505
    // Relative to screen center (360): 505 - 360 = 145
    const nextBlindButtonY = boundingBox.y + boundingBox.height / 2 + 145;
    await page.mouse.click(centerX, nextBlindButtonY);
    await page.waitForTimeout(1000);

    // Check that we progressed to next blind by verifying:
    // - Score reset to 0
    // - Target increased to Big Blind target (30 for Ante 1)
    const allTexts = await getAllTexts(page);
    console.log('All texts after shop:', allTexts);
    expect(allTexts.some(t => t === 'Score: 0')).toBe(true);
    expect(allTexts.some(t => t === 'Target: 30')).toBe(true); // Big Blind target
  });
});
