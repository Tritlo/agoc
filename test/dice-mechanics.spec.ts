import { test, expect } from './fixtures';
import { navigateToGame, selectDiceAndRoll, getEntropyText, getDeckText, getSelectionText, isChoiceDialogVisible, isGameOverVisible, getAllTexts, clickText } from './test-helpers';

test.describe('Dice Mechanics', () => {
  test('displays entropy and deck counts', async ({ page }, testInfo) => {
    const { canvas, boundingBox } = await navigateToGame(page, testInfo);

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

  test('hand is populated with dice to select', async ({ page }, testInfo) => {
    const { canvas, boundingBox } = await navigateToGame(page, testInfo);

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

  test('dice persist on screen after animation completes', async ({ page }, testInfo) => {
    const { canvas, boundingBox } = await navigateToGame(page, testInfo);

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

  test('clicking dice in hand selects them', async ({ page }, testInfo) => {
    const { canvas, boundingBox } = await navigateToGame(page, testInfo);

    // Find dice positions (filter to only hand area at bottom of screen)
    const dicePositions = await page.evaluate(() => {
      const app = (window as any).__PIXI_APP__;
      if (!app?.stage?.children) return [];

      const positions: {x: number, y: number}[] = [];
      const findHandDice = (container: any): void => {
        if (container.eventMode === 'static' && container.cursor === 'pointer') {
          const globalPos = container.getGlobalPosition ? container.getGlobalPosition() : null;
          // Filter to hand area (approx bottom strip)
          if (globalPos && globalPos.y > 350 && globalPos.y < 550) {
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
    await page.mouse.click(firstDie.x, firstDie.y);
    await page.waitForTimeout(200);

    // Check selection text
    const selectionText = await getSelectionText(page);
    console.log('Selection text after click:', selectionText);
    expect(selectionText).toContain('Selected:');
    expect(selectionText).toMatch(/Selected: 1/);

    const state = await page.evaluate(() => (window as any).GAMESTATE);
    const sel = state?.gss_selection?.ss_selected;
    const selCount = Array.isArray(sel) ? sel.length : (sel ? Object.keys(sel).length : 0);
    expect(selCount).toBeGreaterThanOrEqual(1);
  });

  test('blind complete or game over appears after rolling', async ({ page }) => {
    test.setTimeout(120000); // 2 minute timeout
    const { canvas, boundingBox } = await navigateToGame(page);

    // Keep rolling until blind complete or game over (max 4 rolls per blind)
    let blindComplete = false;
    let gameOver = false;
    for (let i = 0; i < 20 && !blindComplete && !gameOver; i++) {
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
    for (let i = 0; i < 20 && !blindComplete && !gameOver; i++) {
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

    // When round completes, entropy is awarded (was 4 starting, now should be > 4)
    const entropyOnComplete = await getEntropyText(page);
    console.log('Entropy when blind complete:', entropyOnComplete);
    const entropyVal = entropyOnComplete ? parseInt(entropyOnComplete.replace(/\D+/g, ''), 10) : 0;
    // Entropy should have increased from starting value of 4 (round 1 reward is 4)
    expect(entropyVal).toBeGreaterThan(4);

    const centerX = boundingBox.x + boundingBox.width / 2;

    // Die choice screen appears first - click Skip to go to shop
    const dieChoiceTexts = await getAllTexts(page);
    console.log('Die choice screen texts:', dieChoiceTexts);
    expect(dieChoiceTexts.some(t => t.includes('Complete!'))).toBe(true);
    expect(dieChoiceTexts.some(t => t === 'Skip')).toBe(true);

    // Click Skip button (uses text-based click)
    await clickText(page, 'Skip');
    await page.waitForTimeout(500);

    // Verify shop is showing
    const shopTexts = await getAllTexts(page);
    console.log('Shop texts:', shopTexts);
    expect(shopTexts.some(t => t === 'SHOP')).toBe(true);
    expect(shopTexts.some(t => t === 'Next Round')).toBe(true);

    // Click Next Round to go to next round
    await clickText(page, 'Next Round');
    await page.waitForTimeout(1000);

    // Check that we progressed to next round by verifying:
    // - Score reset to 0
    // - Target increased (round 2 target is 30)
    const allTexts = await getAllTexts(page);
    console.log('All texts after shop:', allTexts);
    expect(allTexts.some(t => t === 'Score: 0')).toBe(true);
    expect(allTexts.some(t => t === 'Target: 30')).toBe(true); // Round 2 target
  });
});
