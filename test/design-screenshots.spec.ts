import { test, expect } from './fixtures';
import { navigateToGame, selectDiceAndRoll, isChoiceDialogVisible, getJokerCount } from './test-helpers';

test.describe('Design Screenshots', () => {
  test('capture game UI screenshots for design review', async ({ page }) => {
    test.setTimeout(180000); // 3 minute timeout

    // Create screenshots directory reference
    const screenshotDir = 'test-results/design-screenshots';

    // 1. Start screen
    await page.goto('/');
    const canvas = await page.waitForSelector('canvas', { timeout: 8000 });
    expect(canvas).toBeTruthy();
    await page.waitForTimeout(5000); // Wait for spritesheet generation

    await page.screenshot({
      path: `${screenshotDir}/01-start-screen.png`,
      fullPage: true
    });
    console.log('Captured: Start screen');

    // 2. Navigate to game screen
    const boundingBox = await canvas!.boundingBox();
    if (!boundingBox) throw new Error('Canvas has no bounding box');

    const centerX = boundingBox.x + boundingBox.width / 2;
    // Menu items start at screen_height/2 + 20 = 380, spacing 70
    // "Start Game" is the first item
    const startGameY = boundingBox.y + 380;
    await page.mouse.click(centerX, startGameY);
    await page.waitForTimeout(2000);

    await page.screenshot({
      path: `${screenshotDir}/02-game-screen-initial.png`,
      fullPage: true
    });
    console.log('Captured: Initial game screen');

    // 3. Hover over first joker slot (top left area)
    // Joker slots are at y around 20, starting at x around 370
    const jokerSlotX = boundingBox.x + 370;
    const jokerSlotY = boundingBox.y + 20;
    await page.mouse.move(jokerSlotX, jokerSlotY);
    await page.waitForTimeout(500);

    await page.screenshot({
      path: `${screenshotDir}/03-joker-hover.png`,
      fullPage: true
    });
    console.log('Captured: Joker slot hover');

    // 4. Hover over second joker slot
    await page.mouse.move(jokerSlotX + 110, jokerSlotY);
    await page.waitForTimeout(500);

    await page.screenshot({
      path: `${screenshotDir}/04-joker-hover-2.png`,
      fullPage: true
    });
    console.log('Captured: Second joker slot hover');

    // 5. Find and hover over dice in hand
    const dicePositions = await page.evaluate(() => {
      const app = (window as any).__PIXI_APP__;
      if (!app?.stage?.children) return [];

      const positions: {x: number, y: number}[] = [];
      const findHandDice = (container: any): void => {
        if (container.eventMode === 'static' && container.cursor === 'pointer') {
          const globalPos = container.getGlobalPosition ? container.getGlobalPosition() : null;
          // Dice are at y≈440, buttons are at y>550 - filter to hand area only
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

    // Hover over first die (additive)
    if (dicePositions.length > 0) {
      const firstDie = dicePositions[0];
      await page.mouse.move(boundingBox.x + firstDie.x, boundingBox.y + firstDie.y);
      await page.waitForTimeout(500);

      await page.screenshot({
        path: `${screenshotDir}/05-die-hover-first.png`,
        fullPage: true
      });
      console.log('Captured: First die hover');
    }

    // Hover over a different die (try to find one of different type)
    if (dicePositions.length > 4) {
      const laterDie = dicePositions[5];
      await page.mouse.move(boundingBox.x + laterDie.x, boundingBox.y + laterDie.y);
      await page.waitForTimeout(500);

      await page.screenshot({
        path: `${screenshotDir}/06-die-hover-second.png`,
        fullPage: true
      });
      console.log('Captured: Second die hover (different type)');
    }

    // 6. Select 5 dice (for higher scoring potential)
    // Click on 5 dice to select them
    for (let i = 0; i < 5 && i < dicePositions.length; i++) {
      const pos = dicePositions[i];
      await page.mouse.click(boundingBox.x + pos.x, boundingBox.y + pos.y);
      await page.waitForTimeout(200);
    }

    await page.screenshot({
      path: `${screenshotDir}/07-dice-selected.png`,
      fullPage: true
    });
    console.log('Captured: Dice selected');

    // 7. Click roll button
    // Action area is at screen_height - 115, Roll button there
    const rollX = boundingBox.x + boundingBox.width / 2;
    const rollY = boundingBox.y + boundingBox.height - 115;
    await page.mouse.click(rollX, rollY);

    // Capture mid-animation
    await page.waitForTimeout(500);
    await page.screenshot({
      path: `${screenshotDir}/08-rolling-animation.png`,
      fullPage: true
    });
    console.log('Captured: Rolling animation');

    // Wait for animation to complete
    await page.waitForTimeout(2500);

    await page.screenshot({
      path: `${screenshotDir}/09-after-roll.png`,
      fullPage: true
    });
    console.log('Captured: After roll');

    // 8. Roll again until we complete a round or run out of rolls
    let roundComplete = false;
    for (let i = 0; i < 8 && !roundComplete; i++) {
      await selectDiceAndRoll(page, boundingBox, 5);
      await page.waitForTimeout(500);
      roundComplete = await isChoiceDialogVisible(page);
    }

    if (roundComplete) {
      await page.screenshot({
        path: `${screenshotDir}/10-round-complete-die-reward.png`,
        fullPage: true
      });
      console.log('Captured: Round complete - die reward screen');

      // Click Skip to go to shop
      // Panel is at panelY=50, panelH=340, Skip is at panelY + panelH - 60 = 330
      const skipButtonY = boundingBox.y + 330;
      await page.mouse.click(centerX, skipButtonY);
      await page.waitForTimeout(1000);

      await page.screenshot({
        path: `${screenshotDir}/11-shop.png`,
        fullPage: true
      });
      console.log('Captured: Shop screen');

      // Check initial joker count (should be 2 - Blue Chip and Twin Engines)
      const initialJokerCount = await getJokerCount(page);
      console.log('Initial joker count:', initialJokerCount);

      // Hover over first joker in shop
      // Shop panel: panelY=170 (screen_height-panelH)/2, joker boxes at boxY = panelY + 95 = 265
      const shopJokerX = boundingBox.x + boundingBox.width / 2 - 120;
      const shopJokerY = boundingBox.y + 280;
      await page.mouse.move(shopJokerX, shopJokerY);
      await page.waitForTimeout(500);

      await page.screenshot({
        path: `${screenshotDir}/12-shop-joker-hover.png`,
        fullPage: true
      });
      console.log('Captured: Shop joker hover');

      // Click the Buy button for first joker
      // Buy button is at boxY + boxH + 8 = 265 + 110 + 8 = 383
      const buyButtonX = shopJokerX;
      const buyButtonY = boundingBox.y + 383;
      await page.mouse.click(buyButtonX, buyButtonY);
      await page.waitForTimeout(500);

      await page.screenshot({
        path: `${screenshotDir}/12b-after-buy-joker.png`,
        fullPage: true
      });
      console.log('Captured: After buying joker');
    }

    // 9. Open pause menu
    // Menu button is at actionAreaY + 60 = screen_height - 115 + 60 = screen_height - 55
    const menuButtonY = boundingBox.y + boundingBox.height - 55;

    // First need to exit shop if we're in it
    if (roundComplete) {
      // Next Round button is at panelY + panelH - 55 = 170 + 380 - 55 = 495
      const nextRoundButtonY = boundingBox.y + 495;
      await page.mouse.click(centerX, nextRoundButtonY);
      await page.waitForTimeout(1000);

      // Capture after exiting shop to verify joker was added
      await page.screenshot({
        path: `${screenshotDir}/12c-joker-slots-after-purchase.png`,
        fullPage: true
      });

      // Verify joker count increased
      const newJokerCount = await getJokerCount(page);
      console.log('Joker count after purchase:', newJokerCount);
      console.log('Captured: Joker slots after purchase');
    }

    await page.mouse.click(centerX, menuButtonY);
    await page.waitForTimeout(500);

    await page.screenshot({
      path: `${screenshotDir}/13-pause-menu.png`,
      fullPage: true
    });
    console.log('Captured: Pause menu');

    // 10. Go back to game and capture final state
    // Pause menu: Continue is at menu_y = screen_height/2 = 360
    const pauseContinueY = boundingBox.y + 360;
    await page.mouse.click(centerX, pauseContinueY); // Click Continue
    await page.waitForTimeout(500);

    await page.screenshot({
      path: `${screenshotDir}/14-final-game-state.png`,
      fullPage: true
    });
    console.log('Captured: Final game state');

    // 11. Options screen
    await page.mouse.click(centerX, menuButtonY); // Open pause menu
    await page.waitForTimeout(500);
    // Quit is at menu_y + spacing = 360 + 70 = 430
    const quitY = boundingBox.y + 430;
    await page.mouse.click(centerX, quitY); // Click Quit
    await page.waitForTimeout(500);

    // Now on start screen, click Options
    // Options is at 380 + 70 = 450
    const optionsY = boundingBox.y + 450;
    await page.mouse.click(centerX, optionsY);
    await page.waitForTimeout(500);

    await page.screenshot({
      path: `${screenshotDir}/15-options-screen.png`,
      fullPage: true
    });
    console.log('Captured: Options screen');

    console.log('\n=== All design screenshots captured! ===');
    console.log(`Screenshots saved to: ${screenshotDir}/`);
  });
});
