import { Page } from '@playwright/test';

// Helper to navigate to game screen
export async function navigateToGame(page: Page) {
  await page.goto('/');
  const canvas = await page.waitForSelector('canvas', { timeout: 8000 });
  await page.waitForTimeout(5000); // Wait for spritesheet

  const boundingBox = await canvas!.boundingBox();
  if (!boundingBox) throw new Error('Canvas has no bounding box');

  const centerX = boundingBox.x + boundingBox.width / 2;
  const menuY = boundingBox.y + boundingBox.height / 2;
  await page.mouse.click(centerX, menuY);
  await page.waitForTimeout(2000); // Wait for game screen and hand to render

  return { canvas, boundingBox };
}

// Helper to select dice in hand and roll
export async function selectDiceAndRoll(page: Page, boundingBox: { x: number, y: number, width: number, height: number }, numDice: number = 2) {
  // The hand is displayed at y = screenHeight - 280, with dice spaced 110px apart, centered
  const handY = boundingBox.y + boundingBox.height - 280;
  const centerX = boundingBox.x + boundingBox.width / 2;

  // Find actual hand dice positions from the game
  const dicePositions = await page.evaluate(() => {
    const app = (window as any).__PIXI_APP__;
    if (!app?.stage?.children) return [];

    const positions: {x: number, y: number}[] = [];
    const findHandContainer = (container: any, depth: number = 0): any => {
      // The hand container is at y around screenHeight - 280 (around y=440)
      // and contains die containers with eventMode = 'static'
      if (container.children && container.children.length > 0) {
        for (const child of container.children) {
          // Die containers have eventMode 'static' and contain sprites
          if (child.eventMode === 'static' && child.cursor === 'pointer') {
            // This is likely a die container - get its global position
            const globalPos = child.getGlobalPosition ? child.getGlobalPosition() : null;
            // Dice are at y≈440 (screenHeight - 280), buttons are at y>550
            // Filter to hand area only (y between 350 and 550)
            if (globalPos && globalPos.y > 350 && globalPos.y < 550) {
              positions.push({ x: globalPos.x, y: globalPos.y });
            }
          }
          findHandContainer(child, depth + 1);
        }
      }
    };
    findHandContainer(app.stage);
    return positions;
  });

  console.log('Found dice positions:', dicePositions);

  // Click on dice to select them (up to numDice)
  const diceToSelect = Math.min(numDice, dicePositions.length);
  for (let i = 0; i < diceToSelect; i++) {
    const pos = dicePositions[i];
    await page.mouse.click(boundingBox.x + pos.x, boundingBox.y + pos.y);
    await page.waitForTimeout(100);
  }

  // Wait a moment for selection to register
  await page.waitForTimeout(200);

  // Click roll button (at actionAreaY = screen_height - 115)
  const rollX = boundingBox.x + boundingBox.width / 2;
  const rollY = boundingBox.y + boundingBox.height - 115;
  await page.mouse.click(rollX, rollY);

  // Wait for animation to complete (2 seconds + buffer)
  await page.waitForTimeout(2800);
}

// Helper to get current score
export async function getScoreText(page: Page): Promise<string | null> {
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
}

// Helper to get entropy text
export async function getEntropyText(page: Page): Promise<string | null> {
  return await page.evaluate(() => {
    const app = (window as any).__PIXI_APP__;
    if (!app?.stage?.children) return null;

    const findEntropy = (container: any): string | null => {
      if (container.text && container.text.startsWith('Entropy:')) {
        return container.text;
      }
      if (container.children) {
        for (const child of container.children) {
          const result = findEntropy(child);
          if (result) return result;
        }
      }
      return null;
    };
    return findEntropy(app.stage);
  });
}

// Helper to get bag/discard counts text
export async function getDeckText(page: Page): Promise<string | null> {
  return await page.evaluate(() => {
    const app = (window as any).__PIXI_APP__;
    if (!app?.stage?.children) return null;

    const findDeck = (container: any): string | null => {
      if (container.text && container.text.includes('Bag:')) {
        return container.text;
      }
      if (container.children) {
        for (const child of container.children) {
          const result = findDeck(child);
          if (result) return result;
        }
      }
      return null;
    };
    return findDeck(app.stage);
  });
}

// Helper to check if round complete dialog is visible (formerly choice dialog)
export async function isChoiceDialogVisible(page: Page): Promise<boolean> {
  return await page.evaluate(() => {
    const app = (window as any).__PIXI_APP__;
    if (!app?.stage?.children) return false;

    const findDialogText = (container: any): boolean => {
      // Now looks for "Round X Complete!" text
      if (container.text && container.text.includes('Complete!')) {
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

// Helper to check if game over dialog is visible
export async function isGameOverVisible(page: Page): Promise<boolean> {
  return await page.evaluate(() => {
    const app = (window as any).__PIXI_APP__;
    if (!app?.stage?.children) return false;

    const findDialogText = (container: any): boolean => {
      if (container.text && container.text.includes('Game Over')) {
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

// Helper to count jokers displayed in the joker slots
export async function getJokerCount(page: Page): Promise<number> {
  return await page.evaluate(() => {
    const app = (window as any).__PIXI_APP__;
    if (!app?.stage?.children) return 0;

    // Count text elements that match joker names (from allJokers in App.hs)
    const jokerNames = ['Blue Chip', 'Red Storm', 'Twin Engines', 'Mean Green', 'Lucky Seven', 'Steady Hand', 'Multiplier', 'Green Machine'];
    let count = 0;
    const countJokers = (container: any) => {
      if (container.text && jokerNames.some(name => container.text === name)) {
        count++;
      }
      if (container.children) {
        for (const child of container.children) {
          countJokers(child);
        }
      }
    };
    countJokers(app.stage);
    return count;
  });
}

// Helper to get selection count text
export async function getSelectionText(page: Page): Promise<string | null> {
  return await page.evaluate(() => {
    const app = (window as any).__PIXI_APP__;
    if (!app?.stage?.children) return null;

    const findSelection = (container: any): string | null => {
      if (container.text && container.text.startsWith('Selected:')) {
        return container.text;
      }
      if (container.children) {
        for (const child of container.children) {
          const result = findSelection(child);
          if (result) return result;
        }
      }
      return null;
    };
    return findSelection(app.stage);
  });
}

// Helper to get all text elements from stage (for debugging)
export async function getAllTexts(page: Page): Promise<string[]> {
  return await page.evaluate(() => {
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
}
