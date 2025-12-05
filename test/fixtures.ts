import { test as base, Page, TestInfo } from '@playwright/test';
import * as fs from 'fs';
import * as path from 'path';

// Extend the base test to capture GAMESTATE on failure
export const test = base.extend<{
  captureOnFailure: void;
}>({
  captureOnFailure: [async ({ page }, use, testInfo) => {
    // Run the test
    await use();

    // After test: if failed, capture GAMESTATE and screenshot
    if (testInfo.status !== testInfo.expectedStatus) {
      await captureDebugInfo(page, testInfo);
    }
  }, { auto: true }],
});

export { expect } from '@playwright/test';

async function captureDebugInfo(page: Page, testInfo: TestInfo) {
  try {
    // Capture GAMESTATE
    const gameState = await page.evaluate(() => {
      return (window as any).GAMESTATE;
    }).catch(() => null);

    if (gameState) {
      const stateFile = testInfo.outputPath('GAMESTATE.json');
      fs.writeFileSync(stateFile, JSON.stringify(gameState, null, 2));
      await testInfo.attach('GAMESTATE', {
        body: JSON.stringify(gameState, null, 2),
        contentType: 'application/json',
      });
    }

    // Capture all visible text on stage
    const stageTexts = await page.evaluate(() => {
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
    }).catch(() => []);

    if (stageTexts.length > 0) {
      await testInfo.attach('Stage Texts', {
        body: JSON.stringify(stageTexts, null, 2),
        contentType: 'application/json',
      });
    }

    // Take a final screenshot with timestamp
    const screenshotPath = testInfo.outputPath(`failure-${Date.now()}.png`);
    await page.screenshot({ path: screenshotPath, fullPage: true });
    await testInfo.attach('Failure Screenshot', {
      path: screenshotPath,
      contentType: 'image/png',
    });

  } catch (e) {
    console.error('Failed to capture debug info:', e);
  }
}
