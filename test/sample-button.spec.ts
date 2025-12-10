import { test, expect } from './fixtures';
import { navigateToGame, selectDiceAndRoll, getScoreText } from './test-helpers';

test.describe('Sample Button', () => {
  test('selecting dice and clicking roll changes the score', async ({ page }, testInfo) => {
    const { canvas, boundingBox } = await navigateToGame(page, testInfo);

    // Get initial score
    const initialScore = await getScoreText(page);
    console.log(`Initial score: ${initialScore}`);
    expect(initialScore).toBe('Score: 0');

    // Select dice and roll (new deckbuilding mechanic)
    await selectDiceAndRoll(page, boundingBox, 2);

    // Get the new score
    const newScore = await getScoreText(page);
    console.log(`New score: ${newScore}`);

    // The score should have changed
    expect(newScore).not.toEqual(initialScore);
    expect(newScore).toMatch(/^Score: \d+$/);

    // Verify GAMESTATE score also changed
    const state = await page.evaluate(() => (window as any).GAMESTATE);
    expect(state?.gss_score).not.toBe(0);
  });
});
