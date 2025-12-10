{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.List (nub)
import qualified Data.Set as Set
import Game.Logic
import Game.State
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Game logic"
    [ testCase "start game resets state and enters gameplay" $
        let started = applyAction StartGame initialGameState
        in toSnapshot started @?= toSnapshot (initialGameState { gs_screen = GameScreen })
    , testCase "pause then quit then start resets state" $
        let s1 = applyAction StartGame initialGameState
            s2 = applyAction PauseGame s1
            s3 = applyAction QuitToStart s2
            s4 = applyAction StartGame s3
        in toSnapshot s4 @?= toSnapshot (initialGameState { gs_screen = GameScreen })
    , testCase "selection adds and removes dice respecting max" $
        let dieA = DieId 1
            dieB = DieId 2
            base = applyAction StartGame initialGameState
            sel1 = applyAction (SelectDie dieA) base
            sel2 = applyAction (SelectDie dieB) sel1
            selCleared = applyAction ClearSelection sel2
        in do
            Set.member dieA (ss_selected (gs_selection sel1)) @? "first die selected"
            Set.member dieB (ss_selected (gs_selection sel2)) @? "second die selected"
            Set.null (ss_selected (gs_selection selCleared)) @? "selection cleared"
    , testGroup "properties"
        [ QC.testProperty "StartGame resets to baseline snapshot" $
            \(ArbSnapshot snap) ->
                let base = applySnapshot snap initialGameState
                    result = applyAction StartGame base
                    expected = initialGameState { gs_screen = GameScreen }
                in toSnapshot result === toSnapshot expected
        , QC.testProperty "QuitToStart resets to baseline start snapshot" $
            \(ArbSnapshot snap) ->
                let base = applySnapshot snap initialGameState
                    result = applyAction QuitToStart base
                    expected = initialGameState { gs_screen = StartScreen }
                in toSnapshot result === toSnapshot expected
        , QC.testProperty "Pause then resume returns to GameScreen" $
            \(ArbSnapshot snap) ->
                let base = applyAction StartGame (applySnapshot snap initialGameState)
                    paused = applyAction PauseGame base
                    resumed = applyAction ResumeGame paused
                in gs_screen resumed === GameScreen
        , QC.testProperty "Selection never exceeds maxSelection" $
            \(SmallIds ids) ->
                let actions = fmap (SelectDie . DieId) ids
                    final = applyActions actions (applyAction StartGame initialGameState)
                in Set.size (ss_selected (gs_selection final)) <= maxSelection
        , QC.testProperty "ClearSelection empties selection" $
            \(SmallIds ids) ->
                let actions = fmap (SelectDie . DieId) ids ++ [ClearSelection]
                    final = applyActions actions (applyAction StartGame initialGameState)
                in Set.null (ss_selected (gs_selection final))
        , QC.testProperty "Select then Deselect returns to prior selection" $
            \(SmallIds ids) ->
                let base = applyAction StartGame initialGameState
                    selected = applyActions (fmap (SelectDie . DieId) ids) base
                    deselected = applyActions (fmap (DeselectDie . DieId) (reverse ids)) selected
                in ss_selected (gs_selection deselected) === Set.empty
        ]
    ]

-- Helpers
applyActions :: [GameAction] -> GameState -> GameState
applyActions acts st = foldl (flip applyAction) st acts

-- Generators
newtype ArbSnapshot = ArbSnapshot GameSnapshot deriving (Show)
newtype SmallIds = SmallIds [Int] deriving (Show)

instance Arbitrary SmallIds where
    arbitrary = do
        n <- choose (0, 12)
        xs <- vectorOf n (choose (0, 8))
        pure (SmallIds (take maxSelection (nub xs)))

instance Arbitrary ArbSnapshot where
    arbitrary = do
        gss_screen <- elements [StartScreen, OptionsScreen, GameScreen, PauseScreen, LoadingScreen]
        gss_score <- choose (0, 200)
        gss_target <- choose (10, 100)
        gss_round <- choose (1, 5)
        gss_entropy <- choose (0, 10)
        gss_deckZones <- genDeckZones
        gss_selection <- genSelection
        gss_phase <- elements [DrawPhase, SelectionPhase, RollingPhase, ResolutionPhase, ChoicePhase, ShopPhase]
        gss_nextDieId <- choose (0, 30)
        gss_jokers <- sublistOf allJokers
        gss_roundInfo <- genRoundInfo
        gss_rollsRemaining <- choose (0, rollsPerRound)
        gss_shopContents <- genShop
        pure . ArbSnapshot $ GameSnapshot {..}
      where
        genDeckZones = do
            bag <- genDice 6
            hand <- genDice 5
            discard <- genDice 6
            pure DeckZones { dz_bag = bag, dz_hand = hand, dz_discard = discard }
        genSelection = do
            maxSel <- choose (1, maxSelection)
            ids <- sublistOf [DieId i | i <- [0..8]]
            pure $ SelectionState (Set.fromList (take maxSel ids)) maxSel
        genRoundInfo = do
            n <- choose (1, 5)
            pure $ generateRound n
        genShop = frequency
            [ (1, pure Nothing)
            , (3, do
                    j1 <- elements allJokers
                    j2 <- elements allJokers
                    c1 <- choose (4, 8)
                    c2 <- choose (4, 8)
                    rr <- pure 2
                    pure $ Just ShopContents
                        { shop_joker = Just j1
                        , shop_jokerCost = c1
                        , shop_joker2 = Just j2
                        , shop_joker2Cost = c2
                        , shop_rerollCost = rr
                        })
            ]
        genDice k = do
            n <- choose (0, k)
            ts <- vectorOf n arbitrary
            pure [Die (DieId i) t | (i, t) <- zip [0..] ts]

instance Arbitrary DiceType where
    arbitrary = elements [Additive, Multiplicative]

instance Arbitrary DieId where
    arbitrary = DieId <$> choose (0, 30)

