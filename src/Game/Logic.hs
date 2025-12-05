{-# LANGUAGE OverloadedRecordDot #-}

-- | Pure state transitions for the game.
module Game.Logic
    ( GameAction(..)
    , applyAction
    ) where

import qualified Data.Set as Set
import Game.State

-- | All actions that mutate the core state.
data GameAction
    = StartGame           -- ^ Enter gameplay from the start menu (fresh state)
    | PauseGame           -- ^ Enter pause menu
    | ResumeGame          -- ^ Return to gameplay from pause
    | QuitToStart         -- ^ Quit to start menu (reset state)
    | SetScreen ScreenState
    | SetPhase GamePhase
    | SelectDie DieId
    | DeselectDie DieId
    | ClearSelection
    deriving (Eq, Show)

-- | Apply a single action to the current state.
applyAction :: GameAction -> GameState -> GameState
applyAction action gs = case action of
    StartGame ->
        initialGameState { gs_screen = GameScreen }
    PauseGame ->
        gs { gs_screen = PauseScreen }
    ResumeGame ->
        gs { gs_screen = GameScreen }
    QuitToStart ->
        initialGameState { gs_screen = StartScreen }
    SetScreen scr ->
        gs { gs_screen = scr }
    SetPhase phase' ->
        gs { gs_phase = phase' }
    ClearSelection ->
        let SelectionState _ maxSel = gs_selection gs
        in gs { gs_selection = SelectionState Set.empty maxSel }
    SelectDie dieId ->
        let SelectionState sel maxSel = gs_selection gs
        in if Set.size sel < maxSel
              then gs { gs_selection = SelectionState (Set.insert dieId sel) maxSel }
              else gs
    DeselectDie dieId ->
        let SelectionState sel maxSel = gs_selection gs
        in gs { gs_selection = SelectionState (Set.delete dieId sel) maxSel }

