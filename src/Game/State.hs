{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Core game state and pure data structures.
-- This module holds the serialisable state used by the logic layer and UI.
module Game.State
    ( ScreenState(..)
    , DiceType(..)
    , DieId(..)
    , Die(..)
    , DeckZones(..)
    , SelectionState(..)
    , GamePhase(..)
    , ShopContents(..)
    , DieResult(..)
    , JokerEffect(..)
    , Joker(..)
    , RoundInfo(..)
    , RollState(..)
    , GameState(..)
    , allJokers
    , GameSnapshot(..)
    , toSnapshot
    , applySnapshot
    , applyJokerEffect
    , calculateScoreWithJokers
    , initialDeckZones
    , initialJokers
    , initialGameState
    , generateRound
    , rollsPerRound
    , handSize
    , maxSelection
    , drawCount
    ) where

import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Graphics.PixiJS (Container, JSVal)

-- | High-level screen identifier for menus and gameplay.
data ScreenState
    = LoadingScreen
    | StartScreen
    | OptionsScreen
    | GameScreen
    | PauseScreen
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Type of die: additive contributes to X, multiplicative to Y in X*Y scoring.
data DiceType = Additive | Multiplicative
    deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON)

-- | Unique identifier for each die in the game.
newtype DieId = DieId Int
    deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON)

-- | A die with unique identity and type.
data Die = Die
    { die_id   :: DieId
    , die_type :: DiceType
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | The three zones where dice can exist (bag, hand, discard).
data DeckZones = DeckZones
    { dz_bag     :: [Die]   -- ^ Undrawn dice (shuffled)
    , dz_hand    :: [Die]   -- ^ Currently drawn dice
    , dz_discard :: [Die]   -- ^ Used/discarded dice
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Selection state tracking which dice are selected for throwing.
data SelectionState = SelectionState
    { ss_selected      :: Set.Set DieId  -- ^ IDs of selected dice
    , ss_maxSelectable :: Int            -- ^ Max dice that can be selected (fixed at 5)
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Game phases in a round.
data GamePhase
    = DrawPhase         -- Drawing dice from bag
    | SelectionPhase    -- Selecting dice to throw
    | RollingPhase      -- Dice are being animated
    | ResolutionPhase   -- Score is being calculated
    | ChoicePhase       -- Player choosing new die after reaching target (legacy)
    | ShopPhase         -- Shopping between rounds
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Shop contents - what's available for purchase this visit (jokers only).
data ShopContents = ShopContents
    { shop_joker      :: Maybe Joker  -- ^ First joker slot (Nothing if sold)
    , shop_jokerCost  :: Int          -- ^ Cost of first joker (4-8)
    , shop_joker2     :: Maybe Joker  -- ^ Second joker slot (Nothing if sold)
    , shop_joker2Cost :: Int          -- ^ Cost of second joker (4-8)
    , shop_rerollCost :: Int          -- ^ Cost to reroll shop (always 2)
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Result of a single die roll, tracking type and value.
-- Sprite/JS handles stay in the runtime layer and are not part of snapshots.
data DieResult = DieResult
    { dr_type   :: DiceType
    , dr_value  :: Int
    , dr_sprite :: JSVal
    }

-- | Joker effects that modify scoring.
data JokerEffect
    = AddToX Int                    -- ^ Flat +N to X
    | AddToY Int                    -- ^ Flat +N to Y
    | ScaleY Float                  -- ^ Multiply Y by factor
    | PerDieBonus DiceType Int      -- ^ +N to X per mult die, or +N to Y per add die
    | OnValueBonus Int Int          -- ^ +N to X for each die showing value V
    | ComboBonus Int                -- ^ +N to Y per combo triggered
    deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | A joker with name, description, and effect.
data Joker = Joker
    { joker_id          :: Int
    , joker_name        :: String
    , joker_description :: String
    , joker_effect      :: JokerEffect
    , joker_cost        :: Int
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | All available jokers in the game.
allJokers :: [Joker]
allJokers =
    [ Joker 1 "Blue Chip" "+5 to X on every roll" (AddToX 5) 5
    , Joker 2 "Red Storm" "+1 to Y for each mult die rolled" (PerDieBonus Multiplicative 1) 6
    , Joker 3 "Twin Engines" "+3 to Y for each combo triggered" (ComboBonus 3) 7
    , Joker 4 "Mean Green" "+2 to X for each 1 rolled" (OnValueBonus 1 2) 5
    , Joker 5 "Lucky Seven" "+4 to X for each 6 rolled" (OnValueBonus 6 4) 6
    , Joker 6 "Steady Hand" "+3 to Y on every roll" (AddToY 3) 6
    , Joker 7 "Multiplier" "x1.5 to final Y" (ScaleY 1.5) 8
    , Joker 8 "Green Machine" "+1 to Y for each add die rolled" (PerDieBonus Additive 1) 6
    ]

-- | Apply a single joker effect to the current X and Y values.
applyJokerEffect :: [DieResult] -> Int -> JokerEffect -> (Int, Int) -> (Int, Int)
applyJokerEffect results comboCount effect (x, y) = case effect of
    AddToX n -> (x + n, y)
    AddToY n -> (x, y + n)
    ScaleY f -> (x, round (fromIntegral y * f))
    OnValueBonus targetVal bonus ->
        let count = length [r | r <- results, dr_value r == targetVal]
        in (x + count * bonus, y)
    ComboBonus n -> (x, y + comboCount * n)
    PerDieBonus dtype n ->
        let count = length [r | r <- results, dr_type r == dtype]
        in case dtype of
            Multiplicative -> (x + count * n, y)
            Additive -> (x, y + count * n)

-- | Apply all joker effects to calculate final score.
calculateScoreWithJokers :: [Joker] -> [DieResult] -> Int -> (Int, Int, Int)
calculateScoreWithJokers jokers results comboCount =
    let baseX = sum [dr_value r | r <- results, dr_type r == Additive]
        baseY = sum [dr_value r | r <- results, dr_type r == Multiplicative]
        (modX, modY) = foldl (flip $ applyJokerEffect results comboCount . joker_effect)
                             (baseX, baseY) jokers
        finalY = if modY == 0 then 1 else modY
    in (modX, finalY, modX * finalY)

-- | Current round state (simplified from Blind/Ante system).
data RoundInfo = RoundInfo
    { round_number :: Int  -- ^ Which round (1, 2, 3, ...)
    , round_target :: Int  -- ^ Score needed to beat this round
    , round_reward :: Int  -- ^ Currency reward for beating
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Generate round with appropriate target.
generateRound :: Int -> RoundInfo
generateRound roundNum = RoundInfo
    { round_number = roundNum
    , round_target = 15 + (roundNum - 1) * 15  -- 15, 30, 45, 60, ...
    , round_reward = 3 + roundNum              -- 4, 5, 6, 7, ...
    }

-- | Rolls allowed per round (always 4).
rollsPerRound :: Int
rollsPerRound = 4

-- | Current roll state for tracking dice during animation.
data RollState = RollState
    { rs_pending     :: Int          -- ^ Number of dice still animating
    , rs_newResults  :: [DieResult]  -- ^ Results from current batch (for combo detection)
    , rs_prevResults :: [DieResult]  -- ^ Results from previous batches (not checked for combos)
    , rs_isComboRoll :: Bool         -- ^ Whether this is a combo bonus roll
    , rs_comboCount  :: Int          -- ^ Total combos triggered this roll (for joker effects)
    }

-- | Main game state tracked by the runtime and logic layers.
data GameState = GameState
    { gs_screen            :: ScreenState
    , gs_score             :: Int       -- ^ Current accumulated score
    , gs_target            :: Int       -- ^ Target to reach (from current round)
    , gs_persistentSprites :: [JSVal]   -- ^ Sprites from last roll (to clear on next roll)
    , gs_rollState         :: Maybe RollState  -- ^ Active roll tracking (Nothing when idle)
    , gs_round             :: Int       -- ^ Current round number
    -- Deckbuilding fields
    , gs_entropy           :: Int              -- ^ Currency (earned from rounds, spent in shop)
    , gs_deckZones         :: DeckZones        -- ^ Bag, hand, discard piles
    , gs_selection         :: SelectionState   -- ^ Which dice are selected
    , gs_phase             :: GamePhase        -- ^ Current game phase
    , gs_nextDieId         :: Int              -- ^ Counter for unique die IDs
    , gs_handSprites       :: [(DieId, JSVal)] -- ^ Sprites for dice in hand
    , gs_handContainer     :: Maybe Container  -- ^ Container for hand display
    -- Joker system
    , gs_jokers            :: [Joker]          -- ^ Active jokers (max 5)
    , gs_jokerContainer    :: Maybe Container  -- ^ Container for joker slots UI
    -- Round system
    , gs_roundInfo         :: RoundInfo        -- ^ Current round info
    , gs_rollsRemaining    :: Int              -- ^ Rolls left this round
    -- Shop system
    , gs_shopContents      :: Maybe ShopContents -- ^ Current shop offerings (Nothing when not in shop)
    }

-- | Serializable snapshot of the logic state (no JS handles).
data GameSnapshot = GameSnapshot
    { gss_screen         :: ScreenState
    , gss_score          :: Int
    , gss_target         :: Int
    , gss_round          :: Int
    , gss_entropy        :: Int
    , gss_deckZones      :: DeckZones
    , gss_selection      :: SelectionState
    , gss_phase          :: GamePhase
    , gss_nextDieId      :: Int
    , gss_jokers         :: [Joker]
    , gss_roundInfo      :: RoundInfo
    , gss_rollsRemaining :: Int
    , gss_shopContents   :: Maybe ShopContents
    } deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Convert the full runtime state into a JSON-ready snapshot.
toSnapshot :: GameState -> GameSnapshot
toSnapshot GameState{..} = GameSnapshot
    { gss_screen = gs_screen
    , gss_score = gs_score
    , gss_target = gs_target
    , gss_round = gs_round
    , gss_entropy = gs_entropy
    , gss_deckZones = gs_deckZones
    , gss_selection = gs_selection
    , gss_phase = gs_phase
    , gss_nextDieId = gs_nextDieId
    , gss_jokers = gs_jokers
    , gss_roundInfo = gs_roundInfo
    , gss_rollsRemaining = gs_rollsRemaining
    , gss_shopContents = gs_shopContents
    }

-- | Apply a snapshot onto an existing runtime state, clearing UI handles.
applySnapshot :: GameSnapshot -> GameState -> GameState
applySnapshot snap base = base
    { gs_screen = gss_screen snap
    , gs_score = gss_score snap
    , gs_target = gss_target snap
    , gs_round = gss_round snap
    , gs_entropy = gss_entropy snap
    , gs_deckZones = gss_deckZones snap
    , gs_selection = gss_selection snap
    , gs_phase = gss_phase snap
    , gs_nextDieId = gss_nextDieId snap
    , gs_jokers = gss_jokers snap
    , gs_roundInfo = gss_roundInfo snap
    , gs_rollsRemaining = gss_rollsRemaining snap
    , gs_shopContents = gss_shopContents snap
    , gs_persistentSprites = []
    , gs_rollState = Nothing
    , gs_handSprites = []
    , gs_handContainer = Nothing
    , gs_jokerContainer = Nothing
    }

-- | Fixed hand size and selection limit (Balatro-style: draw 8, select 5).
handSize :: Int
handSize = 8

maxSelection :: Int
maxSelection = 5

-- | Calculate draw count (now fixed).
drawCount :: Int -> Int
drawCount _ = handSize

-- | Create initial deck with 6 additive and 6 multiplicative dice.
initialDeckZones :: (DeckZones, Int)
initialDeckZones =
    let additiveDice = [Die (DieId i) Additive | i <- [0..5]]
        multiDice = [Die (DieId i) Multiplicative | i <- [6..11]]
        allDice = additiveDice ++ multiDice
    in (DeckZones { dz_bag = allDice, dz_hand = [], dz_discard = [] }, 12)

-- | Initial jokers for testing (Blue Chip + Twin Engines).
initialJokers :: [Joker]
initialJokers =
    [ allJokers !! 0  -- Blue Chip: +5 to X
    , allJokers !! 2  -- Twin Engines: +3 to Y per combo
    ]

-- | Initial game state.
initialGameState :: GameState
initialGameState =
    let (zones, nextId) = initialDeckZones
        startingRound = generateRound 1
    in GameState
        { gs_screen = StartScreen
        , gs_score = 0
        , gs_target = round_target startingRound
        , gs_persistentSprites = []
        , gs_rollState = Nothing
        , gs_round = 1
        , gs_entropy = 4  -- Starting currency
        , gs_deckZones = zones
        , gs_selection = SelectionState Set.empty maxSelection
        , gs_phase = DrawPhase
        , gs_nextDieId = nextId
        , gs_handSprites = []
        , gs_handContainer = Nothing
        , gs_jokers = initialJokers
        , gs_jokerContainer = Nothing
        , gs_roundInfo = startingRound
        , gs_rollsRemaining = rollsPerRound
        , gs_shopContents = Nothing
        }

