{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE LambdaCase #-}

module Main where
import Lib (blipWithFreq, histogram_plot, HistogramOptions(..), defaultHistogramOptions, closeWindow)
import Graphics.PixiJS
import GHC.Wasm.Prim
import Data.String (IsString(..))
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Control.Monad (when, forM_)
import qualified System.Random.SplitMix.Distributions as D
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Aeson (ToJSON(..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.List (partition)

-- Export the actual initialization function
foreign export javascript "main" main :: IO ()

-- *****************************************************************************
-- * Menu System
-- *****************************************************************************

-- | A menu item with text and an action to perform when clicked
data MenuItem = MenuItem {
    menuItem_text :: String,
    menuItem_action :: IO ()
}

-- | A menu is a list of menu items with positioning info
data Menu = Menu {
    menu_items :: [MenuItem],
    menu_x :: Float,
    menu_y :: Float,
    menu_spacing :: Float,  -- vertical spacing between items
    menu_color :: String,
    menu_hoverColor :: String
}

-- | Render a menu and return the container holding it
renderMenu :: Menu -> IO Container
renderMenu menu = do
    container <- newContainer
    setX container menu.menu_x
    setY container menu.menu_y

    forM_ (zip [0..] menu.menu_items) $ \(idx, item) -> do
        text_obj <- newTextWithStyle (toJSString item.menuItem_text) (toJSString menu.menu_color)
        setEventMode text_obj "static"
        setCursor text_obj "pointer"
        setAnchor text_obj 0.5 0.5
        setX text_obj 0.0
        setY text_obj (fromIntegral idx * menu.menu_spacing)

        -- Click handler
        on "pointerdown" text_obj =<< jsFuncFromHs_ (\_ -> item.menuItem_action)

        -- Hover effect - change color
        on "pointerover" text_obj =<< jsFuncFromHs_
             (\_ -> setPropertyKey ["style", "fill"] (toJSVal text_obj)
                        (stringAsVal $ toJSString menu.menu_hoverColor))
        on "pointerout" text_obj =<< jsFuncFromHs_
             (\_ -> setPropertyKey ["style", "fill"] (toJSVal text_obj)
                        (stringAsVal $ toJSString menu.menu_color))

        addChild container text_obj

    return container

-- *****************************************************************************
-- * Screen Management
-- *****************************************************************************

-- | Different screens in the game
data Screen = StartScreen | OptionsScreen | GameScreen
    deriving (Eq, Show)

-- | Global app state for screen management
data AppState = AppState {
    app_currentScreen :: Screen,
    app_screenContainer :: Container  -- The container holding current screen contents
}

-- | Clear the current screen container
clearScreen :: Container -> IO ()
clearScreen container = do
    numChildren <- getNumChildren container
    -- Remove children from end to start to avoid index issues
    forM_ (reverse [0..numChildren-1]) $ \_ -> do
        removeChildAt container 0
    return ()

-- *****************************************************************************
-- * Button (legacy, kept for game screen)
-- *****************************************************************************

data Button = Button {
    button_text :: String,
    button_x :: Float,
    button_y :: Float,
    button_width :: Float,
    button_height :: Float,
    button_color :: String,
    button_on_click :: JSVal -> IO ()
}

renderButton :: Container -> Button -> IO ()
renderButton container button = do
    button_text <- newTextWithStyle (toJSString button.button_text) (toJSString button.button_color)
    setEventMode button_text "static"
    setAnchor button_text 0.5 0.5
    setX button_text button.button_x
    setY button_text button.button_y
    on "pointerdown" button_text =<< jsFuncFromHs_ button.button_on_click

    on "pointerover" button_text =<< jsFuncFromHs_
         (\_ -> do setPropertyKey ["style", "fill"] (toJSVal button_text) (stringAsVal "blue"))
    on "pointerout" button_text =<< jsFuncFromHs_
         (\_ -> do setPropertyKey ["style", "fill"] (toJSVal button_text) (stringAsVal "black"))

    _ <- addChild container button_text
    return ()

data GameState = GameState {
    gs_score :: Integer,
    gs_dists :: Seq Dist,
    gs_target :: Integer
} deriving (Show, Eq)


data Dist = Uniform {low :: Double, high :: Double}
          | Exponential {lambda :: Double}
          | Gamma {k :: Double, theta :: Double}
          | Normal {mean :: Double, stddev :: Double}
  deriving (Eq, Ord)

instance Show Dist where
    show (Uniform a b) = "𝒰([" ++ show a ++ ", " ++ show b ++ "])"
    show (Exponential lambda) = "Exp(λ=" ++ show lambda ++ ")"
    show (Gamma k theta) = "Γ(k=" ++ show k ++ ", θ=" ++ show theta ++ ")"
    show (Normal mean stddev) = "𝒩(μ=" ++ show mean ++ ", σ²=" ++ show stddev ++ ")"


validateDist :: Dist -> Bool
validateDist (Uniform a b) = a < b
validateDist (Exponential lambda) = lambda > 0
validateDist (Gamma k theta) = k > 0 && theta > 0
validateDist (Normal mean stddev) = stddev > 0


sampleDist :: Monad m => Dist -> D.GenT m Double
sampleDist (Uniform low high) = D.uniformR low high
sampleDist (Exponential theta) = D.exponential theta
sampleDist (Gamma k theta) = D.gamma k theta
sampleDist (Normal mean stddev) = D.normal mean stddev

sampleDists :: Monad m => Seq Dist -> D.GenT m Double
sampleDists dists = do
    case Seq.viewl dists of
        Seq.EmptyL -> return 0.0
        first Seq.:< rest -> do
            x <- sampleDist first
            xs <- sampleDists rest
            return (x + xs)


showDists :: Seq Dist -> String
showDists dists = case Seq.viewl dists of
    Seq.EmptyL -> ""
    first Seq.:< rest -> show first ++ foldMap (\dist -> " + " ++ show dist) rest


poisson :: Monad m => Double -> D.GenT m Int
poisson lambda = go 0 0
  where go !t !k = do
          t' <- (t +) <$> D.exponential lambda
          if t' > 1
          then return k
          else go t' (k + 1)

newtype Histogram = Histogram [(Int, Double)]

instance ToJSON Histogram where
    toJSON (Histogram bins) = toJSON bins

histogram :: Int -> Int -> Seq Dist -> IO Histogram
histogram num_samples bins dists = do
    samples <- D.samplesIO num_samples $ sampleDists dists
    let (h_min, h_max) = (minimum samples, maximum samples)
    let bin_width = (h_max - h_min) / fromIntegral bins
    let bins' :: Double -> Double -> [Double] -> [(Int, Double)]
        bins' !acc !h_cutoff [] = []
        bins' !acc !h_cutoff !samples =
            let (lt, rest) = partition (<= h_cutoff) samples
                len_lt = length lt
                acc' = acc + (fromIntegral len_lt)
                h_cutoff' = h_cutoff + bin_width
            in  (len_lt, h_cutoff) : bins' acc' h_cutoff' rest
        bins = bins' 0.0 (h_min  + bin_width) samples
    return (Histogram bins)



-- | Todo: here we should simplify the distributions by combining like terms,
-- e.g. N(500,100) + N(500,100) -> N(1000,200)
simplifyDists :: Seq Dist -> Seq Dist
simplifyDists dists = dists

histogram_options :: HistogramOptions
histogram_options = defaultHistogramOptions {
    ho_width = 300,
    ho_height = 150,
    ho_fillColor = "black",
    ho_backgroundColor = "white",
    ho_marginLeft = 35,
    ho_marginRight = 10,
    ho_marginTop = 10,
    ho_marginBottom = 25
}

updateScore :: IORef GameState -> Text -> Text -> Text -> Sprite -> IO ()
updateScore game_state_ref score_text target_text distr_text histogram_sprite = do
    game_state@GameState{..} <- readIORef game_state_ref
    sample <- D.sampleIO $ sampleDists gs_dists

    let new_score = gs_score + round sample
    setText score_text (toJSString $ "Score: " ++ show new_score)
    if new_score >= gs_target then do
        blipWithFreq 800.0
        let new_target = gs_target * 2
        let new_dists = gs_dists :|> Normal 500.0 100.0
        setText target_text (toJSString $ "Target: " ++ show new_target)
        setText distr_text (toJSString $ "X ~ " ++ showDists new_dists)

        Histogram histogram <- histogram 10_000 100 new_dists
        histogram_data <- parseJSON (toJSString $ BSC.unpack $ Aeson.encode histogram)
        histogram_texture_jsval <- histogram_plot histogram_data histogram_options
        setProperty "texture" (toJSVal histogram_sprite) histogram_texture_jsval

        writeIORef game_state_ref (game_state { gs_score = 0, gs_target = new_target, gs_dists = new_dists })
    else
        writeIORef game_state_ref (game_state { gs_score = new_score })

-- *****************************************************************************
-- * Screen Rendering Functions
-- *****************************************************************************

-- | Render the game screen (the main gameplay)
renderGameScreen :: Application -> Container -> Int -> Int -> IORef GameState
                 -> (IO ())  -- ^ Action to show pause menu
                 -> IO ()
renderGameScreen app screenContainer screen_width screen_height game_state_ref showPauseMenu = do
    clearScreen screenContainer

    GameState{..} <- readIORef game_state_ref

    -- Histogram at the top (smaller now)
    Histogram hist <- histogram 10_000 100 gs_dists
    histogram_data <- parseJSON (toJSString $ BSC.unpack $ Aeson.encode hist)
    histogram_texture_jsval <- histogram_plot histogram_data histogram_options
    let histogram_texture = fromJSVal histogram_texture_jsval :: Texture
    histogram_sprite <- newSpriteFromTexture histogram_texture
    setX histogram_sprite (fromIntegral screen_width / 2.0)
    setY histogram_sprite 100.0
    setAnchor histogram_sprite 0.5 0.5
    addChild screenContainer histogram_sprite

    -- Game info in the middle
    target_text <- newTextWithStyle (toJSString $ "Target: " ++ show gs_target) "black"
    setX target_text (fromIntegral screen_width / 2.0)
    setY target_text (fromIntegral screen_height / 2.0 - 60.0)
    setAnchor target_text 0.5 0.5
    addChild screenContainer target_text

    score_text <- newTextWithStyle (toJSString $ "Score: " ++ show gs_score) "black"
    setX score_text (fromIntegral screen_width / 2.0)
    setY score_text (fromIntegral screen_height / 2.0)
    setAnchor score_text 0.5 0.5
    addChild screenContainer score_text

    distr_text <- newTextWithStyle (toJSString $ "X ∼ " ++ showDists gs_dists) "black"
    setX distr_text (fromIntegral screen_width / 2.0)
    setY distr_text (fromIntegral screen_height / 2.0 + 60.0)
    setAnchor distr_text 0.5 0.5
    addChild screenContainer distr_text

    -- Buttons at the bottom
    let sample_button = Button {
        button_text = "Sample",
        button_x = fromIntegral screen_width / 2.0,
        button_y = fromIntegral screen_height - 150.0,
        button_width = 100.0,
        button_height = 100.0,
        button_color = "black",
        button_on_click =
             \_ ->
                updateScore game_state_ref score_text target_text distr_text histogram_sprite
    }
    renderButton screenContainer sample_button

    let menu_button = Button {
        button_text = "Menu",
        button_x = fromIntegral screen_width / 2.0,
        button_y = fromIntegral screen_height - 80.0,
        button_width = 100.0,
        button_height = 50.0,
        button_color = "black",
        button_on_click = \_ -> showPauseMenu
    }
    renderButton screenContainer menu_button

-- | Render the pause menu screen
renderPauseMenu :: Application -> Container -> Int -> Int
                -> (IO ())  -- ^ Action to continue game
                -> (IO ())  -- ^ Action to quit to start screen
                -> IO ()
renderPauseMenu app screenContainer screen_width screen_height continueGame quitGame = do
    clearScreen screenContainer

    -- Title
    title <- newTextWithStyle (toJSString "Paused") "black"
    setX title (fromIntegral screen_width / 2.0)
    setY title 150.0
    setAnchor title 0.5 0.5
    addChild screenContainer title

    -- Menu
    let pauseMenu = Menu {
        menu_items = [
            MenuItem { menuItem_text = "Continue", menuItem_action = continueGame },
            MenuItem { menuItem_text = "Quit", menuItem_action = quitGame }
        ],
        menu_x = fromIntegral screen_width / 2.0,
        menu_y = fromIntegral screen_height / 2.0,
        menu_spacing = 60.0,
        menu_color = "black",
        menu_hoverColor = "blue"
    }

    menuContainer <- renderMenu pauseMenu
    _ <- addChild screenContainer menuContainer
    return ()

-- | Render the start screen with menu
renderStartScreen :: Application -> Container -> Int -> Int
                  -> (IO ())  -- ^ Action to show game screen
                  -> (IO ())  -- ^ Action to show options screen
                  -> IO ()
renderStartScreen app screenContainer screen_width screen_height showGame showOptions = do
    clearScreen screenContainer

    -- Title
    title <- newTextWithStyle (toJSString "A Game of Chance") "black"
    setX title (fromIntegral screen_width / 2.0)
    setY title 150.0
    setAnchor title 0.5 0.5
    addChild screenContainer title

    -- Menu
    let startMenu = Menu {
        menu_items = [
            MenuItem { menuItem_text = "Start Game", menuItem_action = showGame },
            MenuItem { menuItem_text = "Options", menuItem_action = showOptions },
            MenuItem { menuItem_text = "Exit", menuItem_action = closeWindow }
        ],
        menu_x = fromIntegral screen_width / 2.0,
        menu_y = fromIntegral screen_height / 2.0,
        menu_spacing = 60.0,
        menu_color = "black",
        menu_hoverColor = "blue"
    }

    menuContainer <- renderMenu startMenu
    _ <- addChild screenContainer menuContainer
    return ()

-- | Render the options screen
renderOptionsScreen :: Application -> Container -> Int -> Int
                    -> (IO ())  -- ^ Action to go back to start screen
                    -> IO ()
renderOptionsScreen app screenContainer screen_width screen_height goBack = do
    clearScreen screenContainer

    -- Title
    title <- newTextWithStyle (toJSString "Options") "black"
    setX title (fromIntegral screen_width / 2.0)
    setY title 150.0
    setAnchor title 0.5 0.5
    addChild screenContainer title

    -- Placeholder text
    placeholder <- newTextWithStyle (toJSString "(No options yet)") "gray"
    setX placeholder (fromIntegral screen_width / 2.0)
    setY placeholder (fromIntegral screen_height / 2.0 - 50.0)
    setAnchor placeholder 0.5 0.5
    addChild screenContainer placeholder

    -- Back menu
    let backMenu = Menu {
        menu_items = [
            MenuItem { menuItem_text = "Back", menuItem_action = goBack }
        ],
        menu_x = fromIntegral screen_width / 2.0,
        menu_y = fromIntegral screen_height / 2.0 + 50.0,
        menu_spacing = 60.0,
        menu_color = "black",
        menu_hoverColor = "blue"
    }

    menuContainer <- renderMenu backMenu
    _ <- addChild screenContainer menuContainer
    return ()

-- | Initial game state
initialGameState :: GameState
initialGameState = GameState {
    gs_score = 0,
    gs_dists = Seq.fromList [Normal 500.0 100.0],
    gs_target = 10_000
}

main :: IO ()
main = do
    -- Initialize PIXI application
    app <- newApp >>= flip initApp "white"
    appendCanvas app
    screen <- getScreen app
    screen_width <- round <$> getRectWidth screen
    screen_height <- round <$> getRectHeight screen

    -- Create screen container that will hold all screen content
    screenContainer <- newContainer
    stage <- getStage app
    addChild stage screenContainer

    -- Initialize game state
    game_state_ref <- newIORef initialGameState

    -- Define screen transition functions using mutual recursion via IORefs
    showStartScreenRef <- newIORef (return () :: IO ())
    showOptionsScreenRef <- newIORef (return () :: IO ())
    showGameScreenRef <- newIORef (return () :: IO ())
    showPauseMenuRef <- newIORef (return () :: IO ())

    let showStartScreen = do
            action <- readIORef showStartScreenRef
            action

    let showOptionsScreen = do
            action <- readIORef showOptionsScreenRef
            action

    let showGameScreen = do
            action <- readIORef showGameScreenRef
            action

    let showPauseMenu = do
            action <- readIORef showPauseMenuRef
            action

    -- Reset game state and go to start screen
    let quitToStart = do
            writeIORef game_state_ref initialGameState
            showStartScreen

    -- Set up the actual screen rendering functions
    writeIORef showStartScreenRef $
        renderStartScreen app screenContainer screen_width screen_height
            showGameScreen showOptionsScreen

    writeIORef showOptionsScreenRef $
        renderOptionsScreen app screenContainer screen_width screen_height
            showStartScreen

    writeIORef showGameScreenRef $
        renderGameScreen app screenContainer screen_width screen_height game_state_ref
            showPauseMenu

    writeIORef showPauseMenuRef $
        renderPauseMenu app screenContainer screen_width screen_height
            showGameScreen quitToStart

    -- Start at the start screen
    showStartScreen
