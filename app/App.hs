{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE LambdaCase #-}

module Main where
import Lib (blipWithFreq, histogram_plot, HistogramOptions(..), defaultHistogramOptions)
import Graphics.PixiJS
import GHC.Wasm.Prim
import Data.String (IsString(..))
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Control.Monad (when)
import qualified System.Random.SplitMix.Distributions as D
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Aeson (ToJSON(..))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as BSC
import Data.List (partition)

-- Export the actual initialization function
foreign export javascript "main" main :: IO ()

data Button = Button {
    button_text :: String,
    button_x :: Float,
    button_y :: Float,
    button_width :: Float,
    button_height :: Float,
    button_color :: String,
    button_on_click :: JSVal -> IO ()
}

renderButton :: Application -> Button -> IO ()
renderButton app button = do
    button_text <- newTextWithStyle (toJSString button.button_text) (toJSString button.button_color)
    setEventMode button_text "static"
    setAnchor button_text 0.5 0.5
    setX button_text button.button_x
    setY button_text button.button_y
    on "pointerdown" button_text =<< jsFuncFromHs_ button.button_on_click
    exportValue "SAMPLE" (toJSVal button_text)

    on "pointerover" button_text =<< jsFuncFromHs_
         (\_ -> do setPropertyKey ["style", "fill"] (toJSVal button_text) (stringAsVal "blue"))
    on "pointerout" button_text =<< jsFuncFromHs_
         (\_ -> do setPropertyKey ["style", "fill"] (toJSVal button_text) (stringAsVal "black"))

    stage <- getStage app
    _ <- addChild stage button_text
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
    ho_width = 600,
    ho_height = 400,
    ho_fillColor = "black",
    ho_backgroundColor = "white"
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

main :: IO ()
main = do
    -- Initialize PIXI application
    app <- newApp >>= flip initApp "white"
    appendCanvas app
    screen <- getScreen app
    screen_width <- round <$> getRectWidth screen
    screen_height <- round <$> getRectHeight screen
    let dists = Seq.fromList [Normal 500.0 100.0]
    game_state_ref <- newIORef GameState {
                             gs_score = 0,
                             gs_dists = dists,
                             gs_target = 10_000
                         }
    target_text <- newTextWithStyle (toJSString "Target: 10000") "black"
    setX target_text (fromIntegral screen_width / 2.0)
    setY target_text ((fromIntegral screen_height / 2.0) - 100.0)
    setAnchor target_text 0.5 0.5
    stage <- getStage app
    addChild stage target_text

    score_text <- newTextWithStyle (toJSString "Score: 0") "black"
    setX score_text (fromIntegral screen_width / 2.0)
    setY score_text (fromIntegral screen_height / 2.0)
    setAnchor score_text 0.5 0.5
    addChild stage score_text

    distr_text <- newTextWithStyle (toJSString $ "X ∼ " ++ showDists dists) "black"
    setX distr_text (fromIntegral screen_width / 2.0)
    setY distr_text ((fromIntegral screen_height / 2.0) + 100.0)
    setAnchor distr_text 0.5 0.5
    addChild stage distr_text

    Histogram histogram <- histogram 10_000 100 dists

    histogram_data <- parseJSON (toJSString $ BSC.unpack $ Aeson.encode histogram)
    histogram_texture_jsval <- histogram_plot histogram_data histogram_options
    let histogram_texture = fromJSVal histogram_texture_jsval :: Texture
    histogram_sprite <- newSpriteFromTexture histogram_texture
    setX histogram_sprite (fromIntegral screen_width / 2.0)
    setY histogram_sprite 200.0
    setAnchor histogram_sprite 0.5 0.5
    addChild stage histogram_sprite


    let sample_button = Button {
        button_text = "Sample",
        button_x = fromIntegral screen_width / 2.0,
        button_y = fromIntegral screen_height - 200.0,
        button_width = 100.0,
        button_height = 100.0,
        button_color = "black",
        button_on_click =
             \event ->
                updateScore game_state_ref score_text target_text distr_text histogram_sprite
    }
    consoleLogShow "rendering button"
    renderButton app sample_button
    return ()
