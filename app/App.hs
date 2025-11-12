{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE LambdaCase #-}

module Main where
import Lib
import GHC.Wasm.Prim
import Data.String (IsString(..))
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Control.Monad (when)
import qualified System.Random.SplitMix.Distributions as D
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq

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

renderButton :: PixiApp -> Button -> IO ()
renderButton app button = do
    button_text <- newText (toJSString button.button_text) (toJSString button.button_color)
    setProperty "eventMode" button_text (stringAsVal "static")
    setAnchor button_text 0.5
    setProperty "x" button_text (floatAsVal button.button_x)
    setProperty "y" button_text (floatAsVal button.button_y)
    addEventListener "pointerdown" button_text =<< jsFuncFromHs_ button.button_on_click
    exportValue "SAMPLE" button_text

    addEventListener "pointerover" button_text =<< jsFuncFromHs_
         (\_ -> do setPropertyKey ["style", "fill"] button_text (stringAsVal "blue"))
    addEventListener "pointerout" button_text =<< jsFuncFromHs_
         (\_ -> do setPropertyKey ["style", "fill"] button_text (stringAsVal "black"))


    addChild app button_text

data GameState = GameState {
    gs_score :: Integer,
    gs_dists :: Seq Dist,
    gs_target :: Integer
} deriving (Show, Eq)


data Dist = Uniform {low :: Double, high :: Double}
          | Beta {alpha :: Double, beta :: Double}
          | Exponential {lambda :: Double}
          | Gamma {k :: Double, theta :: Double}
          | Normal {mean :: Double, stddev :: Double}
          | Bernoulli {p :: Double}
          | Binomial {n :: Int, p :: Double}
          | Poisson {lambda :: Double}
  deriving (Eq)
-- Note Beta(1,1) is Uniform(0,1)
-- Note Gamma(1,1) is Exponential(1)
-- Note Binomial(1,p) is Bernoulli(p)

instance Show Dist where
    show (Uniform a b) = "U(a=" ++ show a ++ ", b=" ++ show b ++ ")"
    show (Beta a b) = "Beta(α=" ++ show a ++ ", β=" ++ show b ++ ")"
    show (Exponential lambda) = "Exp(λ=" ++ show lambda ++ ")"
    show (Gamma k theta) = "Gamma(k=" ++ show k ++ ", θ=" ++ show theta ++ ")"
    show (Normal mean stddev) = "N(μ=" ++ show mean ++ ", σ=" ++ show stddev ++ ")"
    show (Bernoulli p) = "Bern(p=" ++ show p ++ ")"
    show (Binomial n p) = "Bin(n=" ++ show n ++ ", p=" ++ show p ++ ")"
    show (Poisson lambda) = "Poisson(λ=" ++ show lambda ++ ")"

instance Eq Dist where
    (Uniform a b) == (Uniform c d) = a == c && b == d
    (Beta a b) == (Beta c d) = a == c && b == d
    (Exponential lambda) == (Exponential lambda') = lambda == lambda'
    (Gamma k theta) == (Gamma k' theta') = k == k' && theta == theta'
    (Normal mean stddev) == (Normal mean' stddev') = mean == mean' && stddev == stddev'
    (Bernoulli p) == (Bernoulli p') = p == p'
    (Binomial n p) == (Binomial n' p') = n == n' && p == p'
    (Poisson lambda) == (Poisson lambda') = lambda == lambda'

validateDist :: Dist -> Bool
validateDist (Uniform a b) = a < b
validateDist (Beta a b) = a > 0 && b > 0
validateDist (Exponential lambda) = lambda > 0
validateDist (Gamma k theta) = k > 0 && theta > 0
validateDist (Normal mean stddev) = stddev > 0
validateDist (Bernoulli p) = p >= 0 && p <= 1
validateDist (Binomial n p) = n > 0 && p >= 0 && p <= 1
validateDist (Poisson lambda) = lambda > 0


sampleDist :: Monad m => Dist -> D.GenT m Double
sampleDist (Uniform low high) = D.uniformR low high
sampleDist (Beta alpha beta) = D.beta alpha beta
sampleDist (Exponential theta) = D.exponential theta
sampleDist (Gamma k theta) = D.gamma k theta
sampleDist (Normal mean stddev) = D.normal mean stddev
sampleDist (Bernoulli p) = (\b -> if b then 1.0 else 0.0) <$> D.bernoulli p
sampleDist (Binomial n p) = do
    maybe 0.0 (fromIntegral . sum) <$> D.multinomial n (replicate n p)
sampleDist (Poisson lambda) = fromIntegral <$> poisson lambda

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

-- | Todo: here we should simplify the distributions by combining like terms,
-- e.g. N(500,100) + N(500,100) -> N(1000,200)
simplifyDists :: Seq Dist -> Seq Dist
simplifyDists dists = dists

updateScore :: IORef GameState -> PixiText -> PixiText -> PixiText -> IO ()
updateScore game_state_ref score_text target_text distr_text = do
    game_state@GameState{..} <- readIORef game_state_ref
    sample <- D.sampleIO $ sampleDists gs_dists

    let new_score = gs_score + round sample
    setProperty "text" score_text (stringAsVal $ toJSString $ "Score: " ++ show new_score)
    if new_score >= gs_target then do
        blipWithFreq 800.0
        let new_target = gs_target * 2
        let new_dists = gs_dists :|> Normal 500.0 100.0
        setProperty "text" target_text (stringAsVal $ toJSString $ "Target: " ++ show (new_target))
        setProperty "text" distr_text (stringAsVal $ toJSString $ "X ~ " ++ showDists (Seq.fromList [Normal 500.0 100.0]))
        writeIORef game_state_ref (game_state { gs_score = 0, gs_target = new_target, gs_dists = new_dists })
    else
        writeIORef game_state_ref (game_state { gs_score = new_score })

main :: IO ()
main = do
    -- Initialize PIXI application
    app <- newApp >>= flip initApp "white"
    appendCanvas app
    screen <- getProperty "screen" app
    screen_width <- valAsInt <$> getProperty "width" screen
    screen_height <- valAsInt <$> getProperty "height" screen
    game_state_ref <- newIORef GameState {
                             gs_score = 0,
                             gs_dists = Seq.fromList [Normal 500.0 100.0],
                             gs_target = 10_000
                         }
    target_text <- newText (toJSString "Target: 10000") "black"
    setProperty "x" target_text (floatAsVal $ fromIntegral screen_width / 2.0)
    setProperty "y" target_text (floatAsVal $ (fromIntegral screen_height / 2.0) - 100.0)
    setAnchor target_text 0.5
    addChild app target_text

    score_text <- newText (toJSString "Score: 0") "black"
    setProperty "x" score_text (floatAsVal $ fromIntegral screen_width / 2.0)
    setProperty "y" score_text (floatAsVal $ fromIntegral screen_height / 2.0)
    setAnchor score_text 0.5
    addChild app score_text

    distr_text <- newText (toJSString $ "X ~ " ++ showDists (Seq.fromList [Normal 500.0 100.0])) "black"
    setProperty "x" distr_text (floatAsVal $ fromIntegral screen_width / 2.0)
    setProperty "y" distr_text (floatAsVal $ (fromIntegral screen_height / 2.0) + 100.0)
    setAnchor distr_text 0.5
    addChild app distr_text

    let sample_button = Button {
        button_text = "Sample",
        button_x = fromIntegral screen_width / 2.0,
        button_y = fromIntegral screen_height - 200.0,
        button_width = 100.0,
        button_height = 100.0,
        button_color = "black",
        button_on_click =
             \event ->
                updateScore game_state_ref score_text target_text distr_text
    }
    consoleLogShow "rendering button"
    renderButton app sample_button
    return ()
