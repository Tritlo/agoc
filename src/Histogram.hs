{-# LANGUAGE BangPatterns #-}

-- | Histogram and distribution sampling utilities
module Histogram
    ( -- * Distribution types
      Dist(..)
    , validateDist
    , sampleDist
    , sampleDists
    , showDists
      -- * Histogram
    , Histogram(..)
    , histogram
      -- * Other distributions
    , poisson
    , simplifyDists
    ) where

import qualified System.Random.SplitMix.Distributions as D
import Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import Data.Aeson (ToJSON(..))
import Data.List (partition)

-- | Distribution types for sampling
data Dist = Uniform {low :: Double, high :: Double}
          | Exponential {lambda :: Double}
          | Gamma {k :: Double, theta :: Double}
          | Normal {mean :: Double, stddev :: Double}
  deriving (Eq, Ord)

instance Show Dist where
    show (Uniform a b) = "𝒰([" ++ show a ++ ", " ++ show b ++ "])"
    show (Exponential lam) = "Exp(λ=" ++ show lam ++ ")"
    show (Gamma k' theta') = "Γ(k=" ++ show k' ++ ", θ=" ++ show theta' ++ ")"
    show (Normal mu sigma) = "𝒩(μ=" ++ show mu ++ ", σ²=" ++ show sigma ++ ")"

-- | Validate that distribution parameters are valid
validateDist :: Dist -> Bool
validateDist (Uniform a b) = a < b
validateDist (Exponential lam) = lam > 0
validateDist (Gamma k' theta') = k' > 0 && theta' > 0
validateDist (Normal _mean sig) = sig > 0

-- | Sample from a single distribution
sampleDist :: Monad m => Dist -> D.GenT m Double
sampleDist (Uniform lo hi) = D.uniformR lo hi
sampleDist (Exponential theta') = D.exponential theta'
sampleDist (Gamma k' theta') = D.gamma k' theta'
sampleDist (Normal mu sig) = D.normal mu sig

-- | Sample from a sequence of distributions and sum the results
sampleDists :: Monad m => Seq Dist -> D.GenT m Double
sampleDists dists = do
    case Seq.viewl dists of
        Seq.EmptyL -> return 0.0
        first Seq.:< rest -> do
            x <- sampleDist first
            xs <- sampleDists rest
            return (x + xs)

-- | Show a sequence of distributions as a sum
showDists :: Seq Dist -> String
showDists dists = case Seq.viewl dists of
    Seq.EmptyL -> ""
    first Seq.:< rest -> show first ++ foldMap (\dist -> " + " ++ show dist) rest

-- | Sample from a Poisson distribution
poisson :: Monad m => Double -> D.GenT m Int
poisson lam = go 0 0
  where go !t !k' = do
          t' <- (t +) <$> D.exponential lam
          if t' > 1
          then return k'
          else go t' (k' + 1)

-- | Histogram data type
newtype Histogram = Histogram [(Int, Double)]

instance ToJSON Histogram where
    toJSON (Histogram bins) = toJSON bins

-- | Generate a histogram from samples of a distribution sequence
histogram :: Int -> Int -> Seq Dist -> IO Histogram
histogram num_samples numBins dists = do
    sampleData <- D.samplesIO num_samples $ sampleDists dists
    let (h_min, h_max) = (minimum sampleData, maximum sampleData)
    let bin_width = (h_max - h_min) / fromIntegral numBins
    let makeBins :: Double -> Double -> [Double] -> [(Int, Double)]
        makeBins !_acc !_cutoff [] = []
        makeBins !acc !cutoff !remaining =
            let (lt, rest) = partition (<= cutoff) remaining
                len_lt = length lt
                acc' = acc + fromIntegral len_lt
                cutoff' = cutoff + bin_width
            in  (len_lt, cutoff) : makeBins acc' cutoff' rest
        binData = makeBins 0.0 (h_min + bin_width) sampleData
    return (Histogram binData)

-- | Simplify distributions by combining like terms
-- TODO: implement actual simplification (e.g., N(500,100) + N(500,100) -> N(1000,200))
simplifyDists :: Seq Dist -> Seq Dist
simplifyDists dists = dists
