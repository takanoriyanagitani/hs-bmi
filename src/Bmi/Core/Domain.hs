{-# LANGUAGE DataKinds #-}

module Bmi.Core.Domain (
    Weight (..),
    strings2weight,
    mass2weight,
    Height (..),
    strings2height,
    length2height,
    BmiValue (..),
    bmiValue2Int,
    BmiStatus (..),
    computeBmi,
    bmi2status,
    maxHeight,
    maxWeight,
) where

import Bmi.Core.Error (InternalBmiError (..), ErrorCategory (..), MassKg (..), getKg, LengthMeter (..), getMeter)
import Bmi.Core.Mass
import Bmi.Core.Length
import Data.Text qualified as T


-- | Represents a validated human height in meters.
newtype Height = Height {getHeight :: Double}
    deriving (Eq, Show)

-- | Represents a validated human weight in kilograms.
newtype Weight = Weight {getWeight :: Double}
    deriving (Eq, Show)

-- | Parses and validates value/unit strings into a human Weight.
strings2weight :: T.Text -> T.Text -> Either (InternalBmiError 'WeightError) Weight
strings2weight vStr uStr = do
    m <- strings2mass vStr uStr
    mass2weight m

-- | Parses and validates value/unit strings into a human Height.
strings2height :: T.Text -> T.Text -> Either (InternalBmiError 'HeightError) Height
strings2height vStr uStr = do
    l <- strings2length vStr uStr
    length2height l

-- | Maximum human height limit (meters).
maxHeight :: Double
maxHeight = 3.0

-- | Maximum human weight limit (kilograms).
maxWeight :: Double
maxWeight = 700.0

length2height :: LengthMeter -> Either (InternalBmiError 'HeightError) Height
length2height (LengthMeter v) | v == 0 = Left ZeroHeightErr
length2height (LengthMeter v) | v > maxHeight = Left (TooTallHeight v)
length2height l = Right (Height (getMeter l))

mass2weight :: MassKg -> Either (InternalBmiError 'WeightError) Weight
mass2weight (MassKg v) | v == 0 = Left ZeroWeightErr
mass2weight (MassKg v) | v > maxWeight = Left (TooHeavyWeight v)
mass2weight m = Right (Weight (getKg m))

-- | Represents a calculated BMI value.
newtype BmiValue = BmiValue {getBmi :: Double}
    deriving (Eq, Show)

-- | Converts BmiValue to rounded integer.
bmiValue2Int :: BmiValue -> Int
bmiValue2Int (BmiValue v) = floor (v + 0.5)

-- | Categorized BMI status (Normal, Overweight, etc.).
data BmiStatus
    = UnderCritical
    | UnderWarning
    | UnderNormal
    | Normal
    | OverNormal
    | OverWarning
    | OverCritical
    | OverFatal
    deriving (Eq, Show)

-- | Calculates BMI using validated human Weight and Height.
computeBmi :: Weight -> Height -> BmiValue
computeBmi w h = BmiValue $ getWeight w / (getHeight h * getHeight h)

-- | Maps a BMI value to its categorical status.
bmi2status :: BmiValue -> BmiStatus
bmi2status (BmiValue v) | v < 16.0 = UnderCritical
bmi2status (BmiValue v) | v < 17.0 = UnderWarning
bmi2status (BmiValue v) | v < 18.5 = UnderNormal
bmi2status (BmiValue v) | v < 25.0 = Normal
bmi2status (BmiValue v) | v < 30.0 = OverNormal
bmi2status (BmiValue v) | v < 35.0 = OverWarning
bmi2status (BmiValue v) | v < 40.0 = OverCritical
bmi2status _ = OverFatal
