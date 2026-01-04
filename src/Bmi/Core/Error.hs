{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Bmi.Core.Error (
    ErrorCategory (..),
    InternalBmiError (..),
    toBmiError,
    BmiError (..),
    MassKg (..),
    LengthMeter (..),
) where

import Data.Text qualified as T

-- | Represents a raw mass in kilograms.
newtype MassKg = MassKg {getKg :: Double}
    deriving (Eq, Show)

-- | Represents a raw length in meters.
newtype LengthMeter = LengthMeter {getMeter :: Double}
    deriving (Eq, Show)

-- | Category of errors that can occur during BMI processing.
data ErrorCategory
    = WeightError
    | HeightError
    | LtsvError
    | InputError
    deriving (Eq, Show)

-- | Internal GADT for categorized BMI errors.
data InternalBmiError (c :: ErrorCategory) where
    TooHeavyWeight :: Double -> InternalBmiError 'WeightError
    ZeroWeightErr :: InternalBmiError 'WeightError
    NegativeMassErr :: InternalBmiError 'WeightError
    InvalidMassValueErr :: T.Text -> InternalBmiError 'WeightError
    InvalidMassUnitErr :: T.Text -> InternalBmiError 'WeightError
    TooTallHeight :: Double -> InternalBmiError 'HeightError
    ZeroHeightErr :: InternalBmiError 'HeightError
    NegativeLengthErr :: InternalBmiError 'HeightError
    InvalidLengthValueErr :: T.Text -> InternalBmiError 'HeightError
    InvalidLengthUnitErr :: T.Text -> InternalBmiError 'HeightError
    InvalidNumberErr :: T.Text -> InternalBmiError 'LtsvError
    UnableToParseErr :: T.Text -> InternalBmiError 'LtsvError
    InvalidInputErr :: BmiError -> InternalBmiError 'InputError

deriving instance Show (InternalBmiError c)

deriving instance Eq (InternalBmiError c)

-- | Public error type representing BMI validation and parsing failures.
data BmiError
    = InvalidWeight T.Text
    | InvalidHeight T.Text
    | InvalidNumber T.Text
    | InvalidWeightUnit T.Text
    | InvalidHeightUnit T.Text
    | ZeroHeight
    | ZeroWeight
    | NegativeHeight
    | NegativeWeight
    | TooHeavy MassKg
    | TooTall LengthMeter
    | UnableToParse T.Text
    deriving (Eq, Show)

-- | Converts an internal error to the public BmiError type.
toBmiError :: InternalBmiError c -> BmiError
toBmiError (TooHeavyWeight v) = TooHeavy (MassKg v)
toBmiError ZeroWeightErr = ZeroWeight
toBmiError NegativeMassErr = NegativeWeight
toBmiError (InvalidMassValueErr s) = InvalidWeight s
toBmiError (InvalidMassUnitErr s) = InvalidWeightUnit s
toBmiError (TooTallHeight v) = TooTall (LengthMeter v)
toBmiError ZeroHeightErr = ZeroHeight
toBmiError NegativeLengthErr = NegativeHeight
toBmiError (InvalidLengthValueErr s) = InvalidHeight s
toBmiError (InvalidLengthUnitErr s) = InvalidHeightUnit s
toBmiError (InvalidNumberErr s) = InvalidNumber s
toBmiError (UnableToParseErr s) = UnableToParse s
toBmiError (InvalidInputErr e) = e

