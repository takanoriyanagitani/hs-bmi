{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Bmi.Core.Mass (
    MassUnit (..),
    str2massUnit,
    string2massNum,
    strings2mass,
    pair2mass,
) where

import Bmi.Core.Error (InternalBmiError (..), ErrorCategory (..), MassKg (..), getKg)
import Data.Text qualified as T
import Text.Read (readMaybe)

-- | Supported mass units.
data MassUnit
    = Kg
    | Mg
    | G
    deriving (Eq, Show)

-- | Parses a text string into a MassUnit.
str2massUnit :: T.Text -> Either (InternalBmiError 'WeightError) MassUnit
str2massUnit "Kg" = Right Kg
str2massUnit "kg" = Right Kg
str2massUnit "Mg" = Right Mg
str2massUnit "mg" = Right Mg
str2massUnit "G" = Right G
str2massUnit "g" = Right G
str2massUnit s = Left (InvalidMassUnitErr s)

-- | Converts a value and its unit into MassKg.
pair2mass :: MassUnit -> Double -> MassKg
pair2mass Kg v = MassKg v
pair2mass G gval = MassKg (gval / 1000.0)
pair2mass Mg mval = MassKg (mval / 1000.0 / 1000.0)

-- | Parses a numeric string into a mass value (Double).
string2massNum :: T.Text -> Either (InternalBmiError 'WeightError) Double
string2massNum sweight = case readMaybe (T.unpack sweight) of
    Just val -> Right val
    Nothing -> Left (InvalidMassValueErr sweight)

-- | Parses value and unit strings into a MassKg.
strings2mass :: T.Text -> T.Text -> Either (InternalBmiError 'WeightError) MassKg
strings2mass vStr uStr = do
    v <- string2massNum vStr
    u <- str2massUnit uStr
    let m = pair2mass u v
    if getKg m < 0
        then Left NegativeMassErr
        else Right m
