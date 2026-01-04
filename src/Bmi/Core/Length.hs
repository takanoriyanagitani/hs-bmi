{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Bmi.Core.Length (
    LengthUnit (..),
    str2lengthUnit,
    string2lengthNum,
    strings2length,
    pair2length,
) where

import Bmi.Core.Error (InternalBmiError (..), ErrorCategory (..), LengthMeter (..), getMeter)
import Data.Text qualified as T
import Text.Read (readMaybe)

-- | Supported length units.
data LengthUnit
    = M
    | Cm
    | Mm
    deriving (Eq, Show)

-- | Parses a text string into a LengthUnit.
str2lengthUnit :: T.Text -> Either (InternalBmiError 'HeightError) LengthUnit
str2lengthUnit "M" = Right M
str2lengthUnit "m" = Right M
str2lengthUnit "Cm" = Right Cm
str2lengthUnit "cm" = Right Cm
str2lengthUnit "Mm" = Right Mm
str2lengthUnit "mm" = Right Mm
str2lengthUnit s = Left (InvalidLengthUnitErr s)

-- | Parses a numeric string into a length value (Double).
string2lengthNum :: T.Text -> Either (InternalBmiError 'HeightError) Double
string2lengthNum sheight = case readMaybe (T.unpack sheight) of
    Just val -> Right val
    Nothing -> Left (InvalidLengthValueErr sheight)

-- | Converts a value and its unit into LengthMeter.
pair2length :: LengthUnit -> Double -> LengthMeter
pair2length M v = LengthMeter v
pair2length Cm cval = LengthMeter (cval / 100.0)
pair2length Mm mval = LengthMeter (mval / 1000.0)

-- | Parses value and unit strings into a LengthMeter.
strings2length :: T.Text -> T.Text -> Either (InternalBmiError 'HeightError) LengthMeter
strings2length vStr uStr = do
    v <- string2lengthNum vStr
    u <- str2lengthUnit uStr
    let l = pair2length u v
    if getMeter l < 0
        then Left NegativeLengthErr
        else Right l
