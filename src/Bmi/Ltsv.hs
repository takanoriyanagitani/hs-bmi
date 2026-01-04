{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Bmi.Ltsv
Description : LTSV parsing and serialization for BMI data.
Copyright   : (c) takanori.yanagitani, 2026
License     : Apache-2.0
Maintainer  : takanori.yanagitani@gmail.com

This module provides functions to parse LTSV strings into BMI input DTOs
and serialize BMI output DTOs back into LTSV format.
-}
module Bmi.Ltsv (
    LtsvValue (..),
    LtsvLabel (..),
    LtsvPair,
    ltsv2dto,
    string2strings,
    strings2pairs,
    string2pair,
    string2pairs,
    ltsvLine2dto,
    ltsvLine2output,
    response2ltsv,
    ltsv2ltsv,
    status2val,
) where

import Bmi.Core
import Data.Text qualified as T

-- | Represents a raw value from an LTSV field.
newtype LtsvValue = LtsvValue {getValue :: T.Text}
    deriving (Eq, Show)

-- | Represents a label from an LTSV field.
newtype LtsvLabel = LtsvLabel {getLabel :: T.Text}
    deriving (Eq, Show)

-- | A single label-value pair from an LTSV record.
type LtsvPair = (LtsvLabel, LtsvValue)

-- | Formats BMI status into an LTSV-safe string.
status2val :: BmiStatus -> T.Text
status2val UnderCritical = "under_critical"
status2val UnderWarning = "under_warning"
status2val UnderNormal = "under_normal"
status2val Normal = "normal"
status2val OverNormal = "over_normal"
status2val OverWarning = "over_warning"
status2val OverCritical = "over_critical"
status2val OverFatal = "over_fatal"


bmi2labeled :: BmiValue -> T.Text
bmi2labeled b = T.append "bmi:" (T.pack $ show $ bmiValue2Int b)

status2labeled :: BmiStatus -> T.Text
status2labeled s = T.append "status:" (status2val s)

-- | Formats a BmiError into an LTSV string.
error2ltsv :: BmiError -> T.Text
error2ltsv e = T.append "error:" (error2str e)

-- | Converts a BmiError into a safe string for LTSV logging.
error2str :: BmiError -> T.Text
error2str (InvalidWeight _) = "invalid_weight"
error2str (InvalidHeight _) = "invalid_height"
error2str (InvalidNumber _) = "invalid_number"
error2str (InvalidWeightUnit _) = "invalid_weight_unit"
error2str (InvalidHeightUnit _) = "invalid_height_unit"
error2str ZeroHeight = "zero_height"
error2str ZeroWeight = "zero_weight"
error2str NegativeHeight = "negative_height"
error2str NegativeWeight = "negative_weight"
error2str (TooHeavy (MassKg v)) = T.concat ["too_heavy\trejected_weight:", T.pack $ show v]
error2str (TooTall (LengthMeter v)) = T.concat ["too_tall\trejected_height:", T.pack $ show v]
error2str (UnableToParse _) = "ltsv_error"

-- | Formats a ResponseDto into a single LTSV line.
response2ltsv :: ResponseDto -> T.Text
response2ltsv (ResponseDto (Just b) (Just s) _) =
    T.concat [status2labeled s, "\t", bmi2labeled b]
response2ltsv (ResponseDto _ _ (Just e)) = error2ltsv e
response2ltsv _ = "error:Unknown internal error"

-- | Converts an internal error to a public BmiError.
mapInternalError :: Either (InternalBmiError c) a -> Either BmiError a
mapInternalError (Left e) = Left (toBmiError e)
mapInternalError (Right r) = Right r

-- | Parses a raw LTSV line into a public Either BmiError domain Output.
ltsvLine2output :: T.Text -> Either BmiError Output
ltsvLine2output l = do
    idto <- mapInternalError (ltsvLine2dto l)
    mapInternalError (dto2output idto)

-- | The full pure pipeline: raw LTSV line to raw LTSV result.
ltsv2ltsv :: T.Text -> T.Text
ltsv2ltsv = response2ltsv . result2response . ltsvLine2output

-- | Converts a list of label-value pairs into an InputDto.
ltsv2dto :: [LtsvPair] -> Either (InternalBmiError 'LtsvError) InputDto
ltsv2dto pairs = do
    wv <- findValue "weight" pairs
    wu <- findValue "wunit" pairs
    hv <- findValue "height" pairs
    hu <- findValue "hunit" pairs
    pure $ InputDto wv wu hv hu

-- | Convenience: raw line string to InputDto.
ltsvLine2dto :: T.Text -> Either (InternalBmiError 'LtsvError) InputDto
ltsvLine2dto s = ltsv2dto (string2pairs s)

-- | Splits a line into tab-separated fields.
string2strings :: T.Text -> [T.Text]
string2strings = T.splitOn "\t"

-- | Converts a list of raw strings into LtsvPairs.
strings2pairs :: [T.Text] -> [LtsvPair]
strings2pairs = map string2pair

-- | Parses a single "label:value" string into an LtsvPair.
string2pair :: T.Text -> LtsvPair
string2pair p = case T.splitOn ":" p of
    (l : v : _) -> (LtsvLabel l, LtsvValue v)
    (l : _) -> (LtsvLabel l, LtsvValue "")
    [] -> (LtsvLabel "", LtsvValue "")

-- | Full conversion: line string to LtsvPairs.
string2pairs :: T.Text -> [LtsvPair]
string2pairs = strings2pairs . string2strings

-- | Internal helper to find a value by label in a list of pairs.
findValue :: T.Text -> [LtsvPair] -> Either (InternalBmiError 'LtsvError) T.Text
findValue label pairs =
    case lookup (LtsvLabel label) pairs of
        Just (LtsvValue val) -> Right val
        Nothing -> Left (UnableToParseErr $ T.append "missing label: " label)
