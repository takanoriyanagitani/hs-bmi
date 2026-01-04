{-# LANGUAGE DataKinds #-}

module Bmi.Core.Dto (
    Input (..),
    Output (..),
    InputDto (..),
    OutputDto (..),
    ResponseDto (..),
    dto2input,
    dto2output,
    result2response,
    input2output,
) where


import Bmi.Core.Error
import Bmi.Core.Domain
import Data.Text qualified as T

-- | Domain input grouping validated Weight and Height.
data Input = Input
    { inputWeight :: Weight
    , inputHeight :: Height
    }
    deriving (Eq, Show)

-- | Domain output grouping calculated BMI value and status.
data Output = Output
    { outputBmi :: BmiValue
    , outputStatus :: BmiStatus
    }
    deriving (Eq, Show)

-- | Raw string-based input DTO.
data InputDto = InputDto
    { dtoWeightVal :: T.Text
    , dtoWeightUnit :: T.Text
    , dtoHeightVal :: T.Text
    , dtoHeightUnit :: T.Text
    }
    deriving (Eq, Show)

-- | Formatted string-based output DTO.
data OutputDto = OutputDto
    { dtoBmiVal :: T.Text
    , dtoStatusVal :: T.Text
    }
    deriving (Eq, Show)

-- | Unified response structure for success and error cases.
data ResponseDto = ResponseDto
    { resBmi :: Maybe BmiValue
    , resStatus :: Maybe BmiStatus
    , resError :: Maybe BmiError
    }
    deriving (Eq, Show)

-- | Wraps a BMI result or error into a ResponseDto.
result2response :: Either BmiError Output -> ResponseDto
result2response (Right (Output b s)) = ResponseDto (Just b) (Just s) Nothing
result2response (Left e) = ResponseDto Nothing Nothing (Just e)

-- | Converts an InputDto to validated domain Input.
dto2input :: InputDto -> Either (InternalBmiError 'InputError) Input
dto2input (InputDto wv wu hv hu) = do
    w <- case strings2weight wv wu of
        Right weight -> Right weight
        Left err -> Left (InvalidInputErr $ toBmiError err)
    h <- case strings2height hv hu of
        Right height -> Right height
        Left err -> Left (InvalidInputErr $ toBmiError err)
    pure $ Input w h

-- | Full pipeline: InputDto to domain Output.
dto2output :: InputDto -> Either (InternalBmiError 'InputError) Output
dto2output dto = fmap input2output (dto2input dto)

-- | Orchestrates the transition from domain Input to Output.
input2output :: Input -> Output
input2output (Input w h) =
    let bval = computeBmi w h
        stat = bmi2status bval
     in Output bval stat
