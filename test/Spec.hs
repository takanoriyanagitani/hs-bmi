{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Bmi
import System.Exit (exitFailure)

-- Simple test runner
assertEq :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEq label expected actual =
    if expected == actual
        then putStrLn $ "PASSED: " ++ label
        else do
            putStrLn $ "FAILED: " ++ label
            putStrLn $ "  Expected: " ++ show expected
            putStrLn $ "  Actual:   " ++ show actual
            exitFailure

testBmiCalc :: IO ()
testBmiCalc = do
    let w = Weight 70.0
    let h = Height 1.75
    let bval = computeBmi w h
    -- 70 / (1.75 * 1.75) = 22.857...
    assertEq "BMI calculation" 22.857142857142858 (getBmi bval)

testBmiValueToInt :: IO ()
testBmiValueToInt = do
    assertEq "bmiValue2Int 22.857" 23 (bmiValue2Int (BmiValue 22.857))
    assertEq "bmiValue2Int 22.4" 22 (bmiValue2Int (BmiValue 22.4))
    assertEq "bmiValue2Int 22.5" 23 (bmiValue2Int (BmiValue 22.5))

testStrings2Mass :: IO ()
testStrings2Mass = do
    assertEq "strings2mass 70 Kg" (Right (MassKg 70.0)) (strings2mass "70.0" "Kg")
    assertEq "strings2mass 70000 G" (Right (MassKg 70.0)) (strings2mass "70000" "g")
    assertEq "strings2mass -1 Kg" (Left NegativeMassErr) (strings2mass "-1.0" "Kg")

testStrings2Length :: IO ()
testStrings2Length = do
    assertEq "strings2length 1.75 M" (Right (LengthMeter 1.75)) (strings2length "1.75" "M")
    assertEq "strings2length 175 Cm" (Right (LengthMeter 1.75)) (strings2length "175" "Cm")
    assertEq "strings2length -1 M" (Left NegativeLengthErr) (strings2length "-1.0" "M")

testMassUnit :: IO ()
testMassUnit = do
    assertEq "str2massUnit Kg" (Right Kg) (str2massUnit "Kg")
    assertEq "str2massUnit G" (Right G) (str2massUnit "g")

testLengthUnit :: IO ()
testLengthUnit = do
    assertEq "str2lengthUnit M" (Right M) (str2lengthUnit "M")
    assertEq "str2lengthUnit Cm" (Right Cm) (str2lengthUnit "cm")

testLtsvParsing :: IO ()
testLtsvParsing = do
    let line = "height:1.75\thunit:M\tweight:70.0\twunit:Kg"
    let expected = Right (InputDto "70.0" "Kg" "1.75" "M")
    assertEq "ltsvLine2dto" expected (ltsvLine2dto line)

testFullPipeline :: IO ()
testFullPipeline = do
    let line = "height:1.75\thunit:M\tweight:70.0\twunit:Kg"
    let expected = "status:normal\tbmi:23"
    assertEq "ltsv2ltsv" expected (ltsv2ltsv line)

main :: IO ()
main = do
    testBmiCalc
    testBmiValueToInt
    testStrings2Mass
    testStrings2Length
    testMassUnit
    testLengthUnit
    testLtsvParsing
    testFullPipeline
    putStrLn "All tests passed!"
