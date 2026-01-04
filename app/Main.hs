module Main (main) where

import Bmi
import Data.Text qualified as T
import Data.Text.IO qualified as TIO

stdin2text :: IO T.Text
stdin2text = TIO.getLine

text2stdout :: T.Text -> IO ()
text2stdout = TIO.putStrLn

main :: IO ()
main = stdin2text >>= text2stdout . ltsv2ltsv
