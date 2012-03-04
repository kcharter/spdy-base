module Main where

import Test.Framework (defaultMain)

import qualified RawFrameTests as RFT
import qualified BuildParseTests as BPT
import qualified FrameTests as FT


main :: IO ()
main = defaultMain [ RFT.test
                   , BPT.test
                   , FT.test ]
