module Main where

import Test.Framework (defaultMain)

import qualified RawFrameTests as RFT
import qualified FrameTests as FT


main :: IO ()
main = defaultMain [ RFT.test
                   , FT.test ]
