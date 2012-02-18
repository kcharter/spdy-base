module Main where

import Test.Framework (defaultMain)

import qualified RawFrameTests as RFT

main :: IO ()
main = defaultMain [RFT.test]
