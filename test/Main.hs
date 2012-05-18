module Main where

import Test.Framework (defaultMain)

import qualified RawFrameTests as RFT
import qualified BuildParseTests as BPT
import qualified FrameTests as FT
import qualified PriorityChanTests as PCT
import qualified BoundedBufferTests as BBT

main :: IO ()
main = defaultMain [ RFT.test
                   , BPT.test
                   , FT.test
                   , PCT.test
                   , BBT.test ]
