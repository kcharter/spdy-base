module BuildParseTests (test) where

import Blaze.ByteString.Builder (Builder, toByteString)
import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Data.Attoparsec.ByteString (Parser, parseOnly, endOfInput)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Network.SPDY.Frames
import Network.SPDY.Internal.Deserialize
import Network.SPDY.Internal.Serialize

import Instances

test :: Test
test = testGroup "Build-parse tests"
       [ testProperty "Raw frame header" prop_buildParseRawFrameHeader
       , testProperty "DataLength" prop_buildParseDataLength
       , testProperty "StreamID" prop_buildParseStreamID
       , testProperty "HeaderCount" prop_buildParseHeaderCount
       , testProperty "HeaderName" prop_buildParseHeaderName
       , testProperty "HeaderValue" prop_buildParseHeaderValue
       , testProperty "Priority" prop_buildParsePriority
       , testProperty "Slot" prop_buildParseSlot
       , testProperty "TerminationStatus" prop_buildParseTerminationStatus ]

prop_buildParseRawFrameHeader :: RawFrameHeader -> Bool
prop_buildParseRawFrameHeader = prop_buildParse rawHeaderBuilder parseFrameHeader

prop_buildParseDataLength :: DataLength -> Bool
prop_buildParseDataLength = prop_buildParse toBuilder parseDataLength

prop_buildParseStreamID :: StreamID -> Bool
prop_buildParseStreamID = prop_buildParse toBuilder parseStreamID

prop_buildParsePriority :: Priority -> Bool
prop_buildParsePriority = prop_buildParse toBuilder parsePriority

prop_buildParseSlot :: Slot -> Bool
prop_buildParseSlot = prop_buildParse toBuilder parseSlot

prop_buildParseHeaderCount :: HeaderCount -> Bool
prop_buildParseHeaderCount = prop_buildParse toBuilder parseHeaderCount

prop_buildParseHeaderName :: HeaderName -> Bool
prop_buildParseHeaderName = prop_buildParse toBuilder parseHeaderName

prop_buildParseHeaderValue :: HeaderValue -> Bool
prop_buildParseHeaderValue = prop_buildParse toBuilder parseHeaderValue

prop_buildParseTerminationStatus :: TerminationStatus -> Bool
prop_buildParseTerminationStatus = prop_buildParse toBuilder parseTerminationStatus

prop_buildParse :: Eq a => (a -> Builder) -> Parser a -> a -> Bool
prop_buildParse builderFor parser =
  prop_serializeParse (toByteString . builderFor) (parseOnly parser')
    where parser' = do r <- parser; endOfInput; return r

prop_serializeParse :: Eq a => (a -> ByteString) -> (ByteString -> Either String a) -> a -> Bool
prop_serializeParse serialize parse x =
  either (const False) (x ==) $ parse $ serialize x
