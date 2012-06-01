module Network.SPDY.Stream (Stream,
                            StreamContent,
                            moreHeaders,
                            moreData,
                            isLast,
                            forContent,
                            NewStreamOpts(..),
                            newStream,
                            addIncomingData,
                            addIncomingHeaders,
                            updateWindowSize
                            ) where

import Control.Concurrent.MSemN (MSemN)
import qualified Control.Concurrent.MSemN as MSemN
import Control.Monad (when)
import Data.ByteString (ByteString)

import Network.SPDY.Frames (StreamID, Priority, HeaderBlock, DeltaWindowSize, TerminationStatus)
import Network.SPDY.Internal.BoundedBuffer (BoundedBuffer, Sized(..))
import qualified Network.SPDY.Internal.BoundedBuffer as BB

-- | State associated with a particular stream.
data Stream =
  Stream { ssStreamID :: StreamID
           -- ^ The stream ID for this stream.
         , ssPriority :: Priority
           -- ^ The priority for data frames sent on this stream.
         , ssOutgoingWindowSize :: MSemN Int
           -- ^ A quantity semaphore representing the flow control
           -- window size on the remote endpoint.
         , ssIncomingBuffer :: BoundedBuffer StreamContent
           -- ^ A buffer for incoming stream contents.
         }

-- | The various kinds of content that can arrive on or be sent on a stream.
data StreamContent = StreamContent !(Either HeaderBlock ByteString) !Bool
                   deriving (Eq, Show, Read)

-- | Creates stream content from a header block.
moreHeaders :: HeaderBlock -> Bool -> StreamContent
moreHeaders headers last = StreamContent (Left headers) last

-- | Creates stream content from a chunk of data.
moreData :: ByteString -> Bool -> StreamContent
moreData bytes last = StreamContent (Right bytes) last

-- | Indicates whether this is the last content on the stream from the originating side.
isLast :: StreamContent -> Bool
isLast (StreamContent _ b) = b

-- | Processes stream content.
forContent :: (HeaderBlock -> Bool -> a) -> (ByteString -> Bool -> a) -> StreamContent -> a
forContent forHeaders forData (StreamContent c last) =
  either forHeaders forData c last

instance Sized StreamContent where
  size (StreamContent c _) = either (const 0) size c


data NewStreamOpts =
  NewStreamOpts { nsoInitialWindowSize :: Int
                , nsoPriority :: Priority
                , nsoHalfClosed :: Bool
                , nsoOutgoingHandler :: StreamContent -> IO ()
                , nsoWindowUpdateHandler :: DeltaWindowSize -> IO ()
                , nsoResetHandler :: TerminationStatus -> IO ()
                }

newStream :: StreamID -> NewStreamOpts -> IO (Stream, Maybe (StreamContent -> IO ()), IO StreamContent)
newStream sid nso = do
  let dws = nsoInitialWindowSize nso
      priority = nsoPriority nso
      halfClosed = nsoHalfClosed nso
  dwsSem <- MSemN.new dws
  buf <- BB.new dws
  let s = Stream sid priority dwsSem buf
  return (s, if halfClosed then Nothing else Just (contentPusher s), contentPuller s)
  where contentPusher s content = do
          -- TODO: if there is an exception, we should signal an
          -- internal error to the remote endpoint, and tear down the
          -- stream
          -- TODO: if we have previously seen the end of the content,
          -- then we should throw an exception
          MSemN.wait (ssOutgoingWindowSize s) (size content)
          nsoOutgoingHandler nso content
        contentPuller s = do
          content <- BB.remove (ssIncomingBuffer s)
          let delta = size content
          when (delta > 0) (nsoWindowUpdateHandler nso (fromIntegral delta))
          return content

updateWindowSize :: Stream -> DeltaWindowSize -> IO ()
updateWindowSize s delta =
  MSemN.signal (ssOutgoingWindowSize s) (fromIntegral delta)

addIncomingData :: Stream -> ByteString -> Bool -> IO Bool
addIncomingData s bytes last = BB.tryAdd (ssIncomingBuffer s) (moreData bytes last)

addIncomingHeaders :: Stream -> HeaderBlock -> Bool -> IO ()
addIncomingHeaders s headers last = BB.add (ssIncomingBuffer s) (moreHeaders headers last)
