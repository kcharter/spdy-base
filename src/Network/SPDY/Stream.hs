-- | A stream is a bi-directional flow of header blocks and data
-- chunks. SPDY flow control is implemented at the stream level, at
-- least for SPDY 3.
--
-- Content received from the remote endpoint is /incoming/ content,
-- and content sent to the remote endpoint is /outgoing/ content. At
-- each endpoint, there is a finite amount of buffer space available
-- for incoming data. The free space in the buffer is called the
-- /data/ /window/ /size/, and the total capacity of the buffer is the
-- /initial/ data window size. As an endpoint buffers incoming data,
-- the data window size shrinks. When buffered data is consumed by the
-- application layer, the endpoint signals the positive change in its
-- data window size by sending a WINDOW_UPDATE frame to its remote
-- peer.
--
-- In addition to buffering incoming data, an endpoint must track its
-- remote peer's data window size. Sending an outgoing chunk of data
-- decreases the outgoing data window size, and receiving a
-- WINDOW_UPDATE frame increases it. An endpoint must refrain from
-- sending a data chunk that exceeds the current output data window
-- size, and instead wait until the outgoing window enlarges as
-- WINDOW_UPDATE frames arrive from the remote peer.
--
--  A 'Stream' mediates between application-layer code that must read
-- and write content, and internal endpoint code that converts content
-- to and from frames and handles reading and writing on the network
-- connection with the remote endpoint. It helps implement flow
-- control by maintaining a bounded buffer for incoming data, and
-- tracking the outgoing data window size. In particular
--
-- * application-layer code sending outgoing data on the
-- stream will block on a chunk that exceeds the outgoing data window
-- size (see 'newStream');
--
-- * the stream will refuse to buffer data chunks that exceed
-- the incoming data window size (see 'addIncomingData').
--
-- A 'Stream' does not itself know how to send frames to or receive
-- frames from the remote endpoint. These operations are the
-- responsibility of the endpoint itself.

module Network.SPDY.Stream (
  -- * Streams and stream content
  Stream,
  StreamContent,
  moreHeaders,
  moreData,
  isLast,
  forContent,
  -- * Creating streams
  NewStreamOpts(..),
  newStream,
  -- * Operations of interest to endpoints
  addIncomingData,
  addIncomingHeaders,
  updateOutgoingWindowSize,
  -- * State snapshots
  snapshot
  ) where

import Control.Concurrent.MSemN (MSemN)
import qualified Control.Concurrent.MSemN as MSemN
import Control.Monad (when)
import Data.ByteString (ByteString)

import Network.SPDY.Frames (StreamID, HeaderBlock, DeltaWindowSize, TerminationStatus)
import Network.SPDY.Internal.BoundedBuffer (BoundedBuffer, Sized(..))
import qualified Network.SPDY.Internal.BoundedBuffer as BB

-- | State associated with a particular stream.
data Stream =
  Stream { ssStreamID :: StreamID
           -- ^ The stream ID for this stream.
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

-- | Processes stream content to yield some result.
--
-- For example, this can be used in an application layer thread that
-- pulls incoming content from the stream, or to turn outgoing content
-- into frames to send to the remote endpoint.
forContent :: (HeaderBlock -> Bool -> a)
              -- ^ Handles a header block. The second argument
              -- indicates whether this is the last content on the
              -- stream.
              -> (ByteString -> Bool -> a)
              -- ^ Handles a data chunk. The second argument indicates
              -- whether this is the last content on the stream.
              -> StreamContent
              -- ^ The content to process.
              -> a
              -- ^ The result of the operation.
forContent forHeaders forData (StreamContent c last) =
  either forHeaders forData c last

instance Sized StreamContent where
  size (StreamContent c _) = either (const 0) size c


-- | Options for creating streams.
data NewStreamOpts =
  NewStreamOpts { nsoInitialOutgoingWindowSize :: Int
                  -- ^ The initial data window size to use for flow control.
                , nsoIncomingBufferSize :: Int
                  -- ^ The size of the incoming data buffer, i.e. the
                  -- initial size of the incoming data window for the
                  -- stream.
                , nsoHalfClosed :: Bool
                  -- ^ Whether to start the stream half-closed for
                  -- this endpoint. A stream is started half-closed
                  -- when there is no content to send to the remote
                  -- endpoint, beyond the initial set of headers sent
                  -- in a SYN_STREAM or SYN_REPLY frame.
                , nsoOutgoingHandler :: StreamContent -> IO ()
                  -- ^ Handles outgoing content, presumably by sending
                  -- frames to a remote endpoint.
                , nsoWindowUpdateHandler :: DeltaWindowSize -> IO ()
                  -- ^ Handles an update to the local data window
                  -- size, typically by sending a WINDOW_UPDATE frame
                  -- to the remote endpoint.
                , nsoResetHandler :: TerminationStatus -> IO ()
                  -- ^ Handles a reset of the stream, typically by
                  -- sending a RST_STREAM frame to the remote
                  -- endpoint, and perhaps cleaning up other local
                  -- state associated with the stream.
                }

-- | Creates a new stream, along with actions for sending and
-- receiving content. Note that creating a stream does not involve the
-- exchange of SYN_STREAM and SYN_REPLY frames; this is the
-- responsibility of the enclosing endpoint.
--
-- The result includes an optional /push/ action for sending outgoing
-- content, and a /pull/ action for receiving incoming content. The
-- push action is 'Nothing' if and only if the stream is opened
-- half-closed. See 'nsoHalfClosed'.
--
-- The pull action will block until either there is content available,
-- or there is an error on the stream such as a timeout.
--
-- The push action will block for a data chunk when the outgoing data
-- window has shrunk below the size of the chunk. The action will
-- remain blocked until the remote endpoint communicates that there is
-- sufficient space in the data window, or until the stream aborts
-- because of a timeout. The push action will not block indefinitely
-- for headers.
--
-- Once the last incoming content is pulled, the pull action will
-- block forever, so be careful to check 'isLast' on the
-- content. (TODO: change this)
--
-- It is an error to push further outgoing content after pushing
-- content for which 'isLast' is 'True'. However, the push action does
-- not check for this. (TODO: change this)
newStream :: StreamID
             -- ^ The stream ID. This is mostly intended for error messages.
             -> NewStreamOpts
             -- ^ Options governing the behaviour of the stream.
             -> IO (Stream, Maybe (StreamContent -> IO ()), IO StreamContent)
             -- ^ The resulting stream, optional push action, and pull action.
newStream sid nso = do
  let odws = nsoInitialOutgoingWindowSize nso
      ibufs = nsoIncomingBufferSize nso
      halfClosed = nsoHalfClosed nso
  odwsSem <- MSemN.new odws
  buf <- BB.new ibufs
  let s = Stream sid odwsSem buf
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

-- | Updates the outgoing data window size. This is meant to handle
-- incoming WINDOW_UPDATE frames.
updateOutgoingWindowSize :: Stream -> DeltaWindowSize -> IO ()
updateOutgoingWindowSize s delta =
  MSemN.signal (ssOutgoingWindowSize s) (fromIntegral delta)

-- | Attempts to buffer a chunk of incoming data. Will not block indefinitely.
addIncomingData :: Stream
                   -- ^ The stream on which to buffer the data.
                   -> ByteString
                   -- ^ The data to buffer.
                   -> Bool
                   -- ^ Whether this is the last incoming content
                   -- expected on the stream.
                   -> IO Bool
                   -- ^ Whether the data was buffered. A result of
                   -- 'False' indicates that the remote endpoint is
                   -- not respecting flow control.
addIncomingData s bytes last = BB.tryAdd (ssIncomingBuffer s) (moreData bytes last)

-- | Buffer incoming headers. There is no limit on the buffering of
-- headers, and this function should not block indefinitely.
addIncomingHeaders :: Stream
                      -- ^ The stream on which to buffer the headers.
                      -> HeaderBlock
                      -- ^ The header block to buffer.
                      -> Bool
                      -- ^ Whether this is the last content expected on the stream.
                      -> IO ()
addIncomingHeaders s headers last = BB.add (ssIncomingBuffer s) (moreHeaders headers last)


-- | Obtains a snapshot of a stream's state. A snapshot is a triple
-- containing the remote data window size, the remaining buffer size,
-- and the contents of the buffer. This is intended to support unit
-- testing and debugging.
snapshot :: Stream -> IO (Int, Int, [StreamContent])
snapshot s = do
  rdw <- MSemN.peekAvail $ ssOutgoingWindowSize s
  (ldw, contents) <- BB.snapshot $ ssIncomingBuffer s
  return (rdw, ldw, contents)
