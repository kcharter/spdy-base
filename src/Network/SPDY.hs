{- |

SPDY is a protocol intended to speed up the transport of web
content. It allows multiplexing several streams on a single connection
between endpoints, with stream priorities and a notion of flow
control. See <http://dev.chromium.org/spdy> for all the details.

This library supports SPDY version 3, described in the third draft of
the SPDY specification, available at
<http://dev.chromium.org/spdy/spdy-protocol>. -}

module Network.SPDY (
  -- * Versions of the protocol
  spdyVersion3,
  supportedSPDYVersions,
  -- * Frame data structures
  module Network.SPDY.Frames,
  module Network.SPDY.Flags,
  -- * Header compression and decompression
  module Network.SPDY.Compression,
  -- * Converting frames to byte strings
  module Network.SPDY.Serialize,
  -- * Parsing frames from byte strings
  module Network.SPDY.Deserialize) where

import Network.SPDY.Frames
import Network.SPDY.Flags
import Network.SPDY.Compression
import Network.SPDY.Serialize
import Network.SPDY.Deserialize

-- | SPDY version 3
spdyVersion3 :: SPDYVersion
spdyVersion3 = SPDYVersion 3

-- | The list of protocol versions supported by this library, in
-- descending order of preference.
supportedSPDYVersions :: [SPDYVersion]
supportedSPDYVersions = [spdyVersion3]
