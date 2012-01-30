module Network.SPDY.Frames where

data Frame
  = ControlFrame
    -- ^ A control frame.
  | DataFrame
    -- ^ A data frame.
  deriving (Eq, Show, Read)
