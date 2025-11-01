module Http2Types
  (
    FrameType(..),
    FrameHdr(..),
    SettingParams(..),
    byteToFrameType,
    frameTypeToByte
  ) where

import Data.Word

data FrameType
  = Data
  | Headers
  | Priority
  | RstStream
  | Settings
  | PushPromise
  | Ping
  | GoAway
  | WindowUpdate
  | Continuation
  deriving (Show, Eq)

frameTypeToByte :: FrameType -> Word8
frameTypeToByte typ = 
  case typ of
    Data -> 0x00 
    -- DATA
    Headers -> 0x01 
    -- HEADERS
    Priority -> 0x02 
    -- PRIORITY
    RstStream -> 0x03 
    -- RST_STREAM
    Settings -> 0x04 
    -- SETTINGS
    PushPromise -> 0x05 
    -- PUSH_PROMISE
    Ping -> 0x06 
    -- PING
    GoAway -> 0x07 
    -- GOAWAY
    WindowUpdate -> 0x08 
    -- WINDOW_UPDATE
    Continuation -> 0x09 
    -- CONTINUATION

byteToFrameType :: Word8 -> FrameType
byteToFrameType typ = 
  case typ of
    0x00 -> Data
    -- DATA
    0x01 -> Headers
    -- HEADERS
    0x02 -> Priority
    -- PRIORITY
    0x03 -> RstStream
    -- RST_STREAM
    0x04 -> Settings
    -- SETTINGS
    0x05 -> PushPromise
    -- PUSH_PROMISE
    0x06 -> Ping
    -- PING
    0x07 -> GoAway
    -- GOAWAY
    0x08 -> WindowUpdate
    -- WINDOW_UPDATE
    0x09 -> Continuation
    -- CONTINUATION
    _ ->  Data

data FrameHdr = FrameHdr Word32 Word8 Word32 deriving (Show, Eq)

data SettingParams
  = HeaderTableSize
  | EnablePush
  | MaxConcurrentStreams
  | InitialWindowSize
  | MaxFrameSize
  | MaxHeaderListSize
  deriving (Show, Eq)
