{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}


module Http2Output 
  (

  ) where

import Http2Types
import Data.ByteString (ByteString)
import Data.Word (Word32, Word8)
import Data.ByteString.Builder (Builder, word32BE, word16BE, word8)
import Data.Bits

w8 :: Word32 -> Word8
w8 = fromIntegral

word24 :: Word32 -> Builder
word24 x = word8 ( w8 (shiftR (x .&. 0xff0000::Word32) 16) )
        <> word8 ( w8 (shiftR (x .&. 0xff00) 8) )
        <> word8 ( w8 (x .&. 0xff))

outputFrameHdr :: FrameHdr -> Builder
outputFrameHdr (FrameHdr id' flags len) = 
    word24 len
    <> word8 0x04
    <> word8 flags
    <> word32BE id'


-- outputSettings :: [(SettingParams, Word32)] -> Builder
-- outputSettings sets = 
--   let
--    outputSettings_ :: [(SettingParams, Word32)] -> (Builder, Frame)
--    outputSettings_ [] = mempty 
--    outputSettings_ (x:xs) = (outputSetting x <> outputSettings_ xs, Frame Settings 0x04 0x0 0x20)
   
--    outputSetting :: (SettingParams, Word32) -> Builder
--    outputSetting (HeaderTableSize, d) = word16BE 0x01 <> word32BE d
--    outputSetting (EnablePush, d) = word16BE 0x02 <> word32BE d
--    outputSetting (MaxConcurrentStreams, d) = word16BE 0x02 <> word32BE d
--    outputSetting (InitialWindowSize, d) = word16BE 0x04 <> word32BE d
--    outputSetting (MaxFrameSize, d) = word16BE 0x05 <> word32BE d
--    outputSetting (MaxHeaderListSize, d) = word16BE 0x06 <> word32BE d
   
--    (sb, f) = outputSettings_ sets
--   in
--    outputFrameHdr f <> sb

    

-- outputData :: ByteString -> Builder
-- outputData d = word24 10 
     