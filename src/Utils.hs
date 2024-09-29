module Utils
  ( fromWord8s,
    fromByteString,
  )
where

import Data.BitVector as BV
import Data.ByteString as BS
import Data.Word

fromWord8s :: [Word8] -> BV
fromWord8s (x : xs) =
  BV.append (bitVec 8 x) (fromWord8s xs)
fromWord8s [] = nil

fromByteString = fromWord8s . BS.unpack
