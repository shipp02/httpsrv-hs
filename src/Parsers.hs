{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Parsers
  ( parseOrFeed,
    parseReqFirst,
    parseAllFields,
    parseAllValues,
    parseHttp2Magic,
    Method (..),
    Request (..),
    Version (..),
    Field (..),
    parseSettings,
    parseHeaders,
  )
where

import ASCII.Char as ASCII (Char (Comma), toInt)
import Data.Attoparsec.ByteString as AB
import Data.Attoparsec.Char8 as AC
import qualified Data.BitVector as BV
import Data.Bits
import Data.ByteString as BS
import qualified Data.List as L
import Data.Text as T
import Data.Text.Encoding as TE
import Data.Word (Word16, Word32, Word8)
import Utils

data Method
  = Get
  | Put
  | Post
  | Head
  | Delete
  | Connect
  deriving (Show, Eq)

toMethod :: ByteString -> Method
toMethod "GET" = Get
toMethod "PUT" = Put
toMethod "POST" = Post
toMethod "HEAD" = Head
toMethod "CONNECT" = Connect
toMethod _ = Get

data Version = Version Int Int deriving (Show)

data Request = Request Method Text Version deriving (Show)

data Field = Field Text [Text] deriving (Show)

-- True if maic recieved
parseHttp2Magic :: Parser ()
parseHttp2Magic = string (TE.encodeUtf8 "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n") >> return ()

parseOrFeed ::
  Parser r ->
  Maybe (IResult ByteString r) ->
  ByteString ->
  IResult ByteString r
parseOrFeed _ (Just partial) bs = feed partial bs
parseOrFeed p Nothing bs = parse p bs

parseReqFirst :: Parser Request
parseReqFirst = do
  method <- AC.takeWhile (/= ' ')
  skipSpace
  target <- AC.takeWhile (/= ' ')
  skipSpace
  _ <- string (TE.encodeUtf8 "HTTP")
  _ <- char '/'
  vMajor <- decimal
  _ <- char '.'
  vMinor <- decimal
  AC.endOfLine
  return $ Request (toMethod method) (TE.decodeUtf8 target) (Version vMajor vMinor)

parseFieldLine :: Parser Field
parseFieldLine = do
  key <- AC.takeWhile (/= ':')
  _ <- anyWord8
  skipSpace
  value <- parseAllValues []
  AC.endOfLine
  -- TODO: TE.decodeUtf8 Throws uncatchable exception
  return (Field (T.toLower . TE.decodeUtf8 $ key) value)

parseAllValues :: [Text] -> Parser [Text]
parseAllValues vs = do
  skipSpace
  value <- AB.takeWhile (\x -> (x /= toWord8 ASCII.Comma) && (not . isEndOfLine $ x))
  x <- peekWord8'
  if isEndOfLine x
    then return $ vs ++ [TE.decodeUtf8 value]
    else anyWord8 >> parseAllValues (TE.decodeUtf8 value : vs)

toWord8 :: ASCII.Char -> Word8
toWord8 = fromInteger . toInteger . toInt

parseAllFields :: [Field] -> Parser [Field]
parseAllFields f =
  do
    w <- peekWord8'
    let eol = isEndOfLine w
    if eol
      then endOfLine >> (return . L.reverse $ f)
      else do
        f' <- parseFieldLine
        parseAllFields (f' : f)

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

data Frame = Frame FrameType Word32 Word8 Word32 deriving (Show, Eq)

data SettingParams
  = HeaderTableSize
  | EnablePush
  | MaxConcurrentStreams
  | InitialWindowSize
  | MaxFrameSize
  | MaxHeaderListSize
  deriving (Show, Eq)

w32 :: (Integral a) => a -> Word32
w32 = fromIntegral

word24 :: Parser Word32
word24 = do
  a <- flip shiftL 16 . w32 <$> anyWord8
  b <- flip shiftL 8 . w32 <$> anyWord8
  c <- w32 <$> anyWord8
  return $ a .|. b .|. c

word32 :: Parser Word32
word32 = do
  a <- flip shiftL 24 . w32 <$> anyWord8
  b <- flip shiftL 16 . w32 <$> anyWord8
  c <- flip shiftL 8 . w32 <$> anyWord8
  d <- w32 <$> anyWord8
  return $ a .|. b .|. c .|. d

parseHttp2Frame :: Parser Frame
parseHttp2Frame = do
  len <- word24
  typ <- anyWord8
  flags <- anyWord8
  id' <- word32
  case typ of
    0x00 -> return $ Frame Data id' flags len
    -- DATA
    0x01 -> do
      let f = Frame Headers id' flags len
      hf <- parseHeadersFlags f
      _ <- parseHeaders f hf
      return f
    -- HEADERSS
    0x02 -> return $ Frame Priority id' flags len
    -- PRIORITY
    0x03 -> return $ Frame RstStream id' flags len
    -- RST_STREAM
    0x04 -> return $ Frame Settings id' flags len
    -- SETTINGS
    0x05 -> return $ Frame PushPromise id' flags len
    -- PUSH_PROMISE
    0x06 -> return $ Frame Ping id' flags len
    -- PING
    0x07 -> return $ Frame GoAway id' flags len
    -- GOAWAY
    0x08 -> return $ Frame WindowUpdate id' flags len
    -- WINDOW_UPDATE
    0x09 -> return $ Frame Continuation id' flags len
    -- CONTINUATION
    _ -> return $ Frame Data id' flags len

parseData :: Frame -> Parser ()
parseData (Frame _ id flags len) =
  if flags `testBit` 3
    then do
      padlen <- fromIntegral <$> AB.anyWord8
      d <- AB.take (fromIntegral len - 1 - padlen)
      _ <- AB.take padlen
      return ()
    else do
      d <- AB.take . fromIntegral $ len
      return ()

data HeadersFlags = HeadersFlags
  { priority :: Bool,
    padded :: Bool,
    endHeaders :: Bool,
    endStream :: Bool
  }
  deriving (Show, Eq)

parseHeadersFlags :: Frame -> Parser HeadersFlags
parseHeadersFlags (Frame _ _ flags _) = do
  return $ HeadersFlags (flags `testBit` 5) (flags `testBit` 3) (flags `testBit` 2) (flags `testBit` 0)

parseHeaders :: Frame -> HeadersFlags -> Parser ByteString
parseHeaders (Frame _ id flags len) HeadersFlags {..} =
  do
    padlen <- if padded then fromIntegral <$> AB.anyWord8 else return 0
    _ <- AB.take padlen
    _ <-
      if priority
        then do
          sd <- word32
          weight <- AB.anyWord8
          return $ Just (sd, weight)
        else return Nothing
    AB.take $ fromIntegral len - padlen

data Header
  = Indexed Int
  | IndexedName Int Text
  | IndexedNew Text Text
  | Name Int Text
  | New Text Text
  deriving (Show, Eq)

parseHeader :: Word8 -> Parser Header
parseHeader oct
  | oct .&. 0x80 == 0x80 = Indexed <$> parseIndexInt oct 7
  -- \| oct .&. 0x40 == 0x40 = parseIndexedHeader oct
  | oct .&. 0x40 == 0x40 = return $ IndexedName 0 "Not Implemented"
  | oct .&. 0xf0 == 0 = return $ New "Not implemented" "Not Implemented"

-- parseIndexedHeader :: Word8 -> Parser Header
-- parseIndexedHeader oct
-- \| oct .&. setBits 6 == 0 = do
--  nl' <- AB.anyWord8
--  nlen <- parseIndexInt l' 7
--  ns <- AB.take len
--  vl' <- AB.anyWord8
--  vlen <- parseIndexInt l' 7
--  vs <- AB.take len
--  return $ IndexedNew ns vs
-- \| otherwise = do
--  idx <- parseIndexInt oct 6
--  l' <- AB.anyWord8
--  len <- parseIndexInt l' 7
--  vs <- AB.take len
--  return $ IndexedName idx vs

parseIndexInt :: Word8 -> Int -> Parser Int
parseIndexInt w n
  | w .&. setBits n == setBits n =
      let indexInt' i m = do
            b <- AB.anyWord8
            if b .&. 128 == 128
              then indexInt' (i + fromIntegral (b .&. 127) * 2 ^ m) (m + 7)
              else return i
       in indexInt' 0 0
  | otherwise = return . fromIntegral $ w .&. setBits n

setBits :: Int -> Word8
setBits n =
  let setBits' 0 w = setBit w 0
      setBits' n' w = setBits' (n' - 1) (setBit w n')
   in setBits' (n - 1) 0

parsePriority :: Frame -> Parser ()
parsePriority (Frame _ id flags len) =
  do
    sdep <- fromIntegral . BV.uint . fromByteString <$> AB.take 4
    weight <- fromIntegral <$> AB.anyWord8
    return ()

parseRstStream :: Frame -> Parser ()
parseRstStream (Frame _ id flags len) =
  do
    ec <- fromIntegral . BV.uint . fromByteString <$> AB.take 4
    return ()

parseSettings :: Frame -> Parser (Bool, [(SettingParams, Word32)])
parseSettings (Frame _ id flags len) =
  do
    ack <- if flags `testBit` 0 then return True else return False
    ss <- settings_ 0 [] (fromIntegral len)
    return (ack, ss)

-- Basic test code
-- >>> let Done _ res = parse (parseSettings (Frame Settings 0 0 18)) (TE.encodeUtf8 "\x00\x03\x00\x00\x00\x64\x00\x04\x40\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x12\x04\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x64\x00\x04\x40\x00\x00\x00\x00\x02\x00\x00\x00\x00")
-- >>> res == [(MaxConcurrentStreams,100),(InitialWindowSize,1073741824),(EnablePush,0)]
parseSetting :: Parser (SettingParams, Word32)
parseSetting =
  do
    id <- fromIntegral . BV.uint . fromByteString <$> AB.take 2
    val <- fromIntegral . BV.uint . fromByteString <$> AB.take 4
    return (settingParam id, val)

settings_ :: Int -> [(SettingParams, Word32)] -> Int -> Parser [(SettingParams, Word32)]
settings_ used res max =
  if used >= max
    then return res
  else do
    s <- parseSetting
    settings_ (used + 6) (res ++ [s]) max

settingParam :: Word16 -> SettingParams
settingParam x = case x of
  0x01 -> HeaderTableSize
  0x02 -> EnablePush
  0x03 -> MaxConcurrentStreams
  0x04 -> InitialWindowSize
  0x05 -> MaxFrameSize
  0x06 -> MaxHeaderListSize

-- Just takes len bytes
parseAny :: Frame -> Parser ()
parseAny (Frame _ _ _ len) = AB.take (fromIntegral len) >> return ()

parseWindowUpdate :: Frame -> Parser ()
parseWindowUpdate (Frame _ id flags len) =
  do
    winincr <- fromIntegral . BV.uint . fromByteString <$> AB.take 4
    return ()
