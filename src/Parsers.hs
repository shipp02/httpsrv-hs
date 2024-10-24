{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}

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
  ( IResult (..),
    Parser,
    anyWord8,
    feed,
    parse,
    peekWord8',
    string,
    take,
    takeWhile, many1',
  )
import Data.Attoparsec.Char8 as AC
  ( char,
    decimal,
    endOfLine,
    isEndOfLine,
    skipSpace,
    takeWhile,
  )
import qualified Data.BitVector as BV
import Data.Bits (Bits (setBit, shiftL, testBit, (.&.), (.|.)), shiftR)
import Data.ByteString as BS (ByteString, tail, head, null, pack, empty)
import qualified Data.List as L
import Data.Text as T (Text, toLower, singleton)
import Data.Text.Encoding as TE (decodeUtf8, encodeUtf8, decodeASCII)
import Data.Word (Word16, Word32, Word8)
import Utils (fromByteString)
import GHC.RTS.Flags (MiscFlags(installSEHHandlers))
import Debug.Trace (trace)
import HuffmanTable
import Data.BitVector (xnor)
import Data.Either (isLeft, fromRight)
import Control.Monad (liftM)

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
      -- hf <- parseHeadersFlags f
      -- _ <- parseHeaders f hf
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

-- >>> let inp = BS.pack [0,0,196,1,4,0,0,0,1,136,97,150,221,109,95,74,4,74,67,108,202,8,1,121,64,187,113,144,92,104,42,98,209,191,95,135,73,124,165,138,232,25,170,108,150,223,105,126,148,3,202,104,31,165,4,0,188,160,89,184,219,55,4,37,49,104,223,15,19,139,254,91,28,161,28,114,9,102,75,252,255,82,132,143,210,74,143,15,13,2,54,50,64,143,242,180,99,39,82,213,34,211,148,114,22,197,172,74,127,134,2,224,0,156,105,191,118,134,170,105,210,154,252,255,124,135,18,149,77,58,83,95,159,64,139,242,180,182,14,146,172,122,210,99,212,143,137,221,14,140,26,182,228,197,147,79,64,140,242,183,148,33,106,236,58,74,68,152,245,127,138,15,218,148,158,66,193,29,7,39,95,64,144,242,177,15,82,75,82,86,79,170,202,177,235,73,143,82,63,133,168,232,168,210,203]
-- >>> parse parseHeaders inp
-- Partial _
-- TODO: Why is this partial?
parseHeaders :: Parser [Header]
parseHeaders = do
  frame <- parseHttp2Frame
  hf <- parseHeadersFlags $ trace ("frame" ++ show frame) frame
  parseHeaders_ frame $ trace ("HeaderFlags:" ++ show hf) hf

parseHeaders_ :: Frame -> HeadersFlags -> Parser [Header]
parseHeaders_ (Frame _ id flags len) HeadersFlags {..} =
  do
    padlen <- if padded then fromIntegral <$> AB.anyWord8 else return 0
    -- _ <- AB.take padlen
    let prilen = if priority then 5 else 0
    _ <-
      if priority
        then do
          sd <- word32
          weight <- AB.anyWord8
          return $ Just (sd, weight)
        else return Nothing
    -- inp <- AB.take $ fromIntegral len - padlen - prilen
    let ph = do x <- parseHeader
                trace ("header: " ++ show x) return x
    many1' ph

data Header
  = Indexed Int
  | IndexedName Int Text
  | IndexedNew Text Text
  | Name Int Text
  | New Text Text
  deriving (Show, Eq)

parseHeader :: Parser Header
parseHeader = do
  w <- AB.anyWord8
  parseHeader_ w
parseHeader_ :: Word8 -> Parser Header
parseHeader_ oct
  | oct .&. 0x80 == 0x80 = Indexed <$> parseIndexInt oct 7
  | oct .&. 0x40 == 0x40 = parseIndexedHeader oct
  | oct .&. 0xf0 == 0 = parseNonIndexedHeader oct
  | otherwise = return $ New "Otherwise Not Implemented" ""

type Error = String
type BitString = (ByteString, Word8, Int, Word8)
newtype HuffmanParser a = HuffmanParser { runHuffmanParser :: BitString -> (BitString, Either Error a) }

instance Functor HuffmanParser where
  fmap f (HuffmanParser st) = HuffmanParser $ \stream -> case st stream of
    (res, Left err) -> (res, Left err)
    (res, Right a ) -> (res, Right (f a))

instance Applicative HuffmanParser where
  pure a = HuffmanParser (\stream -> (stream, Right a))
  HuffmanParser ff <*> HuffmanParser xx = HuffmanParser $ \stream0 -> case ff stream0 of   -- produce an f
    (stream1, Left err) -> (stream1, Left err)
    (stream1, Right f ) -> case xx stream1 of          -- produce an x
      (stream2, Left err) -> (stream2, Left err)
      (stream2, Right x ) -> (stream2, Right (f x))    -- return (f x)

instance Monad HuffmanParser where
  return = pure
  (>>=) :: HuffmanParser a -> (a -> HuffmanParser b) -> HuffmanParser b
  HuffmanParser runA >>= f =
    HuffmanParser $ \stream -> case runA stream of
      (rest, Left err) -> trace ("Error: " ++ err) (rest, Left err)
      (rest, Right o) -> runHuffmanParser (f o) rest

satisfy :: (Bit->Bool) -> HuffmanParser Bit
satisfy f = HuffmanParser $ \(stream, word, i, lastWord) ->
  if f . boolToBit $ testBit word 7
    then
      let
          last' = updateLast word lastWord
      in
        if i == 7
      -- TODO: This will throw at the end. We don't want that.
          then ((BS.tail stream, BS.head stream, 0, last'), Right . boolToBit $ testBit word 7)
        else ((stream, shiftL word 1, i+1, last'), Right . boolToBit $ testBit word 7)
  else ((stream, word, i, lastWord), Left "Unexpected bit")

bit :: Bit -> HuffmanParser Bit
bit x = satisfy (== x)

updateLast :: Word8 -> Word8 -> Word8
updateLast word l = if testBit word 7 then setBit (shiftL l 1) 0 else shiftL l 1

anyBit :: HuffmanParser (Maybe Bit)
anyBit =
  let
    anyBit_ inp@(stream, word, i, lastWord)
      -- | i == 7 && BS.null stream = (("", 0, 7, updateLast word lastWord), if checkPaddingValid (trace ("checkPadding: " ++ show lastWord) lastWord) then Right Nothing else Left "End of stream")
      | i == 8 = (("", 0, 7, updateLast word lastWord), Right Nothing)
      | i == 7 && BS.null stream =
        (("", 0, i+1, updateLast word lastWord),
        Right . Just . boolToBit $ testBit word 7)
      | i == 7 =
        ((BS.tail stream, BS.head stream, 0, updateLast word lastWord),
        Right . Just .boolToBit $ testBit word 7)
      | otherwise =
        ((stream, shiftL word 1, i+1, updateLast word lastWord),
        Right . Just .boolToBit $ testBit word 7)
  in
    HuffmanParser anyBit_

clearLast :: HuffmanParser ()
clearLast = HuffmanParser $ \(a,b,c,_) -> ((a,b,c,0), Right ())

checkPaddingValid :: Word8 -> Bool
checkPaddingValid 0 = True
checkPaddingValid w = if testBit w 0 then checkPaddingValid (shiftR w 1) else checkPaddingValid0 (shiftR w 1)

checkPaddingValid0 :: Bits a => a -> Bool
checkPaddingValid0 w = not $ testBit w 0

takeUntilEosOr7 :: HuffmanParser Int -> HuffmanParser [Int]
takeUntilEosOr7 p =
  let
    -- takeUntilEosOr7_ :: Monoid a => [a] -> HuffmanParser a -> HuffmanParser [a]
    takeUntilEosOr7_ arr rp@(HuffmanParser r) = HuffmanParser $ \inp@(stream, word, i, lastWord) ->
      if i == 7 && BS.null stream
        then (inp, Right arr)
      else
        let
          (inp2, res) = r inp
          HuffmanParser rest =
            if isLeft res
              then HuffmanParser $ const (inp2, fmap L.singleton res)
            else takeUntilEosOr7_ (arr ++ [fromRight 0 res]) p
        in
          rest inp2
          -- (inp2, Right $ arr ++ [fromRight 0 res])

  in
    takeUntilEosOr7_ [] p


huffmanDecodeOne :: HuffmanTree -> HuffmanParser Int
huffmanDecodeOne Nil = HuffmanParser (, Left "Found Nil")
huffmanDecodeOne (Leaf i) = clearLast >> return i
huffmanDecodeOne (Internal l r) =
  do
    mb <- anyBit
    case mb of
      Just b -> if b == F then huffmanDecodeOne l else huffmanDecodeOne r
      Nothing -> return 65 -- There needs to a better way.

-- convert to HuffmanParser Text

-- >>> import Data.ByteString as BS
-- >>> let x = BS.pack [97,150,221,109,95,74,4,74,67,108,202,8,1,121,64,187,113,144,92,104,42,98,209,191,42,98,209,191]
-- >>> let Done _ (IndexedName _ inp2) = parse (parseIndexedHeader $ BS.head x) (BS.tail x)
-- >>> inp2
-- "Sun, 12 Aug 2018 17:30:41 GMTA"
-- >>> import Data.ByteString as BS
-- >>> let x = BS.pack [64,139,242,180,182,14,146,172,122,210,99,212,143,137,221,14,140,26,182,228,197,147,79]
-- >>> let Done _ (IndexedNew k v) = parse (parseIndexedHeader $ BS.head x) (BS.tail x)
-- >>> (k,v)
-- ("x-frame-optionsA","SAMEORIGINA")
-- >>> import Data.ByteString as BS
-- >>> let x = BS.pack [95,135,73,124,165,138,232,25,170]
-- >>> let Done _ (IndexedName _ inp2) = parse (parseIndexedHeader $ BS.head x) (BS.tail x)
-- >>> inp2
-- "text/plainA"
huffmanDecode :: HuffmanTree -> HuffmanParser Text
huffmanDecode tree = liftM (decodeUtf8 . BS.pack . map fromIntegral) $ takeUntilEosOr7 (huffmanDecodeOne tree)


runHuffman p bs =
  let
    (_, done) = runHuffmanParser p (BS.tail bs, BS.head bs, 0, 0)
  in
    done

-- Will throw because it swallows the error.
-- TODO: Get rid of the 'A" at the end. Why is it not there sometimes?
decodeHeaderBS :: Word8 -> ByteString -> Text
decodeHeaderBS w bs = if testBit w 7 then fromRight "" (runHuffman (huffmanDecode huffmanTree) bs) else TE.decodeUtf8 bs

parseIndexedHeader :: Word8 -> Parser Header
parseIndexedHeader oct
  | oct .&. setBits 6 == 0 = do
      nl' <- AB.anyWord8
      nlen <- parseIndexInt nl' 7
      -- _ <-trace ("nlen" ++ show nlen) return ()
      ns <- AB.take nlen
      vl' <- AB.anyWord8
      vlen <- parseIndexInt vl' 7
      -- _ <-trace ("vlen" ++ show vlen) return ()
      vs <- AB.take vlen
      return $ IndexedNew (decodeHeaderBS nl' ns) (decodeHeaderBS vl' vs)
  | otherwise = do
      idx <- parseIndexInt oct 6
      -- _ <-trace ("idx: " ++ show idx) return ()
      l' <- AB.anyWord8
      len <- parseIndexInt l' 7
      -- _ <-trace ("len: " ++ show len ++ "l': " ++ show l') return ()
      vs <- AB.take len
      return $ IndexedName idx (decodeHeaderBS l' vs)

parseNonIndexedHeader :: Word8 -> Parser Header
parseNonIndexedHeader oct
  | oct .&. setBits 4 == 0 = do
      nl' <- AB.anyWord8
      nlen <- parseIndexInt nl' 4
      -- _ <-trace ("nlen" ++ show nlen) return ()
      ns <- AB.take nlen
      vl' <- AB.anyWord8
      vlen <- parseIndexInt vl' 7
      -- _ <-trace ("vlen" ++ show vlen) return ()
      vs <- AB.take vlen
      return $ New (decodeHeaderBS nl' ns) (decodeHeaderBS vl' vs)
  | otherwise = do
      idx <- parseIndexInt oct 4
      -- _ <-trace ("idx: " ++ show idx) return ()
      l' <- AB.anyWord8
      len <- parseIndexInt l' 7
      -- _ <-trace ("len: " ++ show len ++ "l': " ++ show l') return ()
      vs <- AB.take len
      return $ Name idx $ decodeHeaderBS l' vs


parseIndexInt :: Word8 -> Int -> Parser Int
parseIndexInt w n
  | w .&. setBits n == setBits n =
      let indexInt' i m = do
            b <- AB.anyWord8
            if b .&. 128 == 128
              then indexInt' (i + fromIntegral (b .&. 127) * 2 ^ m) (m + 7)
            else return (i + fromIntegral (b.&.127) * 2^m)
       in fmap (+(2^n-1)) (indexInt' 0 0)
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
-- >>> let Done _ (_,res) = parse (parseSettings (Frame Settings 0 0 18)) (TE.encodeUtf8 "\x00\x03\x00\x00\x00\x64\x00\x04\x40\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x12\x04\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x64\x00\x04\x40\x00\x00\x00\x00\x02\x00\x00\x00\x00")
-- >>> res == [(MaxConcurrentStreams,100),(InitialWindowSize,1073741824),(EnablePush,0)]
-- True
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
    winincr <- BV.uint . fromByteString <$> AB.take 4
    return ()
