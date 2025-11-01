{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc,
    httpServer,
    runTCPServer,
  )
where

import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import Data.Attoparsec.ByteString as AB
import Data.ByteString
import qualified Data.ByteString as BS
import qualified Data.List as L
import Data.Text as T
import Data.Text.Encoding as TE
import Debug.Trace (trace)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Parsers
    ( parseHttp2Magic,
      parseOrFeed,
      parseRequest,
      RequestData(..),
      Field(..),
      Method(Get, Put, Post, Head, Delete, Connect),
      Request(..), parseSettings, parseHttp2Frame )
import Data.Word ( Word32 )
import Http2Types
import Control.Monad.State.Strict ( StateT (runStateT), put, MonadState (get))
import Control.Monad.IO.Class (liftIO)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data ServerState = ServerState HttpState ByteString Socket
data HttpState = RecvRequest (Result Request) 
newtype NetworkAction = Send ByteString
type Server a = StateT ServerState IO a

runTCPServer :: Maybe HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPServer mhost port server = withSocketsDo $ do
  addr <- resolve
  E.bracket (open addr) close loop
  where
    resolve = do
      let hints =
            defaultHints
              { addrFlags = [AI_PASSIVE],
                addrSocketType = Stream
              }
      L.head <$> getAddrInfo (Just hints) mhost (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 1024
      return sock
    loop sock = forever $
      E.bracketOnError (accept sock) (close . fst) $
        \(conn, _peer) ->
          void $
            -- 'forkFinally' alone is unlikely to fail thus leaking @conn@,
            -- but 'E.bracketOnError' above will be necessary if some
            -- non-atomic setups (e.g. spawning a subprocess to handle
            -- @conn@) before proper cleanup of @conn@ is your case
            forkFinally (server conn) (const $ gracefulClose conn 5000)

httpServer :: Socket -> Server ()
httpServer = httpServer' Nothing

httpServer' :: Maybe ByteString -> Socket -> Server ()
httpServer' Nothing s =
  do
    msg <- recv s 4096
    httpServer'' msg s
httpServer' (Just irem) s = httpServer'' irem s

_startHdrs :: ByteString
_startHdrs = BS.concat [
  TE.encodeUtf8 $ T.concat [
              "HTTP/1.1 101 Switching Protocols\r\n"
              , "Connection: Upgrade\r\n"
              , "Upgrade: h2c\r\n\r\n"
              ]
  ,_startFrame
 ]

_startFrame :: ByteString
_startFrame = BS.pack
                  [ 0x00,
                    0x00,
                    0x12,
                    0x04,
                    0x00,
                    0x00,
                    0x00,
                    0x00,
                    0x00,
                    0x00,
                    0x03,
                    0x00,
                    0x00,
                    0x00,
                    0x64,
                    0x00,
                    0x04,
                    0x00,
                    0x10,
                    0x00,
                    0x00,
                    0x00,
                    0x01,
                    0x00,
                    0x00,
                    0x20,
                    0x00
                  ]
_hdrsFrame :: ByteString
_hdrsFrame = BS.pack [
 0x00, 0x00, 0xc4, 0x01, 0x04, 0x00, 0x00, 0x00, 0x01, 0x88, 0x61, 0x96, 0xdd, 0x6d, 0x5f, 0x4a,
 0x04, 0x4a, 0x43, 0x6c, 0xca, 0x08, 0x01, 0x79, 0x40, 0xbb, 0x71, 0x90, 0x5c, 0x68, 0x2a, 0x62,
 0xd1, 0xbf, 0x5f, 0x87, 0x49, 0x7c, 0xa5, 0x8a, 0xe8, 0x19, 0xaa, 0x6c, 0x96, 0xdf, 0x69, 0x7e,
 0x94, 0x03, 0xca, 0x68, 0x1f, 0xa5, 0x04, 0x00, 0xbc, 0xa0, 0x59, 0xb8, 0xdb, 0x37, 0x04, 0x25,
 0x31, 0x68, 0xdf, 0x0f, 0x13, 0x8b, 0xfe, 0x5b, 0x1c, 0xa1, 0x1c, 0x72, 0x09, 0x66, 0x4b, 0xfc,
 0xff, 0x52, 0x84, 0x8f, 0xd2, 0x4a, 0x8f, 0x0f, 0x0d, 0x02, 0x36, 0x32, 0x40, 0x8f, 0xf2, 0xb4,
 0x63, 0x27, 0x52, 0xd5, 0x22, 0xd3, 0x94, 0x72, 0x16, 0xc5, 0xac, 0x4a, 0x7f, 0x86, 0x02, 0xe0,
 0x00, 0x9c, 0x69, 0xbf, 0x76, 0x86, 0xaa, 0x69, 0xd2, 0x9a, 0xfc, 0xff, 0x7c, 0x87, 0x12, 0x95,
 0x4d, 0x3a, 0x53, 0x5f, 0x9f, 0x40, 0x8b, 0xf2, 0xb4, 0xb6, 0x0e, 0x92, 0xac, 0x7a, 0xd2, 0x63,
 0xd4, 0x8f, 0x89, 0xdd, 0x0e, 0x8c, 0x1a, 0xb6, 0xe4, 0xc5, 0x93, 0x4f, 0x40, 0x8c, 0xf2, 0xb7,
 0x94, 0x21, 0x6a, 0xec, 0x3a, 0x4a, 0x44, 0x98, 0xf5, 0x7f, 0x8a, 0x0f, 0xda, 0x94, 0x9e, 0x42,
 0xc1, 0x1d, 0x07, 0x27, 0x5f, 0x40, 0x90, 0xf2, 0xb1, 0x0f, 0x52, 0x4b, 0x52, 0x56, 0x4f, 0xaa,
 0xca, 0xb1, 0xeb, 0x49, 0x8f, 0x52, 0x3f, 0x85, 0xa8, 0xe8, 0xa8, 0xd2, 0xcb
 ]

_dataFrame :: ByteString
_dataFrame = BS.pack [
 0x00, 0x00, 0x3e, 0x00, 0x01, 0x00, 0x00, 0x00, 0x01, 0x55, 0x73, 0x65, 0x72, 0x2d, 0x61, 0x67,
 0x65, 0x6e, 0x74, 0x3a, 0x20, 0x2a, 0x0a, 0x44, 0x69, 0x73, 0x61, 0x6c, 0x6c, 0x6f, 0x77, 0x3a,
 0x20, 0x0a, 0x0a, 0x53, 0x69, 0x74, 0x65, 0x6d, 0x61, 0x70, 0x3a, 0x20, 0x2f, 0x2f, 0x6e, 0x67,
 0x68, 0x74, 0x74, 0x70, 0x32, 0x2e, 0x6f, 0x72, 0x67, 0x2f, 0x73, 0x69, 0x74, 0x65, 0x6d, 0x61,
 0x70, 0x2e, 0x78, 0x6d, 0x6c, 0x20, 0x0a
 ]



httpServer'' :: Socket -> Server ()
httpServer'' s =
  do
    (ServerState _ msg _) <- get
    let p_r = parse parseRequest msg
    serverRun p_r respondRequest s
    -- case p_r of
    --   Done irem x -> respondRequest x s
    --   Partial _ -> void (put $ ServerState p_r BS.empty)
    --   Fail irem _ perror -> do
    --     liftIO $ putStrLn perror
    --     liftIO $ print irem

respondRequest :: Request -> Server ()
respondRequest p_r  =
  do
    (ServerState _ _ s) <- get
    case p_r of
      (Request path _ (GetRequest h)) ->
        if checkUpgrade h s
          then do
            liftIO $ putStrLn "do upgrade"
            liftIO $ sendAll s _startHdrs
            liftIO $ sendAll s _hdrsFrame
            liftIO $ sendAll s _dataFrame
            http2ServerStart' (if BS.null irem then Nothing else Just irem) s
          else do
            liftIO $ sendNormalResp p_r s
            liftIO $ print p_r
            liftIO $ checkClose h s
            httpServer' (if BS.null irem then Nothing else Just irem) s
      (Request path _ (PutRequest h _)) -> do
        liftIO $ print p_r
      (Request path _ (PostRequest h _)) -> do
        liftIO $ print p_r
      (Request path _ (HeadRequest h)) -> do
        liftIO $ print p_r
      (Request path _ (DeleteRequest h)) -> do
        liftIO $ print p_r
      (Request path _ (ConnectRequest h)) -> do
        liftIO $ print p_r

http2ServerStart' :: ByteString -> Socket -> Server ()
http2ServerStart' Nothing s =
  do
    msg <- liftIO $ recv s 4096
    http2ServerStart'' msg s
http2ServerStart' (Just irem) s = http2ServerStart'' irem s


serverRun :: Result Request -> (Request -> Server ()) -> Socket -> Server ()
serverRun p fn s = case p of
  Done irem ret -> do
    fn ret
    void (put $ ServerState p BS.empty)
  Partial _ -> return ()
  Fail _ _ _ -> return ()


http2ServerStart'' :: ByteString -> Socket -> Server ()
http2ServerStart'' msg s =
  do
    let p_r = parse parseHttp2Magic msg
    case p_r of
      Done irem () -> do
        liftIO $ putStrLn "Got magic"
        http2Server irem s
      Partial a -> do
        x <- liftIO $ recv s 4096
        feed p_r x
      Fail irem _ perror -> do
        liftIO $ putStrLn perror
        liftIO $ print irem

http2Server :: ByteString -> Socket -> Server ()
http2Server msg s =
  do
    let p_r = parse parseHttp2Frame msg
    case p_r of
      Done irem ret@(f,_) -> do
        liftIO $ print f
        handleHttp2Settings Nothing f irem s
      Partial _ -> return ()
      Fail _ _ _ -> return ()


handleHttp2Settings :: Maybe (IResult ByteString (Bool, [(SettingParams, Word32)])) -> FrameHdr -> ByteString -> Socket -> Server ()
handleHttp2Settings res frame@(FrameHdr id flags len) msg s =
  do
    let p_r = parseOrFeed (parseSettings frame) res msg
    case p_r of
      Done irem f -> do
        liftIO $ print f
        if len == 0 then sendAll s (BS.pack [0x00, 0x00, 0x00, 0x04, 0x01, 0x00, 0x00, 0x00, 0x00]) else return ()
        http2Server Nothing irem s
      Partial _ -> return ()
      Fail _ _ _ -> return ()
handleHttp2Settings res frame@(FrameHdr id flags len) msg s = http2Server Nothing msg s

checkClose :: [Field] -> Socket -> IO ()
checkClose f s =
  if (Prelude.any (== "close") $ findField f "connection")
    then close s
    else return ()

checkUpgrade :: [Field] -> Socket -> Bool
checkUpgrade f s =
  if (Prelude.any (== "h2c") $ findField f "upgrade")
    then trace "Upgrading" True
    else False

findField :: [Field] -> Text -> [Text]
findField f x = L.concat (L.map (\(Field _ v) -> v) . L.filter (\(Field n _) -> n == x) $ f)

sendNormalResp :: Request -> Socket -> IO ()
sendNormalResp (Request path _ (GetRequest h)) s =
  do
    sendAll s (TE.encodeUtf8 "HTTP/1.1 200 OK\r\n")
    let resp = TE.encodeUtf8 . T.concat $ ["You are on ", path, "\r\n"]
    sendAll s (TE.encodeUtf8 . T.concat $ ["Content-Length: ", T.pack . show . BS.length $ resp, "\r\n\r\n"])
    sendAll s resp
