{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( someFunc,
    httpServer,
    runTCPServer,
  )
where

import ASCII.Char as ASCII (Char (Comma), toInt)
import Control.Concurrent (forkFinally)
import qualified Control.Exception as E
import Control.Monad (forever, void)
import Data.Attoparsec.ByteString as AB
import Data.Attoparsec.ByteString.Char8 as AC
import Data.ByteString
import qualified Data.ByteString as BS
import qualified Data.List as L
import Data.Text as T
import Data.Text.Encoding as TE
import Data.Word (Word8)
import Debug.Trace (trace)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Parsers

someFunc :: IO ()
someFunc = putStrLn "someFunc"

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

httpServer :: Socket -> IO ()
httpServer = httpServer' Nothing Nothing

httpServer' :: Maybe (IResult ByteString (Request, [Field])) -> Maybe ByteString -> Socket -> IO ()
httpServer' res Nothing s =
  do
    msg <- recv s 4096
    httpServer'' res msg s
httpServer' res (Just irem) s = httpServer'' res irem s

httpServer'' :: Maybe (IResult ByteString (Request, [Field])) -> ByteString -> Socket -> IO ()
httpServer'' res msg s =
  do
    let p_r = parseOrFeed (parseReqFirst >>= \x -> parseAllFields [] >>= \y -> return (x, y)) res msg
    case p_r of
      Done irem x@(Request Get path _, h) ->
        if checkUpgrade h s
          then do
            putStrLn "do upgrade"
            sendAll s (TE.encodeUtf8 "HTTP/1.1 101 Switching Protocols\r\n")
            sendAll s (TE.encodeUtf8 "Connection: Upgrade\r\n")
            sendAll s (TE.encodeUtf8 "Upgrade: h2c\r\n\r\n")
            sendAll
              s
              ( BS.pack
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
              )
            http2ServerStart' Nothing (if BS.null irem then Nothing else Just irem) s
          else do
            sendNormalResp x s
            print x
            checkClose h s
            httpServer' Nothing (if BS.null irem then Nothing else Just irem) s
      Done irem x@(Request Put path _, h) -> do
        print x
      Done irem x@(Request Post path _, h) -> do
        print x
      Done irem x@(Request Head path _, h) -> do
        print x
      Done irem x@(Request Delete path _, h) -> do
        print x
      Done irem x@(Request Connect path _, h) -> do
        print x
      Partial _ -> httpServer' (Just p_r) Nothing s
      Fail irem _ perror -> do
        putStrLn perror
        print irem

http2ServerStart' :: Maybe (IResult ByteString ()) -> Maybe ByteString -> Socket -> IO ()
http2ServerStart' res Nothing s =
  do
    msg <- recv s 4096
    http2ServerStart'' res msg s
http2ServerStart' res (Just irem) s = http2ServerStart'' res irem s

http2ServerStart'' :: Maybe (IResult ByteString ()) -> ByteString -> Socket -> IO ()
http2ServerStart'' res msg s =
  do
    let p_r = parseOrFeed (parseHttp2Magic) res msg
    case p_r of
      Done irem () -> putStrLn "Got magic" >> http2Server'' Nothing irem s
      Partial _ -> http2ServerStart' (Just p_r) Nothing s
      Fail irem _ perror -> do
        putStrLn perror
        print irem

http2Server'' :: Maybe (IResult ByteString ()) -> ByteString -> Socket -> IO ()
http2Server'' res msg s =
  do
    -- base <- AB.take 3
    putStrLn ""

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

sendNormalResp :: (Request, [Field]) -> Socket -> IO ()
sendNormalResp (Request Get path _, h) s =
  do
    sendAll s (TE.encodeUtf8 "HTTP/1.1 200 OK\r\n")
    let resp = TE.encodeUtf8 . T.concat $ ["You are on ", path, "\r\n"]
    sendAll s (TE.encodeUtf8 . T.concat $ ["Content-Length: ", T.pack . show . BS.length $ resp, "\r\n\r\n"])
    sendAll s resp
