{-# LANGUAGE OverloadedStrings #-}

module Lib (StompFrame(..), parseFrame, toByteString, headersBlock) where

import Data.Attoparsec.ByteString as AP
import Data.ByteString as B
import Data.Text as T
import Data.Text.Encoding as T

data StompFrame = StompFrame
  { command :: String
  , headers :: [(String, String)]
  , body   :: String
  } deriving (Show)

-- parsing steps
cmd = choice [
              string "CONNECT"
              , string "STOMP"
              , string "DISCONNECT"
              , string "SEND"
              , string "MESSAGE"
              , string "ERROR"
              , string "SUBSCRIBE"
              , string "UNSUBSCRIBE"
              , string "BEGIN"
              , string "ABORT"
              , string "COMMIT"
              , string "CONNECTED"
              ]

headersBlock = sepBy header newline
header = sepBy1 (takeWhile1 $ notInClass ":\n") colon

bodyParse contentLength = case contentLength of
                      Just n  -> AP.take n
                      Nothing -> AP.takeWhile $ notInClass "\NUL"

-- composed parser
stompFrame = do
  c <- cmd
  newline
  hb <- headersBlock
  let headerTuples = toTuples hb
  let cl = contentLength headerTuples
  afterHeaders headerTuples
  b <- bodyParse cl
  string "\NUL"
  return StompFrame{ command=toString c, headers=headerTuples, body=toString b }

parseFrame :: ByteString -> Result StompFrame
parseFrame = parse stompFrame

-- utilities
newline = string $ toByteString "\n"
colon   = string $ toByteString ":"

toByteString :: String -> ByteString
toByteString = T.encodeUtf8 . T.pack

toString :: ByteString -> String
toString = T.unpack . T.decodeUtf8 

toTuples :: [[ByteString]] -> [(String, String)]
toTuples []  = []
toTuples (x:xs) = if Prelude.length x == 2
                 then (toString $ Prelude.head x, toString $ Prelude.last x) : toTuples xs 
                 else error $ "bad parse: header: " ++ Prelude.concatMap toString x

contentLength :: [(String, String)] -> Maybe Int
contentLength xs = case Prelude.filter clFilter xs of
              []  -> Nothing
              [x] -> Just $ read $ snd x
              ys  -> Just $ read $ snd $ Prelude.last ys
  where clFilter = (==) "content-length" . fst

afterHeaders [] = newline
afterHeaders _  = string $ toByteString "\n\n"
