{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Base64.Lazy as B64 (encode)
import qualified Data.ByteString.Base16.Lazy as B16 (decode, encode)
--import Data.ByteString.Lazy (pack, putStrLn, zipWith)
import qualified Data.Bits  as Bits (xor)

-- base16 encoded bytestring
data Base16 = Base16 B.ByteString
  deriving (Eq)

instance Show Base16 where
    show (Base16 bytes) = show $ B16.encode bytes

-- base64 encoded bytestring
type Base64 = B.ByteString

hexToBase64 :: Base16 -> Base64
hexToBase64 (Base16 bytes) = B64.encode bytes

decodeB16 :: B.ByteString -> Base16
decodeB16 = Base16 . fst . B16.decode

xor :: Base16 -> Base16 -> Base16
xor (Base16 str1) (Base16 str2) = Base16 $ B.pack $ B.zipWith Bits.xor (str1) (str2)
