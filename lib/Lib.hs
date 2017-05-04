{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Base64 as B64 (encode)
import qualified Data.ByteString.Base16 as B16 (decode)
import qualified Data.Bits  as Bits (xor)
import Data.List (sortOn)
import Data.Map.Strict (Map, fromList, fromListWith, mapMaybe, unionWith)

-- Hex encoded bytestring
type Hex = B.ByteString

-- Base64 encoded bytestring
type Base64 = B.ByteString

hexToBase64 :: Hex -> Base64
hexToBase64 bytes = B64.encode $ decodeHex bytes

decodeHex :: B.ByteString -> Hex
decodeHex = fst . B16.decode

xor :: B.ByteString -> B.ByteString -> B.ByteString
xor str1 str2 = B.pack $ B.zipWith Bits.xor str1 str2

histogram :: String -> Map Char Int
histogram str = fromListWith (+) [(chr, 1) | chr <- str]

frequency :: String -> Map Char Float
frequency str = mapMaybe freq hist
    where hist = histogram str
          len = length str
          freq cnt = Just $ (fromIntegral cnt) / (fromIntegral len)

englishCharFreq :: Map Char Float
englishCharFreq = fromList [ ('a',8.167), ('b',1.492), ('c',2.782),
  ('d',4.253), ('e',12.702), ('f',2.228), ('g',2.015), ('h',6.094),
  ('i',6.966), ('j',0.153), ('k',0.772), ('l',4.025), ('m',2.406),
  ('n',6.749), ('o',7.507), ('p',1.929), ('q',0.095), ('r',5.987),
  ('s',6.327), ('t',9.056), ('u',2.758), ('v',0.978), ('w',2.360),
  ('x',0.150), ('y',1.974), ('z',0.074) ]


similarMaps :: Map Char Float -> Map Char Float -> Float
similarMaps ref test = sqrt $ foldr (+) 0 mapOfSquareDiffs
    where mapOfSquareDiffs = mapMaybe (\x -> Just $ x**2) $ unionWith diff ref test
          diff r t = (r - t)

singleCharXor :: Char -> B.ByteString -> B.ByteString
singleCharXor chr str = xor ((C.replicate (B.length str) chr) :: B.ByteString) str

data RatedString = RatedString String Float
  deriving (Show)

xorAndRate :: Char -> B.ByteString -> RatedString
xorAndRate chr str = RatedString decodedStr rating
    where decodedStr = C.unpack $ singleCharXor chr str
          rating = similarMaps (frequency decodedStr) englishCharFreq

rankXors :: [Char] -> B.ByteString -> [RatedString]
rankXors chars str = sortOn getRating unsortedXors
    where unsortedXors = map (flip xorAndRate str) chars
          getRating (RatedString _ flt) = flt

bestMatch :: [Char] -> B.ByteString -> String
bestMatch chars str = getStr ((rankXors chars str) !! 0)
    where getStr (RatedString str _) = str

