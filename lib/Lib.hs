{-# LANGUAGE OverloadedStrings #-}

module Lib where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Base64 as B64 (encode)
import qualified Data.ByteString.Base16 as B16 (decode)
import qualified Data.Bits  as Bits (xor)
import Data.List (sortOn)
import Data.Char (toLower)
import Data.Map.Strict (Map, fromList, fromListWith, mapMaybe, unionWith)

import Control.Monad (liftM)
import Test.RandomStrings

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
englishCharFreq = fromList [ ('a',0.8167), ('b',0.1492), ('c',0.2782),
  ('d',0.4253), ('e',0.12702), ('f',0.2228), ('g',0.2015), ('h',0.6094),
  ('i',0.6966), ('j',0.0153), ('k',0.0772), ('l',0.4025), ('m',0.2406),
  ('n',0.6749), ('o',0.7507), ('p',0.1929), ('q',0.0095), ('r',0.5987),
  ('s',0.6327), ('t',0.9056), ('u',0.2758), ('v',0.0978), ('w',0.2360),
  ('x',0.0150), ('y',0.1974), ('z',0.0074), (' ',0.18)]


similarMaps :: Map Char Float -> Map Char Float -> Float
similarMaps ref test = sqrt $ foldr (+) 0 mapOfSquareDiffs
    where mapOfSquareDiffs = mapMaybe (\x -> Just $ x**2) $ unionWith diff ref test
          diff r t = (r - t)

singleCharXor :: Char -> B.ByteString -> B.ByteString
singleCharXor chr str = repeatedKeyXor (C.pack [chr]) str

repeatedKeyXor :: B.ByteString -> B.ByteString -> B.ByteString
repeatedKeyXor key bytes = xor repeatedKey bytes
    where keyLength = B.length key
          keyStr = B.unpack key
          bLength = B.length bytes
          numKeys = div bLength keyLength
          remainder = mod bLength keyLength
          repeatedKey = B.pack $ concat (replicate numKeys keyStr) ++
            (take remainder keyStr)

data RatedString = RatedString String Float
  deriving (Show, Eq)

xorAndRate :: Char -> B.ByteString -> RatedString
xorAndRate chr str = RatedString decodedStr rating
    where decodedStr = C.unpack $ singleCharXor chr str
          rating = similarMaps (frequency (cleanString decodedStr)) englishCharFreq
          cleanString str = map toLower str

rankXors :: [Char] -> B.ByteString -> [RatedString]
rankXors chars str = sortByRating unsortedXors
    where unsortedXors = map (flip xorAndRate str) chars

sortByRating :: [RatedString] -> [RatedString]
sortByRating strings = sortOn getRating strings
    where getRating (RatedString _ flt) = flt

randomSCXor :: B.ByteString -> IO [RatedString]
randomSCXor bytes =
  liftM (flip rankXors bytes) (liftM rmdups $ randomString randomChar 1000)
    where rmdups (x:xs)   | x `elem` xs   = rmdups xs
                          | otherwise     = x : rmdups xs
          rmdups [] = []

decryptSCXor :: B.ByteString -> IO String
decryptSCXor bytes = do ratedStrings <- (randomSCXor bytes)
                        return $ getStr (ratedStrings !! 0)
    where getStr (RatedString str _) = str

loadCyphertexts :: FilePath -> IO [B.ByteString]
loadCyphertexts path = do contents <- C.readFile path
                          return $ C.split '\n' contents

decryptCyphertexts :: [B.ByteString] -> IO [RatedString]
decryptCyphertexts texts = do ratedStringLists <- mapM (randomSCXor . decodeHex) texts
                              return $ map (!! 0) ratedStringLists


