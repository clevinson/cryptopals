{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit

import Lib
import qualified Data.Binary as Bin

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit Tests" [
  testCase "http://cryptopals.com/sets/1/challenges/1 (hex => Base64)" $
    (hexToBase64 (decodeB16 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"))
    @?= "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t",
  testCase "http://cryptopals.com/sets/1/challenges/2 (fixed xor)" $
    xor
      (decodeB16 "1c0111001f010100061a024b53535009181c")
      (decodeB16 "686974207468652062756c6c277320657965")
    @?= (decodeB16 "746865206b696420646f6e277420706c6179"),
  testCase "attempt 2" $
    xor
      (Base16 $ Bin.encode (0x1c0111001f010100061a024b53535009181c :: Integer))
      (Base16 $ Bin.encode (0x686974207468652062756c6c277320657965 :: Integer))
    @?= (Base16 $ Bin.encode (0x746865206b696420646f6e277420706c6179 :: Integer))
  ]
