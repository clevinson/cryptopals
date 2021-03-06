{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit

import Lib

import Data.Map.Strict (fromList)
import Data.ByteString

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Unit Tests" [
  testCase "http://cryptopals.com/sets/1/challenges/1 (hex => Base64)" $
    (hexToBase64 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")
    @?= "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t",

  testCase "http://cryptopals.com/sets/1/challenges/2 (fixed xor)" $
    xor
      (decodeHex "1c0111001f010100061a024b53535009181c")
      (decodeHex "686974207468652062756c6c277320657965")
    @?= (decodeHex "746865206b696420646f6e277420706c6179"),

  testCase "frequency" $
    frequency "fooba" @?= fromList [('f',0.2), ('o',0.4), ('b',0.2), ('a',0.2)],

  testCase "similarMaps" $
    similarMaps
      (fromList [('a', 1), ('b', 2)])
      (fromList [('a', 4), ('b', 6)])
    @?= 5.0,

  testCase "similarMaps (missing key)" $
    similarMaps
      (fromList [('b', 2)])
      (fromList [('a', 3), ('b', 6)])
    @?= 5.0,

  testCase "singleCharXor (identity map)" $
    ((singleCharXor '!' (singleCharXor '!' "hello there")))
    @?= ("hello there" :: ByteString),

  testCaseInfo "http://cryptopals.com/sets/1/challenges/3 (decryptSCXor)" $
    decryptSCXor
      (decodeHex "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"),

  testCase "http://cryptopals.com/sets/1/challenges/5 (repeatedKeyXor)" $
    repeatedKeyXor
      ("ICE" :: ByteString)
      "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal"
    @?= (decodeHex "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f")
  ]
