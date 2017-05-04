{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib


main :: IO ()
main = challengeFour


challengeThree :: IO ()
challengeThree = do
    decodedStr <- decryptSCXor
      (decodeHex "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")
    putStrLn decodedStr

challengeFour :: IO ()
challengeFour = do
    topStrings <- loadCyphertexts "data/challenge-4.txt" >>= decryptCyphertexts
    putStrLn $ show $ (sortByRating topStrings) !! 0
