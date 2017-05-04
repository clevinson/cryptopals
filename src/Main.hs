{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib


main :: IO ()
main = do topStrings <- loadCyphertexts "data/challenge-4.txt" >>= decryptCyphertexts
          putStrLn $ show $ (sortByRating topStrings) !! 0
