{-# LANGUAGE OverloadedStrings #-}

module Main where


main :: IO ()
main = putStrLn "dummy"
--main = do ratedStrings <- rankXors "abcdefghijklmnopqrstuvwxyz" (decodeHex "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736")
--
--unpackEm :: [RatedString] -> IO ()
--unpackEm [] = do putStrLn "Done"
--unpackEm ((RatedString str rating):xs) = do putStrLn (unpack str)
--                                            unpackEm xs
