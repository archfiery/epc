module Main where

import Tests.EPC

import Test.Hspec (hspec)

main :: IO ()
main = do
  hspec testCageOrDODAACChar
