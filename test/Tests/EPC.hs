{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Tests.EPC where

import           Data.EPC
import           Test.Hspec

type EitherEC = Either EPCError CageOrDODAACChar

testCageOrDODAACChar :: Spec
testCageOrDODAACChar = do
  describe "Cage code or DOD AAC char" $ do
    it "Upper case chars are valid char set" $
      ((cageOrDODAACChar <$> ['A'..'Z']) :: [EitherEC]) `shouldBe` (Right <$> ['A'..'Z'])

    it "Lower case chars are invalid char set" $
      ((cageOrDODAACChar <$> ['a'..'z']) :: [EitherEC]) `shouldBe` ([Left TypeMismatch] <* [1..26])

    it "Digit chars are valid char set" $
      ((cageOrDODAACChar <$> ['0'..'9']) :: [EitherEC]) `shouldBe` (Right <$> ['0'..'9'])
