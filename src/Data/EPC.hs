{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.EPC where

import           Control.Lens.TH
import           Control.Monad.Error.Lens
import           Control.Monad.Except
import           Data.Char
import           Text.Read

--SGTIN
--General syntax:
--urn:epc:id:sgtin:CompanyPrefix.ItemRefAndIndicator.SerialNumber
--
--SGTIN-URI ::= ``urn:epc:id:sgtin:'' SGTINURIBODY
--SGTINURIBody :: 2 * (PaddedNumericComponent ``.'') GS3A3Component

data EPCError = TypeMismatch
              | LengthOutOfBound
              deriving (Show, Eq)

makeClassyPrisms ''EPCError

-- |Returns a string made of Digits
makeDigitsStr :: (AsEPCError e, MonadError e m) => String -> m String
makeDigitsStr s = let rs = readMaybe s :: Maybe Integer in
               case rs of
                 Just _  -> pure s
                 Nothing -> throwing _TypeMismatch ()

type PadNum = String
padNum :: (AsEPCError e, MonadError e m) => String -> m PadNum
padNum = undefined

type GS3A3 = String
gS3A3 :: (AsEPCError e, MonadError e m) => String -> m PadNum
gS3A3 = undefined

type NumC = String
numC :: (AsEPCError e, MonadError e m) => String -> m PadNum
numC = undefined

type CPRef = String
cPRef :: (AsEPCError e, MonadError e m) => String -> m PadNum
cPRef = undefined

data CageOrDODAAC = CageCode | DODAAC
  deriving (Show, Eq)

type CageCode = [CageOrDODAACChar]

type DODAAC = [CageOrDODAACChar]

-- |Cage Code or DOD AAC Char
type CageOrDODAACChar = Char
cageOrDODAACChar :: (AsEPCError e, MonadError e m) => Char -> m CageOrDODAACChar
cageOrDODAACChar c
  | isUpper c || isDigit c = pure c
  | otherwise              = throwing _TypeMismatch ()

-- |ADI TDS1.9 Section 6.3.13
-- |Returns true if a Char is an Other ADI Char
isOtherADIChar :: Char -> Bool
isOtherADIChar c
  | c == '-' || c == '/' = True
  | otherwise            = False

-- |ADI Char
type ADIChar = Char

-- |Constructs an ADI char
aDIChar :: (AsEPCError e, MonadError e m) => Char -> m ADIChar
aDIChar c
  | isUpper c || isDigit c || isOtherADIChar c = pure c
  | otherwise                                  = throwing _TypeMismatch ()

type ADI = [ADIChar]

-- |Returns true if a string contains all valid ADI Char
allADIChar :: [Char] -> Bool
allADIChar s = case s of
                []     -> True
                (x:xs) -> let adi = (aDIChar x) :: Either EPCError Char in
                              case adi of
                                Left TypeMismatch -> False
                                Right _           -> allADIChar xs

-- |An ADI extended either starts with # or nothing followed by valid ADI chars
data ADIExtended = ADIExtended (Maybe Char) [ADIChar]
aDIExtended :: String -> Bool
aDIExtended s
  | (length s) >= 2 = case (head s) of
                        '#' -> allADIChar (tail s)
                        _   -> allADIChar s
  | length s == 1   = allADIChar s
  | otherwise       = False

data EPC = SGTIN PadNum PadNum GS3A3
         | SSCC PadNum PadNum
         | SGLN PadNum (Maybe PadNum) GS3A3
         | GRAI PadNum (Maybe PadNum) GS3A3
         | GIAI PadNum GS3A3
         | GSRN PadNum PadNum
         | GSRNP PadNum PadNum
         | GDTI PadNum (Maybe PadNum) GS3A3
         | CPI PadNum CPRef NumC
         | SGCN PadNum (Maybe PadNum) PadNum
         | GID NumC NumC NumC
         | DOD CageOrDODAAC NumC
         | ADI CageOrDODAAC
