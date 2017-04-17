{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Xsh.Arbitrary (
    genSoftChar
  , genUnquotedChar
  , genNameChar
  , genSoftText
  , genUnquotedText
  , withPosition
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

import           Test.QuickCheck (Arbitrary (..), Gen, oneof, resize, listOf1, elements, suchThat)

import           Test.QuickCheck.Instances ()

import qualified Text.Megaparsec.Pos as Mega

import           Xsh.Prelude
import           Xsh.Data

withPosition :: a -> Positioned a
withPosition =
  Positioned
    (Mega.SourcePos
      "Arbitrary"
      (maybe (error "Bad line") id $ Mega.mkPos (1 :: Int))
      (maybe (error "Bad column") id $ Mega.mkPos (1 :: Int)))

instance Arbitrary a => Arbitrary (Positioned a) where
  arbitrary =
    withPosition <$> arbitrary

instance Arbitrary Token where
  arbitrary =
    oneof [
        elements [
             AndToken
           , OrToken
           , StatementToken
           , PipeToken
           ]
      , WordToken <$> arbitrary
      ]

instance Arbitrary Word where
  arbitrary =
    Word <$> (resize 5 $ listOf1 arbitrary)

instance Arbitrary Part where
  arbitrary =
    resize 5 $
      oneof [
          fmap (HardQuotedPart . T.pack) $
            listOf1 $ oneof [genNameChar, elements "_-.,:+/@%${}#!&|"]
        , SoftQuotedPart <$> listOf1 arbitrary
        , UnquotedPart <$> listOf1 arbitrary
        ]

instance Arbitrary Fragment where
  arbitrary =
    oneof [
        fmap (TextFragment . T.pack) $
          listOf1 $ oneof [genNameChar, elements "_-.,:+/@%"]

      , fmap (VariableFragment . T.pack) $
          listOf1 genNameChar
      ]

genSoftChar :: Gen Char
genSoftChar =
  arbitrary `suchThat` (\c -> c >= '\x001f' && (not $ elem c ("`$\"\\\t\n\r\\'" :: [Char])))

genUnquotedChar :: Gen Char
genUnquotedChar =
  arbitrary `suchThat` (\c -> c >= '\x001f' && (not $ elem c ("\"'|&;()<> \t\n`$" :: [Char])))

genNameChar :: Gen Char
genNameChar =
  elements $ join [['A'..'Z'], ['a'..'z'], ['0'..'9'], "_"]

genSoftText :: Gen Text
genSoftText =
  fmap T.pack $
    listOf1 genSoftChar

genUnquotedText :: Gen Text
genUnquotedText =
  fmap T.pack $
    listOf1 genUnquotedChar
