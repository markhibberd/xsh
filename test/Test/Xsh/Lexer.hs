{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Xsh.Lexer where

import qualified Data.Text as T

import           Test.QuickCheck (forAll, conjoin, counterexample, quickCheckAll, (===), elements, arbitrary, suchThat, listOf1)
import           Test.QuickCheck.Instances ()
import           Test.Xsh.Arbitrary

import           Text.Megaparsec (parseMaybe)

import           Xsh.Data
import qualified Xsh.Lexer as Lexer
import           Xsh.Prelude

-- BASELINE EXERCISE 1

prop_unquoted_char_escapes =
  check Lexer.unquotedChar "\\ " $
    ' '

prop_unquoted_car_failures =
  forAll (elements "\"'|&;()<> \t\n`$") $ \c ->
    isFailure Lexer.unquotedChar [c]

prop_unquoted_char_success =
  forAll genUnquotedChar $ \c ->
    check Lexer.unquotedChar [c] $
      c

-- BASELINE EXERCISE 2

prop_soft_char_escapes =
  conjoin [
      counterexample "Tab" $
        check Lexer.softChar "\\t" $
          '\t'

    , counterexample "Newline" $
        check Lexer.softChar "\\n" $
          '\n'

    , counterexample "Carriage return" $
        check Lexer.softChar "\\r" $
          '\r'

    , counterexample "Bashslash" $
        check Lexer.softChar "\\\\" $
          '\\'

    , counterexample "Hardquote" $
        check Lexer.softChar "\\'" $
          '\''

    , counterexample "Softquote" $
        check Lexer.softChar "\\\"" $
          '"'
    ]

prop_soft_car_failures =
  forAll (elements "`$\"") $ \c ->
    isFailure Lexer.softChar [c]


prop_soft_char_success =
  forAll genSoftChar $ \c ->
    check Lexer.softChar [c] $
      c

-- BASELINE EXERCISE 3

prop_name_car_failures =
  forAll (arbitrary `suchThat` (not . flip elem (join [['A'..'Z'], ['a'..'z'], ['0'..'9'], "_"]))) $ \c ->
    isFailure Lexer.nameChar [c]


prop_name_char_success =
  forAll genNameChar $ \c ->
    check Lexer.nameChar [c] $
      c


-- BASELINE EXERCISE 4

prop_soft_text_fragment_success =
  forAll genSoftText $ \t ->
    check Lexer.softText (T.unpack t) $
      TextFragment t

-- BASELINE EXERCISE 5

prop_unquoted_text_fragment_success =
  forAll genUnquotedText $ \t ->
    check Lexer.unquotedText (T.unpack t) $
      TextFragment t

-- BASELINE EXERCISE 6

prop_variable_fragement_basic_success =
  forAll (listOf1 genNameChar) $ \n ->
    check Lexer.variable ("$" <> n) $
      VariableFragment (T.pack n)

prop_variable_fragement_braces_success =
  forAll (listOf1 genNameChar) $ \n ->
    check Lexer.variable ("${" <> n <> "}") $
      VariableFragment (T.pack n)

-- BASELINE EXERCISE 7

prop_unquoted_part =
  conjoin [
    counterexample "Valid text unquoted part." $
      check Lexer.unquoted (T.unpack . renderPart $ UnquotedPart [TextFragment "ABC"]) $
        UnquotedPart [TextFragment "ABC"]

  , counterexample "Valid variable unquoted part." $
      check Lexer.unquoted (T.unpack . renderPart $ UnquotedPart [VariableFragment "DEF"]) $
        UnquotedPart [VariableFragment "DEF"]

  , counterexample "Valid combined unquoted part." $
      check Lexer.unquoted (T.unpack . renderPart $ UnquotedPart [TextFragment "ABC", VariableFragment "DEF", TextFragment "GHI"]) $
        UnquotedPart [TextFragment "ABC", VariableFragment "DEF", TextFragment "GHI"]
  ]

-- BASELINE EXERCISE 8

prop_soft_part =
  conjoin [
    counterexample "Valid text soft quoted part." $
      check Lexer.soft (T.unpack . renderPart $ SoftQuotedPart [TextFragment "ABC"]) $
        SoftQuotedPart [TextFragment "ABC"]

  , counterexample "Valid variable soft quoted part." $
      check Lexer.soft (T.unpack . renderPart $ SoftQuotedPart [VariableFragment "DEF"]) $
        SoftQuotedPart [VariableFragment "DEF"]

  , counterexample "Valid combined soft quoted part." $
      check Lexer.soft (T.unpack . renderPart $ SoftQuotedPart [TextFragment "ABC", VariableFragment "DEF", TextFragment "GHI"]) $
        SoftQuotedPart [TextFragment "ABC", VariableFragment "DEF", TextFragment "GHI"]
  ]

-- BASELINE EXERCISE 9

prop_hard_part =
  check Lexer.hard (T.unpack . renderPart $ HardQuotedPart "AB!@#$%%^&*(C") $
    HardQuotedPart  "AB!@#$%%^&*(C"


-- BASELINE EXERCISE 10

prop_examples =
  conjoin [
      counterexample "Unquoted word" $
        check Lexer.word "unquoted" $
          Word [UnquotedPart [TextFragment "unquoted"]]

    , counterexample "Soft-quoted word" $
        check Lexer.word "\"soft\"" $
          Word [SoftQuotedPart [TextFragment "soft"]]

    , counterexample "Soft-quoted word with space" $
        check Lexer.word "\"soft with space\"" $
          Word [SoftQuotedPart [TextFragment "soft with space"]]

    , counterexample "Soft-quoted with escaped soft-quote" $
        check Lexer.word "\"soft\\\"quotes\"" $
          Word [SoftQuotedPart [TextFragment "soft\"quotes"]]

    , counterexample "Soft-quoted with hard-quote" $
        check Lexer.word "\"soft'quotes\"" $
          Word [SoftQuotedPart [TextFragment "soft'quotes"]]

    , counterexample "Hard-quoted word" $
         check Lexer.word "'hard'" $
          Word [HardQuotedPart "hard"]

    , counterexample "Hard-quoted word with space" $
        check Lexer.word "'hard with space'" $
          Word [HardQuotedPart "hard with space"]

    , counterexample "Hard-quoted with soft-quote" $
        check Lexer.word "'hard\"quotes'" $
          Word [HardQuotedPart "hard\"quotes"]

    , counterexample "Compound word" $
        check Lexer.word "Fred\\ $Flinstone\" \"from' bedrock'" $
          Word [
              UnquotedPart [
                TextFragment "Fred "
              , VariableFragment "Flinstone"
              ]
            , SoftQuotedPart [
                TextFragment " "
              ]
            , UnquotedPart [
                TextFragment "from"
              ]
            , HardQuotedPart " bedrock"
            ]
    ]

check p input output =
  parseMaybe p input === Just output

isFailure p input =
  parseMaybe p input === Nothing

return []
tests = $quickCheckAll
