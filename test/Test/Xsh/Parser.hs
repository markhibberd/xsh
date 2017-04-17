{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Xsh.Parser where

import           Test.QuickCheck (forAll, listOf1, arbitrary, quickCheckAll, (===))
import           Test.QuickCheck.Instances ()
import           Test.Xsh.Arbitrary

import           Text.Megaparsec (parseMaybe)

import           Xsh.Data
import qualified Xsh.Parser as Parser
import           Xsh.Prelude


-- BASELINE EXERCISE 11.

prop_expect_ok t =
  check (Parser.expect t) [t] $
    ()

prop_expect_not_ok =
  isFailure (Parser.expect AndToken) [OrToken]

-- BASELINE EXERCISE 12.

prop_word_ok w =
  check Parser.word [WordToken w] $
    w

-- BASELINE EXERCISE 13.

prop_command_ok =
  forAll (listOf1 arbitrary) $ \ws ->
    check Parser.command (WordToken <$> ws) $
      Command ws

prop_command_not_ok =
  isFailure Parser.command []

-- BASELINE EXERCISE 14.

prop_pipeline_single =
  forAll (listOf1 arbitrary) $ \words ->
    check Parser.pipeline (WordToken <$> words) $
      SingletonPipeline (Command words)

prop_pipeline_piped =
  forAll (listOf1 arbitrary) $ \words1 ->
  forAll (listOf1 arbitrary) $ \words2 ->
    check Parser.pipeline (join $ [
        WordToken <$> words1
      , [PipeToken]
      , WordToken <$> words2]) $
        CompoundPipeline
          (SingletonPipeline $ Command words1)
          (Command words2)

prop_pipeline_piped_piped =
  forAll (listOf1 arbitrary) $ \words1 ->
  forAll (listOf1 arbitrary) $ \words2 ->
  forAll (listOf1 arbitrary) $ \words3 ->
    check Parser.pipeline (join $ [
        WordToken <$> words1
      , [PipeToken]
      , WordToken <$> words2
      , [PipeToken]
      , WordToken <$> words3
      ]) $
        CompoundPipeline
          (CompoundPipeline
             (SingletonPipeline $ Command words1) (Command words2))
             (Command words3)

-- BASELINE EXERCISE 15.

prop_list_single =
  forAll (listOf1 arbitrary) $ \words ->
    check Parser.list (WordToken <$> words) $
      SingletonList (SingletonPipeline (Command words))

prop_list_and =
  forAll (listOf1 arbitrary) $ \words1 ->
  forAll (listOf1 arbitrary) $ \words2 ->
    check Parser.list (join $ [
        WordToken <$> words1
      , [AndToken]
      , WordToken <$> words2
      ]) $
        AndList
          (SingletonList (SingletonPipeline (Command words1)))
          (SingletonPipeline (Command words2))

prop_list_and_pipe =
  forAll (listOf1 arbitrary) $ \words1 ->
  forAll (listOf1 arbitrary) $ \words2 ->
  forAll (listOf1 arbitrary) $ \words3 ->
    check Parser.list (join $ [
        WordToken <$> words1
      , [AndToken]
      , WordToken <$> words2
      , [PipeToken]
      , WordToken <$> words3
      ]) $
        AndList
          (SingletonList (SingletonPipeline (Command words1)))
          (CompoundPipeline (SingletonPipeline (Command words2)) (Command words3))

prop_list_pipe_and =
  forAll (listOf1 arbitrary) $ \words1 ->
  forAll (listOf1 arbitrary) $ \words2 ->
  forAll (listOf1 arbitrary) $ \words3 ->
    check Parser.list (join $ [
        WordToken <$> words1
      , [PipeToken]
      , WordToken <$> words2
      , [AndToken]
      , WordToken <$> words3
      ]) $
        AndList
          (SingletonList (CompoundPipeline (SingletonPipeline (Command words1)) (Command words2)))
          (SingletonPipeline (Command words3))

check p input output =
  parseMaybe p (Parser.TokenStream (withPosition <$> input)) === Just output

isFailure p input =
  parseMaybe p (Parser.TokenStream (withPosition <$> input)) === Nothing

return []
tests = $quickCheckAll
