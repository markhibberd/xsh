{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Xsh.Parser (
    parse
  , expect
  , word
  , command
  , pipeline
  , list
  , TokenStream (..)
  ) where

import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Set as Set

import           System.IO (FilePath)

import qualified Text.Megaparsec as Mega
import           Text.Megaparsec (Dec)

import           Xsh.Data
import qualified Xsh.Lexer as Lexer
import           Xsh.Prelude


--
-- A convienience function to lex and parse a program.
--
-- Please excuse the stringy error message, these can be done much
-- better, megaparsec allows for typed error messages and stores a
-- complete annotated doc for the error message. String is just done
-- here in haste.
--
parse :: FilePath -> [Char] -> Either [Char] Program
parse p input = do
  tokens <- first Mega.parseErrorPretty $ Mega.parse Lexer.program p input
  first Mega.parseErrorPretty $ Mega.parse program p (TokenStream tokens)

--
-- Our parser takes a stream of positioned tokens and produces a program.
--

type Parser =
  Mega.Parsec Dec TokenStream

--
-- Programs are series of lists, separated by statement terminators.
--
-- It is valid to have statement terminator (multiple even) without
-- a pipeline before it, these are just ignored.
--
program :: Parser Program
program = do
  _ <- many $ expect StatementToken
  Mega.choice [
      do x <- list
         Program y <- program
         pure . Program $ x : y
    , pure $ Program []
    ]

--
-- BASELINE EXERCISE 15.
--
-- Lists are a series of pipelines separated by '&&' or '||'.
--
-- Important: lists are left associative.
--
list :: Parser List
list =
  pipeline >>= list' . SingletonList

list' :: List -> Parser List
list' x =
  let
    pand = do
      expect AndToken
      y <- pipeline
      list' $ AndList x y

    por = do
      expect OrToken
      y <- pipeline
      list' $ OrList x y
  in
    Mega.choice [pand, por, pure x]

--
-- BASELINE EXERCISE 14.
--
-- Pipelines are a series of '|' separated commands.
--
-- Important: pipelines are left associative.
--
pipeline :: Parser Pipeline
pipeline =
  command >>= pipeline' . SingletonPipeline

pipeline' :: Pipeline -> Parser Pipeline
pipeline' x =
  let
    pcompound = do
      expect PipeToken
      y <- command
      pipeline' $ CompoundPipeline x y
  in
    Mega.choice [pcompound, pure x]

--
-- BASELINE EXERCISE 13.
--
--
-- A simple command is made up of one or more words.
--
command :: Parser Command
command =
  Command <$> some word

--
-- BASELINE EXERCISE 12.
--
-- Parser that matches a single word.
--
-- Hint:
--   satisfyM
--
word :: Parser Word
word =
  satisfyM $ \t ->
    case getPositioned t of
      WordToken tt ->
        Just tt
      _ ->
        Nothing

--
-- BASELINE EXERCISE 11.
--
--
-- Expect a specific token.
--
-- Hint:
--   satisfyM
--
expect :: Token -> Parser ()
expect x =
  satisfyM $ \t ->
    case getPositioned t == x of
      True ->
        Just ()
      False ->
        Nothing

-- --
-- A satisfy method that allows you to transform token at the same time
-- as matching, this is useful as we are dealing with our own tokens that
-- don't work with the built in satisfy.
-- --

satisfyM :: (Positioned Token -> Maybe a) -> Parser a
satisfyM f =
  let
    test x =
      case f . getWrappedToken $ x of
        Just z ->
          Right z
        Nothing ->
          Left (Set.singleton (Mega.Tokens (x:|[])), Set.empty, Set.empty)
  in
    Mega.token test Nothing

-- --
-- Some megaparsec shenanigans to use our Token as the parser Token.
-- --

newtype TokenStream =
  TokenStream [Positioned Token]

newtype WrappedToken =
  WrappedToken {
      getWrappedToken :: Positioned Token
    } deriving (Eq, Show, Ord)

instance Mega.Stream TokenStream where
  type Token TokenStream = WrappedToken
  uncons (TokenStream []) = Nothing
  uncons (TokenStream (t:ts)) = Just (WrappedToken t, TokenStream ts)
  updatePos _ _pos spos tok = (spos, getPosition . getWrappedToken $ tok)

instance Mega.ShowToken WrappedToken where
  showTokens =
    show
