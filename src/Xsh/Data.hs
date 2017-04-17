{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Xsh.Data (
  -- * Environment Types
    Name (..)
  , Value (..)
  , Variable (..)
  , Environment (..)
  -- * Lexer Types
  , Token (..)
  , Word (..)
  , Part (..)
  , Fragment (..)
  , Positioned (..)
  , renderToken
  , renderWord
  , renderPart
  , renderFragment
  -- * Parser Types
  , Program (..)
  , List (..)
  , Pipeline (..)
  , Command (..)
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

import qualified Text.Megaparsec.Pos as Mega

import           Xsh.Prelude

newtype Name =
  Name {
      getName :: Text
    } deriving (Eq, Show)

newtype Value =
  Value {
      getValue :: Text
    } deriving (Eq, Show)

data Variable =
  Variable {
      variableName :: Name
    , variableValue :: Value
    } deriving (Eq, Show)

newtype Environment =
  Environment {
      getEnvironment :: [Variable]
    } deriving (Eq, Show)

-- =====================================
-- Lexer types
-- =====================================

data Token =
    WordToken Word
  | AndToken
  | OrToken
  | StatementToken
  | PipeToken
    deriving (Eq, Show, Ord)

renderToken :: Token -> Text
renderToken t =
  case t of
    WordToken w ->
      renderWord w
    AndToken ->
      "&&"
    OrToken ->
      "||"
    StatementToken ->
      ";"
    PipeToken ->
      "|"

data Word =
    Word [Part]
    deriving (Eq, Show, Ord)

renderWord :: Word -> Text
renderWord w =
  case w of
    Word parts ->
      mconcat (renderPart <$> parts)

data Part =
    HardQuotedPart Text
  | SoftQuotedPart [Fragment]
  | UnquotedPart [Fragment]
    deriving (Eq, Show, Ord)

renderPart :: Part -> Text
renderPart p =
  case p of
    HardQuotedPart t ->
      "'" <> t <> "'"
    SoftQuotedPart fs ->
      let
        s =
          T.replace "\t" "\\\t" $
          T.replace "\n" "\\\n" $
          T.replace "\r" "\\\r" $
          T.replace "\\" "\\\\" $
          T.replace "'" "\\'" $
          T.replace "\"" "\\\"" $
            mconcat (renderFragment <$> fs)
      in
        "\"" <> s <> "\""
    UnquotedPart fs ->
      T.replace " " "\\ "$
        mconcat (renderFragment <$> fs)

data Fragment =
    TextFragment Text
  | VariableFragment Text
    deriving (Eq, Show, Ord)

renderFragment :: Fragment -> Text
renderFragment f =
  case f of
    TextFragment t ->
      t
    VariableFragment t ->
      "${" <> t <> "}"

data Positioned a =
    Positioned {
        getPosition :: Mega.SourcePos
      , getPositioned :: a
      } deriving (Eq, Show, Ord)

-- =====================================
-- Parser types
-- =====================================

data Command =
    Command [Word]
    deriving (Eq, Show)

data Pipeline =
    SingletonPipeline Command
  | CompoundPipeline Pipeline Command
    deriving (Eq, Show)

data List =
    SingletonList Pipeline
  | AndList List Pipeline
  | OrList List Pipeline
    deriving (Eq, Show)

data Program =
    Program [List]
    deriving (Eq, Show)
