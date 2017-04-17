{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Xsh.Expansion (
    expand
  , part
  , fragment
  ) where

import           Data.Text (Text)

import           Xsh.Data
import           Xsh.Prelude

--
-- Expand word into text.
--
expand :: Word -> Text
expand (Word a) =
  mconcat . fmap part $ a

--
-- Expand part of a word into text.
--
part :: Part -> Text
part p =
  case p of
    HardQuotedPart t ->
      t
    SoftQuotedPart t ->
      mconcat . fmap fragment $ t
    UnquotedPart t ->
      mconcat . fmap fragment $ t

--
-- Expand fragment of a part of a word into text.
--
-- Note: In the baseline version, I have included variable's in the
-- lexer, but supported them here yet, to give a head-start for phase
-- two. The variable case can be stubbed out for now.
--
fragment :: Fragment -> Text
fragment f =
  case f of
    TextFragment t ->
      t
    -- PHASE 2: ADD EXPANSION (will require changes to pass environment)
    VariableFragment _t ->
      "TODO"
