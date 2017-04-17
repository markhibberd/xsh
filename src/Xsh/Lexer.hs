{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Xsh.Lexer (
    Lexer
  , program
  , word
  , hard
  , soft
  , unquoted
  , variable
  , unquotedText
  , softText
  , nameChar
  , softChar
  , unquotedChar
  ) where

import           Data.Char (isSpace)
import qualified Data.Text as T

import qualified Text.Megaparsec as Mega
import qualified Text.Megaparsec.Lexer as Lexer
import           Text.Megaparsec (Dec)

import           Text.Megaparsec (choice)
import           Text.Megaparsec.Char (satisfy, string, char)
import           Text.Megaparsec.Lexer (lexeme)

import           Xsh.Data
import           Xsh.Prelude


--
-- Our lexer takes a stream of characters and produces a stream of
-- positioned tokens.
--
type Lexer =
  Mega.Parsec Dec [Char]

--
-- To lex a complete shell program, we:
--
-- 1. Ignore any leading spaces or commants
-- 2. Repeatedly lex zero or more program tokens until eof
--
-- Hints:
--   'spaceOrComment'
--   `Mega.eof'
--
program :: Lexer [Positioned Token]
program =
  spaceOrComment *> many token <* Mega.eof

--
-- Tokens that we care about so far.
--
-- Baseline:
--   &&
--   ||
--   ;
--   |
--   word
--
--
-- Later if you want to expand this shell you would include:
--   if/then/fi
--   case/esac
--   while/do/done
--   for/do/done
--   redirections, i.e. < > <& >& >>
--   here doc, i.e. << <<-
--   function
--   subshells, i.e. ()
--   statement blocks, i.e. {}
--
token :: Lexer (Positioned Token)
token =
  lexeme spaceOrComment $ Mega.getPosition >>= \p -> fmap (Positioned p) $ choice [
      AndToken <$ string "&&"
    , OrToken <$ string "||"
    , StatementToken <$ string ";"
    , PipeToken <$ string "|"
    -- effective but not the best idea "semi-colon" insertion, to make
    -- multi-statement parsing trivial at the cost of not really being
    -- able to sensibly implement "partial" parsing for multiline repl
    , StatementToken <$ Mega.eol
    , WordToken <$> word
    ]

--
-- Space or comment to be ignored after tokens.
--
spaceOrComment :: Lexer ()
spaceOrComment =
  Lexer.space (choice [
      void . Mega.try $ string "\\\n"
    , void $ Mega.satisfy (\c -> isSpace c && c /= '\n')
    ]) (Lexer.skipLineComment "#") (Lexer.skipLineComment "#")


--
-- BASELINE EXERCISE 10.
--
-- A word is a non-empty series of hard, soft or un- quoted parts of a
-- word.
--
word :: Lexer Word
word =
  error "TODO: Lexer.word"

--
-- BASELINE EXERCISE 9.
--
-- Hard-quoted part of a word. Some series of characters delimited by '.
--
hard :: Lexer Part
hard =
  error "TODO: Lexer.hard"

--
-- BASELINE EXERCISE 8.
--
-- Soft-quoted part of a word. Some series of variables or text
-- declarations, delimited by ".
--
-- Later this might include sub-expression fragements.
--
soft :: Lexer Part
soft =
  error "TODO: Lexer.soft"

--
-- BASELINE EXERCISE 7.
--
-- Unquoted part of a word. Some non-empty series of unquoted
-- variables or text declarations.
--
-- Later this might include glob fragements and/or sub-expression
-- fragements.
--
unquoted :: Lexer Part
unquoted =
  error "TODO: Lexer.unquoted"

--
-- BASELINE EXERCISE 6.
--
-- A variable expansion.
--
-- One of '$NAME' or ${NAME}, where NAME is some non-empty sequence of
-- valid name characters (defined by varChar).
--
-- Hint: choice and you will need Mega.try to handle both cases
--
variable :: Lexer Fragment
variable =
  error "TODO: Lexer.variable"

--
-- BASELINE EXERCISE 5.
--
-- Unquoted text fragment, any non-empty sequence of bare text that
-- can be contained in an unquoted portion of a word.
--
unquotedText :: Lexer Fragment
unquotedText =
  error "TODO: Lexer.unquotedText"

--
-- BASELINE EXERCISE 4 (Answer provided as example).
--
-- Soft quoted text fragment, any non-empty sequence of bare text that
-- can be contained within soft quoted portion of a word.
--
softText :: Lexer Fragment
softText =
  fmap (TextFragment . T.pack) $
    some softChar

--
-- BASELINE EXERCISE 3.
--
-- 'nameChar' is any character that can appear as the name of a variable.
--
-- Valid characters: A-Z a-z 0-9 _
--
nameChar :: Lexer Char
nameChar =
  error "TODO: Lexer.nameChar"

--
-- BASELINE EXERCISE 2.
--
-- 'softChar' is any character that can appear in an soft quoted with " part of
-- a word.
--
--
-- Valid escapes: \t, \n, \r, \\, \', \" (there others, but out of scope
-- for now: \a, \b, \e, \E, \V, \xHH, \ooo.
--
-- Valid characters: Any character that is not a word separator or an expansion trigger.
--
-- Word separator / quoting / expansion characters: ` $
--
-- Hint: choice
--
softChar :: Lexer Char
softChar =
  error "TODO: Lexer.softChar"

--
-- BASELINE EXERCISE 1 (Answer provided as example).
--
-- 'unquotedChar' is any bare character that can appear in an unquoted
-- part of a word and isn't a part of some variable or expansion.
--
--
-- Valid escapes: \<space> is to be treated as a literal space.
--
-- Valid characters: Any character that is not a word separator or an expansion trigger.
--
-- Word separator / quoting / expansion characters: " ' | & ; ( ) < > <space> \t \n ` $
--  -- note, this would include * etc... if we were implementing globbing
--
-- Hint: choice
--
unquotedChar :: Lexer Char
unquotedChar =
  choice [
      ' ' <$ string "\\ "
    , satisfy (not . flip elem ("\"'|&;()<> \t\n`$":: [Char]))
    ]
