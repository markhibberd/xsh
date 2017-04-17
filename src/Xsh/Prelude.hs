{-# LANGUAGE NoImplicitPrelude #-}
module Xsh.Prelude (
    module X
  ) where

import           Control.Applicative as X
import           Control.Monad as X
import           Data.Bool as X (Bool (..), (||), (&&), not)
import           Data.Char as X (Char)
import           Data.Bifunctor as X (Bifunctor (..))
import           Data.Either as X (Either (..), either)
import           Data.Foldable as X
import           Data.Function as X ((.), ($), flip, id, const)
import           Data.Functor as X (($>))
import           Data.Int as X
import           Data.Maybe as X (Maybe (..), maybe, fromMaybe)
import           Data.Monoid as X (Monoid (..), (<>))
import           Data.Traversable as X
import           Prelude as X (Eq (..), Show (..), Ord (..), Num (..), Enum, Bounded (..), Integral (..), error)
