{-# LANGUAGE FlexibleContexts, Rank2Types #-}
module Compat (
  Failure(..)
, Parser
, Applicative
, fail
) where

import Control.Applicative
import Control.Failure
import Prelude hiding (fail)

type Parser a = (Applicative m, Failure String m) => m a

fail :: Failure String m => String -> m v
fail = failure
