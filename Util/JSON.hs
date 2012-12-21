{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall #-}

module Util.JSON
    ( module Control.Category
    , module Language.JsonGrammar
    , module Data.Iso
    , tagged
    , optionalProp
    ) where

import Data.Aeson ( Value, Object )
import Prelude hiding ( id, (.) )
import Control.Category

import Data.Iso hiding ( Endo, option )
import Language.JsonGrammar

tagged ∷ String → Iso (Value :- t) (a :- t) → Iso (Value :- t) (a :- t)
tagged tag g = object (propBy g tag)

optionalProp ∷ ( Json a ) ⇒ String → Iso (Object :- t) (Object :- Maybe a :- t)
optionalProp name = duck just . prop name <> duck nothing
