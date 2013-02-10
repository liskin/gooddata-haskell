{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall #-}

module Util.JSON
    ( module Control.Category
    , module Language.JsonGrammar
    , module Data.Iso
    , tag
    , optionalProp
    ) where

import Data.Aeson ( Value, Object )
import Prelude hiding ( id, (.) )
import Control.Category

import Data.Iso hiding ( Endo, option )
import Language.JsonGrammar

tag ∷ String → Iso (Value :- t) (Value :- t)
tag t = object (prop t)

optionalProp ∷ ( Json a ) ⇒ String → Iso (Object :- t) (Object :- Maybe a :- t)
optionalProp name = duck just . prop name <> duck nothing
