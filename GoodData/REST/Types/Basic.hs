-- vim:set foldenable foldmethod=marker foldcolumn=2:

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}

module GoodData.REST.Types.Basic where

import Data.Aeson ( parseJSON, FromJSON, toJSON, ToJSON, Value )
import Data.Text ( Text )
import Prelude hiding ( id, (.) )
import Util.JSON

newtype BOOLEAN = -- {{{
    BOOLEAN { fromBOOLEAN ∷ Bool }
    deriving ( Show, Read, Eq, Ord, Enum, Bounded )

instance FromJSON BOOLEAN where
    parseJSON x = (BOOLEAN . toEnum) `fmap` parseJSON x

instance ToJSON BOOLEAN where
    toJSON = toJSON . fromEnum . fromBOOLEAN

instance Json BOOLEAN where
    grammar = liftAeson

-- }}}
newtype INT = -- {{{
    INT { fromINT ∷ Int }
    deriving ( Show, Read, Eq, Ord, Enum, Bounded, Num, Integral, Real )

isoINT = $(deriveIsos ''INT)

instance Json INT where
    grammar = isoINT . liftAeson

-- }}}
type DATETIME = STRING -- FIXME
type PROFILEURI = URISTRING
type STRING = Text
type TAGS = STRING
type UNIMPLEMENTED = Value
type URISTRING = String
