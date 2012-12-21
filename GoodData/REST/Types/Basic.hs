-- vim:set foldenable foldmethod=marker foldcolumn=2:

{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall #-}

module GoodData.REST.Types.Basic where

import Data.Aeson ( parseJSON, FromJSON, toJSON, ToJSON )
import Data.Text ( Text )
import Prelude hiding ( id, (.) )
import Util.JSON

newtype BOOLEAN = -- {{{
    BOOLEAN { fromBOOLEAN ∷ Bool }
    deriving ( Show, Read )

instance FromJSON BOOLEAN where
    parseJSON x = (BOOLEAN . toEnum) `fmap` parseJSON x

instance ToJSON BOOLEAN where
    toJSON = toJSON . fromEnum . fromBOOLEAN

instance Json BOOLEAN where
    grammar = liftAeson

-- }}}
type STRING = Text
type URISTRING = String
