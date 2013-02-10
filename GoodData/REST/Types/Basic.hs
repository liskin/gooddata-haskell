-- vim:set foldenable foldmethod=marker foldcolumn=2:

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}

module GoodData.REST.Types.Basic where

import Data.Aeson ( parseJSON, FromJSON, toJSON, ToJSON, Value )
import Data.Data ( Data(..), Typeable, Typeable1, mkNoRepType, gcast1 )
import Data.Text ( Text )
import Prelude hiding ( id, (.) )
import Util.JSON

newtype BOOLEAN = -- {{{
    BOOLEAN { fromBOOLEAN ∷ Bool }
    deriving ( Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable )

instance FromJSON BOOLEAN where
    parseJSON x = (BOOLEAN . toEnum) `fmap` parseJSON x

instance ToJSON BOOLEAN where
    toJSON = toJSON . fromEnum . fromBOOLEAN

instance Json BOOLEAN where
    grammar = liftAeson

-- }}}
newtype INT = -- {{{
    INT { fromINT ∷ Int }
    deriving ( Show, Read, Eq, Ord, Enum, Bounded, Num, Integral, Real, Data, Typeable )

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

data URI a = -- {{{
    URIType a ⇒ URI URISTRING (Maybe a)

type URIType a = ( Json a, Typeable a, Data a, Eq a, Ord a, Show a, Read a )

deriving instance Show (URI a)
deriving instance URIType a ⇒ Read (URI a)
deriving instance Eq (URI a)
deriving instance Ord (URI a)
deriving instance Typeable1 URI

instance ( Typeable a, Data a ) ⇒ Data (URI a) where
    toConstr _   = error "toConstr"
    gunfold _ _  = error "gunfold"
    dataTypeOf _ = mkNoRepType "GoodData.REST.Types.Basic.URI"
    dataCast1  x = gcast1 x

isoURI = Iso f g
    where
        f (x :- t) = Just (URI x Nothing :- t)
        g (URI x _ :- t) = Just (x :- t)

instance URIType a ⇒ Json (URI a) where
    grammar = isoURI . grammar

-- }}}
