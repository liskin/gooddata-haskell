{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}

module GoodData.REST.Types where

import Prelude hiding ( id, (.) )
import Control.Category

import Data.Aeson ( parseJSON, FromJSON, toJSON, ToJSON, Value, Object )
import Data.Text ( Text )
import Data.Iso hiding ( Endo )
import Language.JsonGrammar

type Endo a = a → a

tagged ∷ String → Endo (Iso (Value :- t) (a :- t))
tagged tag g = object (propBy g tag)

optionalProp ∷ ( Json a ) ⇒ String → Iso (Object :- t) (Object :- Maybe a :- t)
optionalProp name = duck just . prop name <> duck nothing

-- BOOLEAN --
newtype BOOLEAN = BOOLEAN { fromBOOLEAN ∷ Bool }
    deriving ( Show, Read )

instance FromJSON BOOLEAN where
    parseJSON x = (BOOLEAN . toEnum) `fmap` parseJSON x

instance ToJSON BOOLEAN where
    toJSON = toJSON . fromEnum . fromBOOLEAN

instance Json BOOLEAN where
    grammar = liftAeson

-- STRING, URISTRING --
type STRING = Text
type URISTRING = String

-- PostUserLogin --
data PostUserLogin = PostUserLogin { login, password ∷ STRING, remember ∷ BOOLEAN }
    deriving ( Show, Read )

postUserLogin = $(deriveIsos ''PostUserLogin)

instance Json PostUserLogin where
    grammar = tagged "postUserLogin" $ postUserLogin . object (ignoreRest . prop "login" . prop "password" . prop "remember")

-- UserLogin --
data UserLogin = UserLogin { state, profile ∷ URISTRING, token ∷ Maybe STRING }
    deriving ( Show, Read )

userLogin = $(deriveIsos ''UserLogin)

instance Json UserLogin where
    grammar = tagged "userLogin" $ userLogin . object (ignoreRest . prop "state" . prop "profile" . optionalProp "token")
