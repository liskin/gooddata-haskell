-- vim:set foldenable foldmethod=marker foldcolumn=2:

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}

module GoodData.REST.Types.Account where

import Data.Data ( Data, Typeable )
import GoodData.REST.Types.Basic
import Prelude hiding ( id, (.) )
import Util.JSON

-- login {{{

data PostUserLogin = -- {{{
    PostUserLogin { login, password ∷ STRING, remember ∷ BOOLEAN }
    deriving ( Show, Read, Eq, Ord, Data, Typeable )

isoPostUserLogin = $(deriveIsos ''PostUserLogin)

instance Json PostUserLogin where
    grammar = isoPostUserLogin . object o . tag "postUserLogin"
        where o = ignoreRest . prop "login" . prop "password" . prop "remember"

---- }}}
data UserLogin = -- {{{
    UserLogin { state, profile ∷ URISTRING, token ∷ Maybe STRING }
    deriving ( Show, Read, Eq, Ord, Data, Typeable )

isoUserLogin = $(deriveIsos ''UserLogin)

instance Json UserLogin where
    grammar = isoUserLogin . object o . tag "userLogin"
        where o = ignoreRest . prop "state" . prop "profile" . optionalProp "token"

---- }}}

-- }}}
