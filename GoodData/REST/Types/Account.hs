-- vim:set foldenable foldmethod=marker foldcolumn=2:

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}

module GoodData.REST.Types.Account where

import GoodData.REST.Types.Basic
import Prelude hiding ( id, (.) )
import Util.JSON

-- login {{{

data PostUserLogin = -- {{{
    PostUserLogin { login, password ∷ STRING, remember ∷ BOOLEAN }
    deriving ( Show, Read )

postUserLogin = $(deriveIsos ''PostUserLogin)

instance Json PostUserLogin where
    grammar = tagged "postUserLogin" $ postUserLogin . object (ignoreRest . prop "login" . prop "password" . prop "remember")

---- }}}
data UserLogin = -- {{{
    UserLogin { state, profile ∷ URISTRING, token ∷ Maybe STRING }
    deriving ( Show, Read )

userLogin = $(deriveIsos ''UserLogin)

instance Json UserLogin where
    grammar = tagged "userLogin" $ userLogin . object (ignoreRest . prop "state" . prop "profile" . optionalProp "token")

---- }}}

-- }}}
