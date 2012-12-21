-- vim:set foldenable foldmethod=marker foldcolumn=2:

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}

module GoodData.REST.Types.MD.Obj where

import GoodData.REST.Types.Basic
import Prelude hiding ( id, (.) )
import Util.JSON

type Identifier = STRING
data LINK = -- {{{
    LINK
    { link ∷ URISTRING
    , title ∷ Maybe STRING
    , category ∷ Maybe STRING
    , summary ∷ Maybe STRING
    , tags ∷ Maybe TAGS
    , author ∷ Maybe PROFILEURI
    , created ∷ Maybe DATETIME
    , contributor ∷ Maybe PROFILEURI
    , updated ∷ Maybe DATETIME
    , deprecated ∷ Maybe BOOLEAN
    , projectTemplate ∷ Maybe URISTRING
    , help ∷ Maybe URISTRING
    , identifier ∷ Maybe Identifier
    }
    deriving ( Show, Read )

isoLINK = $(deriveIsos ''LINK)

instance Json LINK where
    grammar = isoLINK . object
        ( ignoreRest
        . prop "link"
        . optionalProp "title"
        . optionalProp "category"
        . optionalProp "summary"
        . optionalProp "tags"
        . optionalProp "author"
        . optionalProp "created"
        . optionalProp "contributor"
        . optionalProp "updated"
        . optionalProp "deprecated"
        . optionalProp "projectTemplate"
        . optionalProp "help"
        . optionalProp "identifier"
        )

-- }}}
