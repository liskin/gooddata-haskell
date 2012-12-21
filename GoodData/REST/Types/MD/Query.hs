-- vim:set foldenable foldmethod=marker foldcolumn=2:

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}

module GoodData.REST.Types.MD.Query where

import GoodData.REST.Types.Basic
import GoodData.REST.Types.MD.Obj
import Prelude hiding ( id, (.) )
import Util.JSON

data Query = -- {{{
    Query { entries ∷ [ LINK ], meta ∷ QueryMeta }
    deriving ( Show, Read )

isoQuery = $(deriveIsos ''Query)

instance Json Query where
    grammar = tagged "query" $ isoQuery . object
        ( ignoreRest . prop "entries" . prop "meta" )

-- }}}
data QueryMeta = -- {{{
    QueryMeta { summary, title, category ∷ STRING }
    deriving ( Show, Read )

isoQueryMeta = $(deriveIsos ''QueryMeta)

instance Json QueryMeta where
    grammar = isoQueryMeta . object
        ( ignoreRest . prop "summary" . prop "title" . prop "category" )

-- }}}
