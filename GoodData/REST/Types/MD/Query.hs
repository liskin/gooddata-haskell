-- vim:set foldenable foldmethod=marker foldcolumn=2:

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}

module GoodData.REST.Types.MD.Query where

import Data.Data ( Data, Typeable )
import GoodData.REST.Types.Basic
import GoodData.REST.Types.MD.Obj
import Prelude hiding ( id, (.) )
import Util.JSON

data Query = -- {{{
    Query { entries ∷ [ LINK ], meta ∷ QueryMeta }
    deriving ( Show, Read, Eq, Ord, Data, Typeable )

isoQuery = $(deriveIsos ''Query)

instance Json Query where
    grammar = isoQuery . object o . tag "query"
        where o = ignoreRest . prop "entries" . prop "meta"

-- }}}
data QueryMeta = -- {{{
    QueryMeta { summary, title, category ∷ STRING }
    deriving ( Show, Read, Eq, Ord, Data, Typeable )

isoQueryMeta = $(deriveIsos ''QueryMeta)

instance Json QueryMeta where
    grammar = isoQueryMeta . object o
        where o = ignoreRest . prop "summary" . prop "title" . prop "category"

-- }}}
