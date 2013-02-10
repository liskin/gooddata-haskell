-- vim:set foldenable foldmethod=marker foldcolumn=2:

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}

module GoodData.REST.Types.MD.Obj where

import Data.Data ( Data, Typeable )
import GoodData.REST.Types.Basic
import Prelude hiding ( id, (.) )
import Util.JSON

type Identifier = STRING
data LINK = -- {{{
    LINK
    { linkLink ∷ URISTRING
    , linkTitle ∷ Maybe STRING
    , linkCategory ∷ Maybe STRING
    , linkSummary ∷ Maybe STRING
    , linkTags ∷ Maybe TAGS
    , linkAuthor ∷ Maybe PROFILEURI
    , linkCreated ∷ Maybe DATETIME
    , linkContributor ∷ Maybe PROFILEURI
    , linkUpdated ∷ Maybe DATETIME
    , linkDeprecated ∷ Maybe BOOLEAN
    , linkProjectTemplate ∷ Maybe URISTRING
    , linkHelp ∷ Maybe URISTRING
    , linkIdentifier ∷ Maybe Identifier
    }
    deriving ( Show, Read, Eq, Ord, Data, Typeable )

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
data Meta = -- {{{
    Meta
    { metaTitle ∷ STRING
    , metaCategory ∷ Maybe STRING
    , metaSummary ∷ Maybe STRING
    , metaTags ∷ Maybe TAGS
    , metaAuthor ∷ Maybe PROFILEURI
    , metaCreated ∷ Maybe DATETIME
    , metaContributor ∷ Maybe PROFILEURI
    , metaUpdated ∷ Maybe DATETIME
    , metaDeprecated ∷ Maybe BOOLEAN
    , metaProjectTemplate ∷ Maybe URISTRING
    , metaUri ∷ Maybe URISTRING
    , metaIdentifier ∷ Maybe Identifier
    }
    deriving ( Show, Read, Eq, Ord, Data, Typeable )

isoMeta = $(deriveIsos ''Meta)

instance Json Meta where
    grammar = isoMeta . object
        ( ignoreRest
        . prop "title"
        . optionalProp "category"
        . optionalProp "summary"
        . optionalProp "tags"
        . optionalProp "author"
        . optionalProp "created"
        . optionalProp "contributor"
        . optionalProp "updated"
        . optionalProp "deprecated"
        . optionalProp "projectTemplate"
        . optionalProp "uri"
        . optionalProp "identifier"
        )

-- }}}
data AttributeDisplayForm = -- {{{
    AttributeDisplayForm
    { attrDFMeta ∷ Meta
    , attrDFLinks ∷ Maybe AttributeDisplayFormLinks
    , attrDFContent ∷ AttributeDisplayFormContent
    }
    deriving ( Show, Read, Eq, Ord, Data, Typeable )

isoAttributeDisplayForm = $(deriveIsos ''AttributeDisplayForm)

instance Json AttributeDisplayForm where
    grammar = isoAttributeDisplayForm . object o . tag "attributeDisplayForm"
        where o = ignoreRest . prop "meta" . optionalProp "links" . prop "content"

-- }}}
data AttributeDisplayFormLinks = -- {{{
    AttributeDisplayFormLinks
    { attrDFElements ∷ URISTRING
    }
    deriving ( Show, Read, Eq, Ord, Data, Typeable )

isoAttributeDisplayFormLinks = $(deriveIsos ''AttributeDisplayFormLinks)

instance Json AttributeDisplayFormLinks where
    grammar = isoAttributeDisplayFormLinks . object ( ignoreRest . prop "elements" )

-- }}}
data AttributeDisplayFormContent = -- {{{
    AttributeDisplayFormContent
    { attrDFFormOf ∷ URISTRING
    , attrDFExpression ∷ Maybe STRING
    , attrDFLDMExpression ∷ STRING
    , attrDFType ∷ Maybe STRING
    , attrDFDefault ∷ Maybe INT
    }
    deriving ( Show, Read, Eq, Ord, Data, Typeable )

isoAttributeDisplayFormContent = $(deriveIsos ''AttributeDisplayFormContent)

instance Json AttributeDisplayFormContent where
    grammar = isoAttributeDisplayFormContent . object
        ( ignoreRest
        . prop "formOf"
        . optionalProp "expression"
        . prop "ldmexpression"
        . optionalProp "type"
        . optionalProp "default"
        )

-- }}}
data Attribute = -- {{{
    Attribute
    { attrMeta ∷ Meta
    , attrContent ∷ AttributeContent
    }
    deriving ( Show, Read, Eq, Ord, Data, Typeable )

isoAttribute = $(deriveIsos ''Attribute)

instance Json Attribute where
    grammar = isoAttribute . object o . tag "attribute"
        where o = ignoreRest . prop "meta" . prop "content"

-- }}}
data AttributeContent = -- {{{
    AttributeContent
    { attrPKs, attrFKs ∷ Maybe [ Key ]
    , attrDimension ∷ Maybe URISTRING
    , attrDisplayForms ∷ [ AttributeDisplayForm ]
    , attrType ∷ Maybe STRING
    -- some fields omitted, they seem unnecessary
    }
    deriving ( Show, Read, Eq, Ord, Data, Typeable )

isoAttributeContent = $(deriveIsos ''AttributeContent)

instance Json AttributeContent where
    grammar = isoAttributeContent . object
        ( ignoreRest
        . optionalProp "pk"
        . optionalProp "fk"
        . optionalProp "dimension"
        . propBy (list (grammar . untag "attributeDisplayForm")) "displayForms"
        . optionalProp "type"
        )

-- }}}
data Key = -- {{{
    Key (URI Column)
    deriving ( Show, Read, Eq, Ord, Data, Typeable )

isoKey = $(deriveIsos ''Key)

instance Json Key where
    grammar = isoKey . object o
        where o = prop "data" . fixedProp "type" ("col" ∷ STRING)

-- }}}
data Column = -- {{{
    Column
    { colMeta ∷ Meta
    , colContent ∷ ColumnContent
    }
    deriving ( Show, Read, Eq, Ord, Data, Typeable )

isoColumn = $(deriveIsos ''Column)

instance Json Column where
    grammar = isoColumn . object o . tag "column"
        where o = ignoreRest . prop "meta" . prop "content"

-- }}}
data ColumnContent = -- {{{
    ColumnContent
    { colTable ∷ URISTRING
    , colDBName ∷ STRING
    , colTableSortOrder ∷ INT
    -- some fields omitted, they seem unused
    }
    deriving ( Show, Read, Eq, Ord, Data, Typeable )

isoColumnContent = $(deriveIsos ''ColumnContent)

instance Json ColumnContent where
    grammar = isoColumnContent . object
        ( ignoreRest
        . prop "table"
        . prop "columnDBName"
        . prop "tableSortOrder"
        )

-- }}}
