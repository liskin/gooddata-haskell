{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall #-}

module GoodData.Expand ( expand, allURIs, emptyToAnyURIs ) where

import Data.Data ( Data, Typeable, cast, typeOf )
import Data.Generics.Aliases ( ext1T, ext1Q )
import Data.Generics.Schemes ( everywhere, everything )
import GoodData.REST.Types.Basic ( URI(..) )
import GoodData.Session ( jsonGetCall, responseBody, GoodDataAction )
import qualified Data.Map as M

data AnyURI = forall a. AnyURI (URI a)

deriving instance Show AnyURI

instance Eq AnyURI where
    AnyURI a@(URI {}) == AnyURI b@(URI {}) = Just a == cast b

instance Ord AnyURI where
    AnyURI a@(URI {}) `compare` AnyURI b@(URI {}) =
        case typeOf a `compare` typeOf b of
            EQ → let Just b' = cast b in a `compare` b'
            x  → x

type GetExpands = forall a. ( Data a ) ⇒ a → [ AnyURI ]

gexpand ∷ GetExpands → [ AnyURI ] → GoodDataAction [ AnyURI ]
gexpand expands uris = do
        dict ← go M.empty uris
        let dict' = M.map (substAnyURIContent dict') dict
        return [ dict' M.! (emptyAnyURI uri) | uri ← uris ]
    where
        go dict [] = return dict
        go dict (x : xs)
            | emptyAnyURI x `M.member` dict = go dict xs
            | otherwise = do
                x'@(AnyURI (URI _ (Just c))) ← fillAnyURI x
                go (M.insert (emptyAnyURI x) x' dict) (expands c ++ xs)

expand ∷ ( Typeable a ) ⇒ GetExpands → [ URI a ] → GoodDataAction [ URI a ]
expand expands = fmap (map fromAnyURI) . gexpand expands . map anyURI

substAnyURIContent ∷ M.Map AnyURI AnyURI → AnyURI → AnyURI
substAnyURIContent dict (AnyURI u) = AnyURI (substURIContent dict u)

substURIContent ∷ M.Map AnyURI AnyURI → URI a → URI a
substURIContent dict (URI u (Just c)) =
    URI u (Just (everywhere (id `ext1T` substURI dict) c))
substURIContent _ _ = error "substURIContent: empty URI"

substURI ∷ ( Typeable a ) ⇒ M.Map AnyURI AnyURI → URI a → URI a
substURI dict u =
    case anyURI u `M.lookup` dict of
        Just u' → fromAnyURI u'
        Nothing → u

fillAnyURI ∷ AnyURI → GoodDataAction AnyURI
fillAnyURI (AnyURI u) = AnyURI `fmap` fillURI u

fillURI ∷ URI a → GoodDataAction (URI a)
fillURI (URI url Nothing) = do
    c ← responseBody `fmap` jsonGetCall url
    return $ URI url (Just c)
fillURI u = return u

emptyAnyURI ∷ AnyURI → AnyURI
emptyAnyURI (AnyURI u) = AnyURI (emptyURI u)

emptyURI ∷ URI a → URI a
emptyURI (URI u _) = URI u Nothing

allURIs ∷ GetExpands
allURIs = everything (++) (const [] `ext1Q` emptyToAnyURIs)

anyURI ∷ URI a → AnyURI
anyURI u@(URI {}) = AnyURI u

fromAnyURI ∷ (Typeable a) ⇒ AnyURI → URI a
fromAnyURI (AnyURI u@(URI {})) = let Just u' = cast u in u'

emptyToAnyURIs ∷ URI a → [ AnyURI ]
emptyToAnyURIs u = [ anyURI u | isEmptyURI u ]

-- isEmptyAnyURI ∷ AnyURI → Bool
-- isEmptyAnyURI (AnyURI u) = isEmptyURI u

isEmptyURI ∷ URI a → Bool
isEmptyURI (URI _ content) = content == Nothing
