{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall #-}

module Data.Conduit.Aeson
    ( sinkJson, sinkJson'
    , sinkFromJSON, sinkFromJSON'
    , sinkFromJson, sinkFromJson'
    ) where

import Control.Monad.Trans ( lift )
import Data.Aeson ( json, json', Value, FromJSON, fromJSON, Result(..) )
import Data.Conduit ( Sink, MonadThrow, monadThrow )
import Data.Conduit.Attoparsec ( sinkParser )
import Data.ByteString ( ByteString )
import Language.JsonGrammar ( fromJson, Json )

import Control.Exception ( ErrorCall(..) ) -- FIXME

sinkJson ∷ ( MonadThrow m ) ⇒ Sink ByteString m Value
sinkJson = sinkParser json

sinkJson' ∷ ( MonadThrow m ) ⇒ Sink ByteString m Value
sinkJson' = sinkParser json'

-- lift required for conduit 0.4 only
sinkFromJSON ∷ ( MonadThrow m, FromJSON a ) ⇒ Sink ByteString m a
sinkFromJSON = lift . fromResultM . fromJSON =<< sinkJson

sinkFromJSON' ∷ ( MonadThrow m, FromJSON a ) ⇒ Sink ByteString m a
sinkFromJSON' = lift . fromResultM . fromJSON =<< sinkJson'

sinkFromJson ∷ ( MonadThrow m, Json a ) ⇒ Sink ByteString m a
sinkFromJson = lift . fromJustM . fromJson =<< sinkJson

sinkFromJson' ∷ ( MonadThrow m, Json a ) ⇒ Sink ByteString m a
sinkFromJson' = lift . fromJustM . fromJson =<< sinkJson'

fromResultM ∷ ( MonadThrow m ) ⇒ Result a → m a
fromResultM (Success a) = return a
fromResultM (Error   e) = monadThrow (ErrorCall e) -- FIXME

fromJustM ∷ ( MonadThrow m ) ⇒ Maybe a → m a
fromJustM (Just a) = return a
fromJustM Nothing  = monadThrow (ErrorCall "Data.Conduit.Aeson.fromJustM: Nothing")
