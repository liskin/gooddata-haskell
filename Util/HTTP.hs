{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}

module Util.HTTP
    ( jsonPost, jsonGet
    , plainGet, plainDelete
    , httpWithCookies, Json
    ) where

import Control.Monad.IO.Class ( liftIO )
import Data.Aeson ( encode )
import Data.Maybe ( fromJust )
import Data.Time.Clock ( getCurrentTime )
import Debug.Trace.LocationTH ( check )
import Language.JsonGrammar
import Network.HTTP.Conduit
import qualified Data.ByteString.Lazy as L

jsonToLBS ∷ ( Json a ) ⇒ a → L.ByteString
jsonToLBS = encode . $check fromJust . toJson

jsonPost url value = jsonAccept `fmap` jsonBody value `fmap` plainGet url
jsonGet url = jsonAccept `fmap` plainGet url

jsonAccept ∷ ( Monad m ) ⇒ Request m → Request m
jsonAccept req = req { requestHeaders = rh }
    where
        acc = "Accept"
        rh = (acc, appJson) : filter (\(x, _) → x /= acc) (requestHeaders req)

jsonBody ∷ ( Monad m, Json a ) ⇒ a → Request m' → Request m
jsonBody value req = req
        { requestBody = RequestBodyLBS (jsonToLBS value)
        , method = "POST"
        , requestHeaders = rh
        }
    where
        ct = "Content-Type"
        rh = (ct, appJson) : filter (\(x, _) → x /= ct) (requestHeaders req)

appJson = "application/json"

plainGet url = do
    req ← parseUrl url
    return $ req
        { redirectCount = 0 -- cookies don't work without this
        -- , checkStatus = \_ _ → Nothing -- FIXME: only for printing responses
        }

plainDelete url = setMethod "DELETE" `fmap` plainGet url

setMethod m req = req { method = m }

httpWithCookies cookie_jar req man = do
    now ← liftIO getCurrentTime
    let (req', cookie_jar') = insertCookiesIntoRequest req (evictExpiredCookies cookie_jar now) now
    res ← http req' man
    return $ updateCookieJar res req' now cookie_jar'
