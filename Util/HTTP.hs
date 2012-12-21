{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}

module Util.HTTP ( jsonPost, plainGet, httpWithCookies, setMethod, Json ) where

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

jsonPost url value = jsonBody value `fmap` plainGet url

jsonBody ∷ ( Monad m, Json a ) ⇒ a → Request m' → Request m
jsonBody value req = req
        { requestBody = RequestBodyLBS (jsonToLBS value)
        , method = "POST"
        , requestHeaders = (ct, appJson) : (acc, appJson) :
            filter (\(x, _) → x `notElem` [ ct, acc ]) (requestHeaders req)
        }
    where
        ct = "Content-Type"
        acc = "Accept"
        appJson = "application/json"

plainGet url = do
    req ← parseUrl url
    return $ req
        { redirectCount = 0 -- cookies don't work without this
        -- , checkStatus = \_ _ → Nothing -- FIXME: only for printing responses
        }

httpWithCookies cookie_jar req man = do
    now ← liftIO getCurrentTime
    let (req', cookie_jar') = insertCookiesIntoRequest req (evictExpiredCookies cookie_jar now) now
    res ← http req' man
    return $ updateCookieJar res req' now cookie_jar'

setMethod m req = req { method = m }
