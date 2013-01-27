{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall #-}

module GoodData.REST.Session
    ( secureUrl, withGoodData, GoodDataAction
    , jsonPostCall, jsonGetCall, getCall, deleteCall
    , Response(..)
    ) where

import Control.Monad.State ( evalStateT, StateT, get, gets, put )
import Data.Conduit ( ResourceT, ($$), Source )
import Data.Conduit.List ( sinkNull )
import Network.HTTP.Conduit
    ( withManager, Manager, responseBody, Response, def, CookieJar, Request )
import Data.ByteString ( ByteString )

import Data.Conduit.Aeson ( sinkFromJson' )
import GoodData.REST.Types.Account
import GoodData.REST.Types.Basic
import Util.HTTP

data Session = Session { baseUrl ∷ String, manager ∷ Manager, cookies ∷ CookieJar }
type GoodDataAction = StateT Session (ResourceT IO)
type ResponseGDBS = Response (Source GoodDataAction ByteString)

secureUrl ∷ String
secureUrl = "https://secure.gooddata.com"

withGoodData ∷ String → STRING → STRING → GoodDataAction a → IO a
withGoodData url l p act = withManager $ \man → do
    let session = Session { baseUrl = url, manager = man, cookies = def }
    flip evalStateT session $ do
        ul ← accountLogin l p
        accountToken
        ret ← act
        accountLogout ul
        return ret

http' ∷ Request GoodDataAction → GoodDataAction ResponseGDBS
http' req = do
    session ← get
    (cookies', res) ← httpWithCookies (cookies session) req (manager session)
    -- TODO: handle 401 - rerequest temporary token
    put $ session { cookies = cookies' }
    return res

jsonPostCall ∷ ( Json a, Json b ) ⇒ String → a → GoodDataAction (Response b)
jsonPostCall url value = do
    uri ← mkUri url
    res ← http' =<< jsonPost uri value
    retVal ← responseBody res $$ sinkFromJson'
    return $ const retVal `fmap` res

jsonGetCall ∷ ( Json a ) ⇒ String → GoodDataAction (Response a)
jsonGetCall url = do
    uri ← mkUri url
    res ← http' =<< jsonGet uri
    retVal ← responseBody res $$ sinkFromJson'
    return $ const retVal `fmap` res

getCall ∷ String → GoodDataAction ResponseGDBS
getCall uri = http' =<< plainGet =<< mkUri uri

deleteCall ∷ String → GoodDataAction ResponseGDBS
deleteCall uri = http' =<< plainDelete =<< mkUri uri

accountLogin ∷ STRING → STRING → GoodDataAction UserLogin
accountLogin l p = fmap responseBody $
    jsonPostCall "/gdc/account/login" $ PostUserLogin l p (BOOLEAN False)

accountToken ∷ GoodDataAction ()
accountToken = do
    res ← getCall "/gdc/account/token"
    responseBody res $$ sinkNull

accountLogout ∷ UserLogin → GoodDataAction ()
accountLogout ul = do
    res ← deleteCall (state ul)
    responseBody res $$ sinkNull

mkUri ∷ String → GoodDataAction String
mkUri p = (++ p) `fmap` gets baseUrl
