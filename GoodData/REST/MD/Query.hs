{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures #-}

module GoodData.REST.MD.Query where

import GoodData.Session
import GoodData.REST.Types.MD.Query

query ∷ String → String → GoodDataAction Query
query project typ = fmap responseBody $ jsonGetCall $ "/gdc/md/" ++ project ++ "/query/" ++ typ
