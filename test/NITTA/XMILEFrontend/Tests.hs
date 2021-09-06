{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : NITTA.LuaFrontend.Tests
Description :
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.XMILEFrontend.Tests (
    tests,
) where

import qualified Data.Text as T
import NITTA.XMILEFrontend
import Test.Tasty (TestTree)
import Test.Tasty.HUnit
import Test.Tasty.TH
import Xeno.DOM
import Xeno.DOM.Internal

case_XMILE_parse = undefined

tests :: TestTree
tests = $(testGroupGenerator)
