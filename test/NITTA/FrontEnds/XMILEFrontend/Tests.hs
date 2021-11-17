{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

{- |
Module      : NITTA.FrontEnds.XMILEFrontend.Tests
Description :
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.FrontEnds.XMILEFrontend.Tests (
    tests,
) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit
import Test.Tasty.TH

case_XMILE_parse = undefined

tests :: TestTree
tests = $(testGroupGenerator)
