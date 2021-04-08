{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module NITTA.LuaFrontend.TestsNew (
    tests,
) where


import Data.String.Interpolate
import NITTA.LuaFrontendNew 
import Test.Tasty.HUnit
import Test.Tasty.TH
import Test.Tasty
import Data.Text

findStartupFunkName =
    let src =
            [__i|
                function sum(a)
                    local t = 2
                    local r = -1
                    sum(a + t + r)
                end
                sum(0)
            |]
        (actualName, _, _) = findStartupFunction (getLuaBlockFromSources src)
        expectedName = pack "sum"
     in expectedName @?= actualName


tests :: TestTree
tests = $(testGroupGenerator)