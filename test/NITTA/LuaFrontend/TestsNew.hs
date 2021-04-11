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
import Language.Lua
import Control.Monad.State

findStartupFunctionTest =
    let
        src = 
            [__i|
                function sum(a)
                    local t = 2
                    local r = -1
                    sum(a + t + r)
                end
                sum(0)
            |]
        (actualName, FunCall (NormalFunCall _ (Args actualArgValue)), FunAssign _ (FunBody actualArg _ _)) = findStartupFunction (getLuaBlockFromSources src)
        expectedValues = (pack "sum", [Name $ pack "a"], [Number IntNum $ pack "1"])
     in expectedValues @?= (actualName, actualArg, actualArgValue)

processStatementSimpleAssignmentTest = 
    let assignment = Assign [VarName (Name $ pack "a")] [Number IntNum (pack "2")]
        result = [Constant { cName = pack "a", cValueString = pack "2"}]
    in result @?= snd (runState (processStatement (pack "_") assignment) [])

tests :: TestTree
tests = $(testGroupGenerator)