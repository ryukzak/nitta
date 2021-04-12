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

case_find_startup_function =
    let
        src =
            [__i|
                function sum(a)
                    local t = 2
                    local r = -1
                    sum(a + t + r)
                end
                sum(1)
            |]
        (actualName, FunCall (NormalFunCall _ (Args actualArgValue)), FunAssign _ (FunBody actualArg _ _)) = findStartupFunction (getLuaBlockFromSources src)
        expectedValues = (pack "sum", [Name $ pack "a"], [Number IntNum $ pack "1"])
     in expectedValues @?= (actualName, actualArg, actualArgValue)

case_process_local_assignment_statement =
    let assignment = LocalAssign [Name $ pack "a"] (Just [Number IntNum (pack "2")])
        result = [Constant { cName = pack "a", cValueString = pack "2", cValueType = IntNum }]
    in result @?= execState (processStatement (pack "_") assignment) []

case_process_assignment_statement =
    let assignment = Assign [VarName (Name $ pack "a")] [Number IntNum (pack "2")]
        result = [Constant { cName = pack "a", cValueString = pack "2", cValueType = IntNum }]
    in result @?= execState (processStatement (pack "_") assignment) []

case_process_multiple_assignments_statement =
    let assignment = Assign [VarName (Name $ pack "a"), VarName (Name $ pack "b")] [Number IntNum $ pack "2", Number FloatNum $ pack "2.5"]
        result = [Constant { cName = pack "a", cValueString = pack "2", cValueType = IntNum}, Constant { cName = pack "b", cValueString = pack "2.5", cValueType = FloatNum}]
    in result @?= execState (processStatement (pack "_") assignment) []

tests :: TestTree
tests = $(testGroupGenerator)