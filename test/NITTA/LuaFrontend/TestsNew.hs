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

import Control.Monad.State
import qualified Data.HashMap.Strict as Map
import Data.String.Interpolate
import qualified Data.Text as T
import Language.Lua
import qualified NITTA.Intermediate.Functions as F
import NITTA.Intermediate.Types
import NITTA.LuaFrontendNew
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

case_find_startup_function =
    let src =
            [__i|
                function sum(a)
                    local t = 2
                    local r = -1
                    sum(a + t + r)
                end
                sum(1)
            |]
        (actualName, FunCall (NormalFunCall _ (Args actualArgValue)), FunAssign _ (FunBody actualArg _ _)) = findStartupFunction (getLuaBlockFromSources src)
        expectedValues = (T.pack "sum", [Name $ T.pack "a"], [Number IntNum $ T.pack "1"])
     in (actualName, actualArg, actualArgValue) @?= expectedValues

case_process_local_assignment_statement =
    let assignment = LocalAssign [Name $ T.pack "a"] (Just [Number IntNum (T.pack "2")])
        expected = Map.fromList [(T.pack "a", LuaValueVersion{luaValueVersionName = T.pack "a", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = False})]
        (_str :: String, AlgBuilder{algBuffer}) = runState (processStatement (T.pack "_") assignment) defaultAlgBuilder
     in algBuffer @?= expected

case_process_assignment_statement =
    let assignment = Assign [VarName (Name $ T.pack "a")] [Number IntNum (T.pack "2")]
        expected = Map.fromList [(T.pack "a", LuaValueVersion{luaValueVersionName = T.pack "a", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = False})]
        (_str :: String, AlgBuilder{algBuffer}) = runState (processStatement (T.pack "_") assignment) defaultAlgBuilder
     in algBuffer @?= expected

case_process_multiple_assignments_statement =
    let assignment = Assign [VarName (Name $ T.pack "a"), VarName (Name $ T.pack "b")] [Number IntNum $ T.pack "2", Number FloatNum $ T.pack "2.5"]
        expected =
            Map.fromList
                [ (T.pack "a", LuaValueVersion{luaValueVersionName = T.pack "a", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = False})
                , (T.pack "b", LuaValueVersion{luaValueVersionName = T.pack "b", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = False})
                ]
        (_str :: String, AlgBuilder{algBuffer}) = runState (processStatement (T.pack "_") assignment) defaultAlgBuilder
     in algBuffer @?= expected

case_process_add_statement =
    let assignment = Assign [VarName (Name (T.pack "a"))] [Binop Add (Number IntNum (T.pack "1")) (Number IntNum (T.pack "2"))]
        expected =
            [ Func{fIn = [T.pack "!1#0", T.pack "!2#0"], fOut = [LuaValueVersion{luaValueVersionName = T.pack "a", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = False}], fValues = [], fName = "add", fInt = []}
            , Func{fIn = [], fOut = [LuaValueVersion{luaValueVersionName = T.pack "2", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = True}], fValues = [2], fName = "constant", fInt = []}
            , Func{fIn = [], fOut = [LuaValueVersion{luaValueVersionName = T.pack "1", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = True}], fValues = [1], fName = "constant", fInt = []}
            ]
        (_str :: String, AlgBuilder{algGraph}) = runState (processStatement (T.pack "_") assignment) defaultAlgBuilder
     in algGraph @?= expected

case_process_sub_statement =
    let assignment = Assign [VarName (Name (T.pack "a"))] [Binop Sub (Number IntNum (T.pack "1")) (Number IntNum (T.pack "2"))]
        expected =
            [ Func{fIn = [T.pack "!1#0", T.pack "!2#0"], fOut = [LuaValueVersion{luaValueVersionName = T.pack "a", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = False}], fValues = [], fName = "sub", fInt = []}
            , Func{fIn = [], fOut = [LuaValueVersion{luaValueVersionName = T.pack "2", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = True}], fValues = [2], fName = "constant", fInt = []}
            , Func{fIn = [], fOut = [LuaValueVersion{luaValueVersionName = T.pack "1", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = True}], fValues = [1], fName = "constant", fInt = []}
            ]
        (_str :: String, AlgBuilder{algGraph}) = runState (processStatement (T.pack "_") assignment) defaultAlgBuilder
     in algGraph @?= expected

case_process_divide_statement =
    let assignment = Assign [VarName (Name (T.pack "a"))] [Binop Div (Number IntNum (T.pack "1")) (Number IntNum (T.pack "2"))]
        expected =
            [ Func{fIn = [T.pack "!1#0", T.pack "!2#0"], fOut = [LuaValueVersion{luaValueVersionName = T.pack "a", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = False}], fValues = [], fName = "divide", fInt = []}
            , Func{fIn = [], fOut = [LuaValueVersion{luaValueVersionName = T.pack "2", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = True}], fValues = [2], fName = "constant", fInt = []}
            , Func{fIn = [], fOut = [LuaValueVersion{luaValueVersionName = T.pack "1", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = True}], fValues = [1], fName = "constant", fInt = []}
            ]
        (_str :: String, AlgBuilder{algGraph}) = runState (processStatement (T.pack "_") assignment) defaultAlgBuilder
     in algGraph @?= expected

case_process_multiply_statement =
    let assignment = Assign [VarName (Name (T.pack "a"))] [Binop Mul (Number IntNum (T.pack "1")) (Number IntNum (T.pack "2"))]
        expected =
            [ Func{fIn = [T.pack "!1#0", T.pack "!2#0"], fOut = [LuaValueVersion{luaValueVersionName = T.pack "a", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = False}], fValues = [], fName = "multiply", fInt = []}
            , Func{fIn = [], fOut = [LuaValueVersion{luaValueVersionName = T.pack "2", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = True}], fValues = [2], fName = "constant", fInt = []}
            , Func{fIn = [], fOut = [LuaValueVersion{luaValueVersionName = T.pack "1", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = True}], fValues = [1], fName = "constant", fInt = []}
            ]
        (_str :: String, AlgBuilder{algGraph}) = runState (processStatement (T.pack "_") assignment) defaultAlgBuilder
     in algGraph @?= expected

--case_process_neg_statement =
--   let assignment = Assign [VarName (Name (T.pack "x"))] [Unop Neg (PrefixExp (PEVar (VarName (Name (T.pack "y")))))]
--       expected =
--               [ Func{fIn = [T.pack "y"], fOut = [T.pack "a"], fValues = [], fName = "neg", fInt = []}]
--       (_str :: String, AlgBuilder{algGraph}) = runState (processStatement (T.pack "_") assignment) defaultAlgBuilder
--    in algGraph @?= expected
--
--case_process_funcall_statement =
--   let assignment = FunCall (NormalFunCall (PEVar (VarName (Name (T.pack "send")))) (Args [PrefixExp (PEVar (VarName (Name (T.pack "x"))))]))
--       expected =
--               [ Func{fIn = [T.pack "a"], fOut = [T.pack "a"], fValues = [], fName = "constant", fInt = []}]
--       (_str :: String, AlgBuilder{algGraph}) = runState (processStatement (T.pack "_") assignment) defaultAlgBuilder
--    in algGraph @?= expected

case_temporary_variable =
    let assignment = Assign [VarName (Name $ T.pack "a")] [Binop Add (Binop Add (Number IntNum $ T.pack "1") (Number IntNum $ T.pack "2")) (Number IntNum $ T.pack "3")]
        expected =
            [ Func{fIn = [T.pack "_0#a", T.pack "!3#0"], fOut = [LuaValueVersion{luaValueVersionName = T.pack "a", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = False}], fValues = [], fName = "add", fInt = []}
            , Func{fIn = [], fOut = [LuaValueVersion{luaValueVersionName = T.pack "3", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = True}], fValues = [3], fName = "constant", fInt = []}
            , Func{fIn = [T.pack "!1#0", T.pack "!2#0"], fOut = [LuaValueVersion{luaValueVersionName = T.pack "_a&", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = False}], fValues = [], fName = "add", fInt = []}
            , Func{fIn = [], fOut = [LuaValueVersion{luaValueVersionName = T.pack "2", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = True}], fValues = [2], fName = "constant", fInt = []}
            , Func{fIn = [], fOut = [LuaValueVersion{luaValueVersionName = T.pack "1", luaValueVersionAssignCount = 0, luaValueVersionIsConstant = True}], fValues = [1], fName = "constant", fInt = []}
            ]
        (_str :: String, AlgBuilder{algGraph}) = runState (processStatement (T.pack "_") assignment) defaultAlgBuilder
     in algGraph @?= expected

case_lua_constant_declatation =
    let src =
            [__i|
                function sum(a)
                    local t = 2
                    local r = -1
                    sum(a + t + r)
                end
                sum(0)
            |]
        dfg =
            [ F.constant 2 [T.pack "t^0#0"] :: F T.Text Int
            , F.constant (-1) [T.pack "r^0#0"]
            , F.add (T.pack "a^0#0") (T.pack "t^0#0") [T.pack "_1#loop"]
            , F.add (T.pack "_1#loop") (T.pack "r^0#0") [T.pack "_0#loop"]
            , F.loop 0 (T.pack "_0#loop") [T.pack "a^0#0"]
            ]
     in functions (parseLuaSources src) @?= dfg

case_lua_two_name_for_same_constant =
    let src =
            [__i|
                function sum(a)
                    local t = 1
                    local r = 1
                    sum(a + t + r)
                end
                sum(0)
            |]
        dfg =
            [ F.constant 1 [T.pack "t^0#0"] :: F T.Text Int
            , F.constant 1 [T.pack "r^0#0"]
            , F.add (T.pack "a^0#0") (T.pack "t^0#0") [T.pack "_1#loop"]
            , F.add (T.pack "_1#loop") (T.pack "r^0#0") [T.pack "_0#loop"]
            , F.loop 0 (T.pack "_0#loop") [T.pack "a^0#0"]
            ]
     in functions (parseLuaSources src) @?= dfg

case_lua_negative_operator =
    let src =
            [__i|
                function sum(a)
                    b = -a
                    sum(b)
                end
                sum(0)
            |]
        dfg =
            [ F.neg (T.pack "a^0#0") [T.pack "b^0#0"] :: F T.Text Int
            , F.loop 0 (T.pack "b^0#0") [T.pack "a^0#0"]
            ]
     in functions (parseLuaSources src) @?= dfg

defaultAlgBuilder = AlgBuilder{algGraph = [], algBuffer = Map.empty, algVarGen = Map.empty, algVars = Map.empty, algStartupArgs = Map.empty} :: AlgBuilder String Int

tests :: TestTree
tests = $(testGroupGenerator)
