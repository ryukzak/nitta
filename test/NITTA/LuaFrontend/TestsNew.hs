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
import qualified Data.Map as Map
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
        expected = Map.fromList [(T.pack "a", Variable{luaValueName = T.pack "a", luaValueAssignCount = 0, luaValueAccessCount = 0, isStartupArgument = False, startupArgumentString = T.pack ""})]
        (_str :: String, AlgBuilder{algBuffer}) = runState (processStatement (T.pack "_") assignment) defaultAlgBuilder
     in algBuffer @?= expected

case_process_assignment_statement =
    let assignment = Assign [VarName (Name $ T.pack "a")] [Number IntNum (T.pack "2")]
        expected = Map.fromList [(T.pack "a", Variable{luaValueName = T.pack "a", luaValueAssignCount = 0, luaValueAccessCount = 0, isStartupArgument = False, startupArgumentString = T.pack ""})]
        (_str :: String, AlgBuilder{algBuffer}) = runState (processStatement (T.pack "_") assignment) defaultAlgBuilder
     in algBuffer @?= expected

case_process_multiple_assignments_statement =
    let assignment = Assign [VarName (Name $ T.pack "a"), VarName (Name $ T.pack "b")] [Number IntNum $ T.pack "2", Number FloatNum $ T.pack "2.5"]
        expected =
            Map.fromList
                [ (T.pack "a", Variable{luaValueName = T.pack "a", luaValueAssignCount = 0, luaValueAccessCount = 0, isStartupArgument = False, startupArgumentString = T.pack ""})
                , (T.pack "b", Variable{luaValueName = T.pack "b", luaValueAssignCount = 0, luaValueAccessCount = 0, isStartupArgument = False, startupArgumentString = T.pack ""})
                ]
        (_str :: String, AlgBuilder{algBuffer}) = runState (processStatement (T.pack "_") assignment) defaultAlgBuilder
     in algBuffer @?= expected

case_process_add_statement =
    let assignment = Assign [VarName (Name (T.pack "a"))] [Binop Add (Number IntNum (T.pack "1")) (Number IntNum (T.pack "2"))]
        expected =
            [ Func{fIn = [T.pack "!1#0", T.pack "!2#0"], fOut = [T.pack "a"], fValues = [], fName = "add", fInt = []}
            , Func{fIn = [], fOut = [T.pack "2"], fValues = [2], fName = "constant", fInt = []}
            , Func{fIn = [], fOut = [T.pack "1"], fValues = [1], fName = "constant", fInt = []}
            ]
        (_str :: String, AlgBuilder{algGraph}) = runState (processStatement (T.pack "_") assignment) defaultAlgBuilder
     in algGraph @?= expected

case_process_sub_statement =
    let assignment = Assign [VarName (Name (T.pack "a"))] [Binop Sub (Number IntNum (T.pack "1")) (Number IntNum (T.pack "2"))]
        expected =
            [ Func{fIn = [T.pack "!1#0", T.pack "!2#0"], fOut = [T.pack "a"], fValues = [], fName = "sub", fInt = []}
            , Func{fIn = [], fOut = [T.pack "2"], fValues = [2], fName = "constant", fInt = []}
            , Func{fIn = [], fOut = [T.pack "1"], fValues = [1], fName = "constant", fInt = []}
            ]
        (_str :: String, AlgBuilder{algGraph}) = runState (processStatement (T.pack "_") assignment) defaultAlgBuilder
     in algGraph @?= expected

case_process_divide_statement =
    let assignment = Assign [VarName (Name (T.pack "a"))] [Binop Div (Number IntNum (T.pack "1")) (Number IntNum (T.pack "2"))]
        expected =
            [ Func{fIn = [T.pack "!1#0", T.pack "!2#0"], fOut = [T.pack "a"], fValues = [], fName = "divide", fInt = []}
            , Func{fIn = [], fOut = [T.pack "2"], fValues = [2], fName = "constant", fInt = []}
            , Func{fIn = [], fOut = [T.pack "1"], fValues = [1], fName = "constant", fInt = []}
            ]
        (_str :: String, AlgBuilder{algGraph}) = runState (processStatement (T.pack "_") assignment) defaultAlgBuilder
     in algGraph @?= expected

case_process_multiply_statement =
    let assignment = Assign [VarName (Name (T.pack "a"))] [Binop Mul (Number IntNum (T.pack "1")) (Number IntNum (T.pack "2"))]
        expected =
            [ Func{fIn = [T.pack "!1#0", T.pack "!2#0"], fOut = [T.pack "a"], fValues = [], fName = "multiply", fInt = []}
            , Func{fIn = [], fOut = [T.pack "2"], fValues = [2], fName = "constant", fInt = []}
            , Func{fIn = [], fOut = [T.pack "1"], fValues = [1], fName = "constant", fInt = []}
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
            [ Func{fIn = [T.pack "_0#a", T.pack "!3#0"], fOut = [T.pack "a"], fValues = [], fName = "add", fInt = []}
            , Func{fIn = [], fOut = [T.pack "3"], fValues = [3], fName = "constant", fInt = []}
            , Func{fIn = [T.pack "!1#0", T.pack "!2#0"], fOut = [T.pack "_0#a"], fValues = [], fName = "add", fInt = []}
            , Func{fIn = [], fOut = [T.pack "2"], fValues = [2], fName = "constant", fInt = []}
            , Func{fIn = [], fOut = [T.pack "1"], fValues = [1], fName = "constant", fInt = []}
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
            [ F.constant 2 ["t^0#0"] :: F String Int
            , F.constant (-1) ["r^0#0"]
            , F.add "a^0#0" "t^0#0" ["_1#loop"]
            , F.add "_1#loop" "r^0#0" ["_0#loop"]
            , F.loop 0 "_0#loop" ["a^0#0"]
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
            [ F.constant 1 ["t^0#0"] :: F String Int
            , F.constant 1 ["r^0#0"]
            , F.add "a^0#0" "t^0#0" ["_1#loop"]
            , F.add "_1#loop" "r^0#0" ["_0#loop"]
            , F.loop 0 "_0#loop" ["a^0#0"]
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
            [ F.neg "a^0#0" ["b^0#0"] :: F String Int
            , F.loop 0 "b^0#0" ["a^0#0"]
            ]
     in functions (parseLuaSources src) @?= dfg

defaultAlgBuilder = AlgBuilder{algGraph = [], algBuffer = Map.empty, algVarGen = Map.empty} :: AlgBuilder String Int

tests :: TestTree
tests = $(testGroupGenerator)
