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
import NITTA.Intermediate.DataFlow
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
        expected = Map.fromList [(T.pack "a", Variable{luaValueName = T.pack "a", luaValueParsedFunction = F.constant 2 ["a^0#0"], luaValueAssignCount = 0, luaValueAccessCount = 0, isStartupArgument = False, startupArgumentString = T.pack ""})]
        AlgBuilder{algBuffer} = (execState (processStatement (T.pack "_") assignment) defaultAlgBuilder) :: AlgBuilder Int
     in algBuffer @?= expected

case_process_assignment_statement =
    let assignment = Assign [VarName (Name $ T.pack "a")] [Number IntNum (T.pack "2")]
        expected = Map.fromList [(T.pack "a", Variable{luaValueName = T.pack "a", luaValueParsedFunction = F.constant 2 ["a^0#0"], luaValueAssignCount = 0, luaValueAccessCount = 0, isStartupArgument = False, startupArgumentString = T.pack ""})]
        AlgBuilder{algBuffer} = (execState (processStatement (T.pack "_") assignment) defaultAlgBuilder) :: AlgBuilder Int
     in algBuffer @?= expected

case_process_multiple_assignments_statement =
    let assignment = Assign [VarName (Name $ T.pack "a"), VarName (Name $ T.pack "b")] [Number IntNum $ T.pack "2", Number FloatNum $ T.pack "2.5"]
        expected =
            Map.fromList
                [ (T.pack "a", Variable{luaValueName = T.pack "a", luaValueParsedFunction = F.constant 2 ["a^0#0"], luaValueAssignCount = 0, luaValueAccessCount = 0, isStartupArgument = False, startupArgumentString = T.pack ""})
                , (T.pack "b", Variable{luaValueName = T.pack "b", luaValueParsedFunction = F.constant 2 ["b^0#0"], luaValueAssignCount = 0, luaValueAccessCount = 0, isStartupArgument = False, startupArgumentString = T.pack ""})
                ]
        AlgBuilder{algBuffer} = (execState (processStatement (T.pack "_") assignment) defaultAlgBuilder) :: AlgBuilder Int
     in algBuffer @?= expected

case_process_add_statement =
    let assignment = Assign [VarName (Name (T.pack "a"))] [Binop Add (Number IntNum (T.pack "1")) (Number IntNum (T.pack "2"))]
        expected =
            DFCluster
                [ DFLeaf (F{fun = F.constant 1 ["!1#0"], funHistory = []})
                , DFLeaf (F{fun = F.constant 2 ["!2#0"], funHistory = []})
                --, DFLeaf (F {fun = F.Add (I "!1#0") (I "!2#0") (O (fromList ["a"])), funHistory = []})
                ]
        AlgBuilder{algGraph} = (execState (processStatement (T.pack "_") assignment) defaultAlgBuilder) :: AlgBuilder Int
     in algGraph @?= expected

case_process_sub_statement =
    let assignment = Assign [VarName (Name (T.pack "a"))] [Binop Sub (Number IntNum (T.pack "1")) (Number IntNum (T.pack "2"))]
        expected =
            DFCluster
                [ DFLeaf (F{fun = F.constant 1 ["!1#0"], funHistory = []})
                , DFLeaf (F{fun = F.constant 2 ["!2#0"], funHistory = []})
                --, DFLeaf (F {fun = F.Sub (I "!1#0") (I "!2#0") (O (fromList ["a"])), funHistory = []})
                ]
        AlgBuilder{algGraph} = (execState (processStatement (T.pack "_") assignment) defaultAlgBuilder) :: AlgBuilder Int
     in algGraph @?= expected

case_process_divide_statement =
    let assignment = Assign [VarName (Name (T.pack "a"))] [Binop Div (Number IntNum (T.pack "1")) (Number IntNum (T.pack "2"))]
        expected =
            DFCluster
                [ DFLeaf (F{fun = F.constant 1 ["!1#0"], funHistory = []})
                , DFLeaf (F{fun = F.constant 2 ["!2#0"], funHistory = []})
                --, DFLeaf (F {fun = F.Division (I "!1#0") (I "!2#0") (O (fromList ["a"])) (O (fromList [""])), funHistory = []})
                ]
        AlgBuilder{algGraph} = (execState (processStatement (T.pack "_") assignment) defaultAlgBuilder) :: AlgBuilder Int
     in algGraph @?= expected

case_process_multiply_statement =
    let assignment = Assign [VarName (Name (T.pack "a"))] [Binop Mul (Number IntNum (T.pack "1")) (Number IntNum (T.pack "2"))]
        expected =
            DFCluster
                [ DFLeaf (F{fun = F.constant 1 ["!1#0"], funHistory = []})
                , DFLeaf (F{fun = F.constant 2 ["!2#0"], funHistory = []})
                --, DFLeaf (F {fun = F.Multiply (I "!1#0") (I "!2#0") (O (fromList ["a"])), funHistory = []})
                ]
        AlgBuilder{algGraph} = (execState (processStatement (T.pack "_") assignment) defaultAlgBuilder) :: AlgBuilder Int
     in algGraph @?= expected

case_temporary_variable =
    let assignment = Assign [VarName (Name $ T.pack "a")] [Binop Add (Binop Add (Number IntNum $ T.pack "1") (Number IntNum $ T.pack "2")) (Number IntNum $ T.pack "3")]
        expected =
            DFCluster
                [ DFLeaf (F{fun = F.constant 3 ["!3#0"], funHistory = []})
                , DFLeaf (F{fun = F.add "!1#0" "!2#0" ["_0#a"], funHistory = []})
                , DFLeaf (F{fun = F.constant 2 ["!2#0"], funHistory = []})
                , DFLeaf (F{fun = F.constant 1 ["!1#0"], funHistory = []})
                --, DFLeaf (F {fun = F.Multiply (I "!1#0") (I "!2#0") (O (fromList ["a"])), funHistory = []})
                ]
        AlgBuilder{algGraph} = (execState (processStatement (T.pack "_") assignment) defaultAlgBuilder) :: AlgBuilder Int
     in algGraph @?= expected

--case_debug =
--   let result = parseLuaSources $ T.pack "function sum(x)\n    y = 2 + x\n    sum(y + x)\nend\nsum(0)"
--        expected = DFCluster []
--     in expected @?= result

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
            [ F.loop 0 "_0#loop" ["a^0#0"]
            , F.add "_1#loop" "r^0#0" ["_0#loop"]
            , F.constant (-1) ["r^0#0"]
            , F.add "a^0#0" "t^0#0" ["_1#loop"]
            , F.constant 2 ["t^0#0"] :: F String Int
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
            [ F.loop 0 "_0#loop" ["a^0#0"]
            , F.add "_1#loop" "r^0#0" ["_0#loop"]
            , F.constant 1 ["r^0#0"]
            , F.add "a^0#0" "t^0#0" ["_1#loop"]
            , F.constant 1 ["t^0#0"] :: F String Int
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
            [ F.loop 0 "b^0#0" ["a^0#0"]
            , F.negative "a^0#0" ["b^0#0"] :: F String Int
            ]
     in functions (parseLuaSources src) @?= dfg

defaultAlgBuilder = AlgBuilder{algGraph = DFCluster [], algBuffer = Map.empty, algVarGen = Map.empty}

tests :: TestTree
tests = $(testGroupGenerator)
