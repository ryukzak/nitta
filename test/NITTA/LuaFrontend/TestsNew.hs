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
        AlgBuilder{algBuffer} = execState (processStatement (T.pack "_") assignment) defaultAlgBuilder
     in algBuffer @?= expected

case_process_assignment_statement =
    let assignment = Assign [VarName (Name $ T.pack "a")] [Number IntNum (T.pack "2")]
        expected = Map.fromList [(T.pack "a", Variable{luaValueName = T.pack "a", luaValueParsedFunction = F.constant 2 ["a^0#0"], luaValueAssignCount = 0, luaValueAccessCount = 0, isStartupArgument = False, startupArgumentString = T.pack ""})]
        AlgBuilder{algBuffer} = execState (processStatement (T.pack "_") assignment) defaultAlgBuilder
     in algBuffer @?= expected

case_process_multiple_assignments_statement =
    let assignment = Assign [VarName (Name $ T.pack "a"), VarName (Name $ T.pack "b")] [Number IntNum $ T.pack "2", Number FloatNum $ T.pack "2.5"]
        expected =
            Map.fromList
                [ (T.pack "a", Variable{luaValueName = T.pack "a", luaValueParsedFunction = F.constant 2 ["a^0#0"], luaValueAssignCount = 0, luaValueAccessCount = 0, isStartupArgument = False, startupArgumentString = T.pack ""})
                , (T.pack "b", Variable{luaValueName = T.pack "b", luaValueParsedFunction = F.constant 2 ["b^0#0"], luaValueAssignCount = 0, luaValueAccessCount = 0, isStartupArgument = False, startupArgumentString = T.pack ""})
                ]
        AlgBuilder{algBuffer} = execState (processStatement (T.pack "_") assignment) defaultAlgBuilder
     in algBuffer @?= expected

case_process_add_statement =
    let assignment = Assign [VarName (Name (T.pack "a"))] [Binop Add (Number IntNum (T.pack "1")) (Number IntNum (T.pack "2"))]
        expected =
            DFCluster
                [ DFLeaf (F{fun = F.constant 1 ["!1#0"], funHistory = []})
                , DFLeaf (F{fun = F.constant 2 ["!2#0"], funHistory = []})
                --, DFLeaf (F {fun = F.Add (I "!1#0") (I "!2#0") (O (fromList ["a"])), funHistory = []})
                ]
        AlgBuilder{algGraph} = execState (processStatement (T.pack "_") assignment) defaultAlgBuilder
     in algGraph @?= expected

case_process_sub_statement =
    let assignment = Assign [VarName (Name (T.pack "a"))] [Binop Sub (Number IntNum (T.pack "1")) (Number IntNum (T.pack "2"))]
        expected =
            DFCluster
                [ DFLeaf (F{fun = F.constant 1 ["!1#0"], funHistory = []})
                , DFLeaf (F{fun = F.constant 2 ["!2#0"], funHistory = []})
                --, DFLeaf (F {fun = F.Sub (I "!1#0") (I "!2#0") (O (fromList ["a"])), funHistory = []})
                ]
        AlgBuilder{algGraph} = execState (processStatement (T.pack "_") assignment) defaultAlgBuilder
     in algGraph @?= expected

case_process_divide_statement =
    let assignment = Assign [VarName (Name (T.pack "a"))] [Binop Div (Number IntNum (T.pack "1")) (Number IntNum (T.pack "2"))]
        expected =
            DFCluster
                [ DFLeaf (F{fun = F.constant 1 ["!1#0"], funHistory = []})
                , DFLeaf (F{fun = F.constant 2 ["!2#0"], funHistory = []})
                --, DFLeaf (F {fun = F.Division (I "!1#0") (I "!2#0") (O (fromList ["a"])) (O (fromList [""])), funHistory = []})
                ]
        AlgBuilder{algGraph} = execState (processStatement (T.pack "_") assignment) defaultAlgBuilder
     in algGraph @?= expected

case_process_multiply_statement =
    let assignment = Assign [VarName (Name (T.pack "a"))] [Binop Mul (Number IntNum (T.pack "1")) (Number IntNum (T.pack "2"))]
        expected =
            DFCluster
                [ DFLeaf (F{fun = F.constant 1 ["!1#0"], funHistory = []})
                , DFLeaf (F{fun = F.constant 2 ["!2#0"], funHistory = []})
                --, DFLeaf (F {fun = F.Multiply (I "!1#0") (I "!2#0") (O (fromList ["a"])), funHistory = []})
                ]
        AlgBuilder{algGraph} = execState (processStatement (T.pack "_") assignment) defaultAlgBuilder
     in algGraph @?= expected

--case_debug =
--   let result = parseLuaSources $ T.pack "function sum(x)\n    y = 2 + x\n    sum(y + x)\nend\nsum(0)"
--        expected = DFCluster []
--     in expected @?= result


defaultAlgBuilder = AlgBuilder{algGraph = DFCluster [], algBuffer = Map.empty, algVarGen = Map.empty }

tests :: TestTree
tests = $(testGroupGenerator)
