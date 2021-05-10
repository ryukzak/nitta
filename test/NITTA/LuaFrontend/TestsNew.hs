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

module NITTA.LuaFrontend.TestsNew
  ( tests,
  )
where

import Control.Monad.State
import qualified Data.Map as Map
import Data.Set.Internal
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
      result = Map.fromList [(T.pack "a", Variable {luaValueName = T.pack "a", luaValueString = T.pack "2", luaValueType = IntNum, luaValueAssignCount = 1, luaValueAccessCount = 0})]
   in snd (execState (processStatement (T.pack "_") assignment) (DFCluster [], Map.empty)) @?= result

case_process_assignment_statement =
  let assignment = Assign [VarName (Name $ T.pack "a")] [Number IntNum (T.pack "2")]
      result = Map.fromList [(T.pack "a", Variable {luaValueName = T.pack "a", luaValueString = T.pack "2", luaValueType = IntNum, luaValueAssignCount = 1, luaValueAccessCount = 0})]
   in snd (execState (processStatement (T.pack "_") assignment) (DFCluster [], Map.empty)) @?= result

case_process_multiple_assignments_statement =
  let assignment = Assign [VarName (Name $ T.pack "a"), VarName (Name $ T.pack "b")] [Number IntNum $ T.pack "2", Number FloatNum $ T.pack "2.5"]
      result = Map.fromList [(T.pack "a", Variable {luaValueName = T.pack "a", luaValueString = T.pack "2", luaValueType = IntNum, luaValueAssignCount = 1, luaValueAccessCount = 0}), (T.pack "b", Variable {luaValueName = T.pack "b", luaValueString = T.pack "2.5", luaValueType = FloatNum, luaValueAssignCount = 1, luaValueAccessCount = 0})]
   in snd (execState (processStatement (T.pack "_") assignment) (DFCluster [], Map.empty)) @?= result

case_process_add_statement =
  let assignment = Assign [VarName (Name (T.pack "a"))] [Binop Add (Number IntNum (T.pack "1")) (Number IntNum (T.pack "2"))]
      result = DFCluster [DFLeaf (F {fun = F.Add (I "!1#0") (I "!2#0") (O (fromList ["a"])), funHistory = []})]
   in fst (execState (processStatement (T.pack "_") assignment) (DFCluster [], Map.empty)) @?= result

case_process_sub_statement =
  let assignment = Assign [VarName (Name (T.pack "a"))] [Binop Sub (Number IntNum (T.pack "1")) (Number IntNum (T.pack "2"))]
      result = DFCluster [DFLeaf (F {fun = F.Sub (I "!1#0") (I "!2#0") (O (fromList ["a"])), funHistory = []})]
   in fst (execState (processStatement (T.pack "_") assignment) (DFCluster [], Map.empty)) @?= result

case_process_divide_statement =
  let assignment = Assign [VarName (Name (T.pack "a"))] [Binop Div (Number IntNum (T.pack "1")) (Number IntNum (T.pack "2"))]
      result = DFCluster [DFLeaf (F {fun = F.Division (I "!1#0") (I "!2#0") (O (fromList ["a"])) (O (fromList [""])), funHistory = []})]
   in fst (execState (processStatement (T.pack "_") assignment) (DFCluster [], Map.empty)) @?= result

case_process_multiply_statement =
  let assignment = Assign [VarName (Name (T.pack "a"))] [Binop Mul (Number IntNum (T.pack "1")) (Number IntNum (T.pack "2"))]
      result = DFCluster [DFLeaf (F {fun = F.Multiply (I "!1#0") (I "!2#0") (O (fromList ["a"])), funHistory = []})]
   in fst (execState (processStatement (T.pack "_") assignment) (DFCluster [], Map.empty)) @?= result

case_debug = 
   let result = parseLuaSources $ T.pack "function sum(x)\n    y = 2 + x\n    sum(y)\nend\nsum(0)"
       expected = DFCluster []
   in expected @?= result


tests :: TestTree
tests = $(testGroupGenerator)