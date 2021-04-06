module NITTA.LuaFrontend.TestsNew (
    tests,
) where


import Data.String.Interpolate
import Language.Lua
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Types
import NITTA.LuaFrontendNew
import Test.Tasty.HUnit
import Test.Tasty.TH
import Test.Tasty
import Data.Text (Text)


case_lua_constant_declatation =
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
        result = (pack "sum",FunCall (NormalFunCall (PEVar (VarName (Name $ pack "sum"))) _),FunAssign (FunName (Name $ pack "sum") _ _) _)
     in result @?= findStartupFunction $ parseText src


tests :: TestTree
tests = $(testGroupGenerator)