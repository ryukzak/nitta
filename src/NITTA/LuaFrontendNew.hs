{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}


module NITTA.LuaFrontendNew (
    parseLuaSources,
    -- *Internal
    findStartupFunction
) where

import Data.Text (Text, pack, unpack)
import Language.Lua
import NITTA.Intermediate.DataFlow
import NITTA.Intermediate.Types


findStartupFunction (Block statements Nothing)
    | [call] <- filter (\case FunCall{} -> True; _ -> False) statements
      , [funAssign] <- filter (\case FunAssign{} -> True; _ -> False) statements
      , (FunCall (NormalFunCall (PEVar (VarName (Name fnCall))) _)) <- call
      , (FunAssign (FunName (Name fnAssign) _ _) _) <- funAssign
      , fnCall == fnAssign =
        (fnCall, call, funAssign)
findStartupFunction _ = error "can't find startup function in lua source code"

parseLuaSources src = 
    let syntaxTree = either (\e -> error $ "Exception while parsing Lua sources: " ++ show e) id $ parseText chunk src
        (mainName, mainCall, mainFunDef) = findStartupFunction syntaxTree

    in mainCall