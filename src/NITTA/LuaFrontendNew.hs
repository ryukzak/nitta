{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}


module NITTA.LuaFrontendNew (
    parseLuaSources,
    -- *Internal
    findStartupFunction,
    getLuaBlockFromSources
) where

import Language.Lua


findStartupFunction (Block statements Nothing)
    | [call] <- filter (\case FunCall{} -> True; _ -> False) statements
      , [funAssign] <- filter (\case FunAssign{} -> True; _ -> False) statements
      , (FunCall (NormalFunCall (PEVar (VarName (Name fnCall))) _)) <- call
      , (FunAssign (FunName (Name fnAssign) _ _) _) <- funAssign
      , fnCall == fnAssign =
        (fnCall, call, funAssign)
findStartupFunction _ = error "can't find startup function in lua source code"


getLuaBlockFromSources src = either (\e -> error $ "Exception while parsing Lua sources: " ++ show e) id $ parseText chunk src

parseLuaSources src = 
    let syntaxTree = getLuaBlockFromSources src
        (_, mainCall, _) = findStartupFunction syntaxTree
    in mainCall