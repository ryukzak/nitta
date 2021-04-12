{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}


module NITTA.LuaFrontendNew (
    parseLuaSources,
    AlgBuilderItem (..),
    -- *Internal
    findStartupFunction,
    getLuaBlockFromSources,
    processStatement,
) where

import Language.Lua
import Control.Monad.State
import Data.Text (Text)


data AlgBuilderItem =
     Constant {cName :: Text, cValueString :: Text}
        deriving (Eq, Ord)

instance Show AlgBuilderItem where
--    show (Function ins outs funcName values ints) = "Function { fIn = {" ++ show ins ++ "}, fOut = {" ++ show outs ++ "}, name = " ++ show funcName ++ ", values = {" ++ show values ++ "}, fInt = {" ++ show ints
    show (Constant name value) = "Constant { name = {" ++ show name ++ "}, value = {" ++ show value ++ "}"

funAssignStatements (FunAssign _ (FunBody _ _ (Block statements _))) = statements
funAssignStatements _ = error "funAssignStatements : not a function assignment"


parseLeftExp = map $ \case
    VarName (Name v) -> v
    e -> error $ "bad left expression: " ++ show e

processStatement :: Text -> Stat -> State [AlgBuilderItem] Int
--Lua language Stat structure parsing
--LocalAssign
processStatement _ (LocalAssign _names Nothing) = do
    return 0
processStatement fn (LocalAssign names (Just exps)) =
    processStatement fn $ Assign (map VarName names) exps

--Assign
processStatement fn (Assign lexps@[_] [Unop Neg (Number ntype ntext)]) =
    processStatement fn (Assign lexps [Number ntype ("-" <> ntext)])
    
processStatement _ (Assign lexps@[_] [n@(Number _ _)]) = do
    let [name] = parseLeftExp lexps
    expConstant name n

processStatement startupFunctionName (Assign vars exps) | length vars == length exps = do
    mapM_ (\(var,expr) -> processStatement startupFunctionName (Assign [var] [expr])) $ zip vars exps
    return 0


processStatement _ _ = undefined

expConstant :: Text -> Exp -> State [AlgBuilderItem] Int
expConstant name (Number _ valueString) = do
    items <- get
    put (items ++ [Constant{cName=name, cValueString=valueString}]) 
    return 0
expConstant _ _ = undefined

buildAlg syntaxTree = flip execState st $ do
    mapM_ (processStatement startupFunctionName) $ funAssignStatements startupFunctionDef
    where
        (startupFunctionName, _, startupFunctionDef) = findStartupFunction syntaxTree
        st = []::[AlgBuilderItem]


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
        alg = buildAlg syntaxTree
    in alg