{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module NITTA.LuaFrontendNew
  ( parseLuaSources,

    -- * Internal
    LuaValue (..),
    findStartupFunction,
    getLuaBlockFromSources,
    processStatement,
  )
where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Text as T
import Language.Lua
import NITTA.Intermediate.DataFlow
import qualified NITTA.Intermediate.Functions as F

data LuaValue
  = Constant {luaValueName :: T.Text, luaValueType :: NumberType, luaValueAccessCount :: Int}
  | Variable {luaValueName :: T.Text, luaValueString :: T.Text, luaValueType :: NumberType, luaValueAccessCount :: Int, luaValueAssignCount :: Int, isStartupArgument :: Bool}
  deriving (Show)

instance Eq LuaValue where
  (==) Constant {luaValueName = a} Constant {luaValueName = b} = a == b
  (==) Variable {luaValueName = a} Variable {luaValueName = b} = a == b
  (==) _ _ = False

instance Ord LuaValue where
  (<=) Constant {luaValueName = a} Constant {luaValueName = b} = a == b
  (<=) Constant {luaValueName = a} Variable {luaValueName = b} = a == b
  (<=) Variable {luaValueName = a} Constant {luaValueName = b} = a <= b
  (<=) Variable {luaValueName = a} Variable {luaValueName = b} = a <= b

funAssignStatements (FunAssign _ (FunBody _ _ (Block statements _))) = statements
funAssignStatements _ = error "funAssignStatements : not a function assignment"

--left part of lua statement
parseLeftExp = map $ \case
  VarName (Name v) -> v
  e -> error $ "bad left expression: " ++ show e

--right part of lua statement
parseRightExp fOut (Binop op a b) = do
  (graph, constants) <- get
  (a', _) <- parseExpArg a
  (b', _) <- parseExpArg b
  put (addFuncToDataFlowGraph graph (getBinopFunc op a' b'), constants)
  return ""
  where
    getBinopFunc Add a' b' = F.add a' b' strFOut
    getBinopFunc Sub a' b' = F.sub a' b' strFOut
    getBinopFunc Mul a' b' = F.multiply a' b' strFOut
    getBinopFunc Div a' b' = F.division a' b' [head strFOut] (tail strFOut)
    getBinopFunc o _ _ = error $ "unknown binop: " ++ show o
    strFOut = map T.unpack fOut
parseRightExp _ _ = undefined

parseExpArg :: Exp -> State (DataFlowGraph String Int, Map.Map T.Text LuaValue) (String, T.Text)
parseExpArg n@(Number _ _) = do
  addConstant n
parseExpArg (Unop Neg n) = do
  (name, luaValue) <- parseExpArg n
  return ([head name] ++ "-" ++ tail name, luaValue)
parseExpArg (PrefixExp (PEVar (VarName (Name name)))) = do
  addVariableAccess name
parseExpArg _ = undefined

addStartupFuncArgs :: Stat -> Stat -> State (DataFlowGraph String Int, Map.Map T.Text LuaValue) String
addStartupFuncArgs (FunCall (NormalFunCall _ (Args args))) (FunAssign _ (FunBody names _ _)) = do
  mapM_ (\(arg, Name name) -> addVariable name arg True) $ zip args names
  return ""
addStartupFuncArgs _ _ = undefined

--Lua language Stat structure parsing
--LocalAssign
processStatement :: T.Text -> Stat -> State (DataFlowGraph String Int, Map.Map T.Text LuaValue) String
processStatement _ (LocalAssign _names Nothing) = do
  return ""
processStatement fn (LocalAssign names (Just exps)) =
  processStatement fn $ Assign (map VarName names) exps
--Assign
processStatement fn (Assign lexps@[_] [Unop Neg (Number ntype ntext)]) =
  processStatement fn (Assign lexps [Number ntype ("-" <> ntext)])
processStatement _ (Assign lexps@[_] [n@(Number _ _)]) = addVariable (head $ parseLeftExp lexps) n False
processStatement _ (Assign [lexp] [rexp]) = do
  parseRightExp (parseLeftExp [lexp]) rexp
processStatement startupFunctionName (Assign vars exps) | length vars == length exps = do
  mapM_ (\(var, expr) -> processStatement startupFunctionName (Assign [var] [expr])) $ zip vars exps
  return ""
--startup function recursive call
processStatement fn (FunCall (NormalFunCall (PEVar (VarName (Name fName))) (Args args)))
  | fn == fName = do
    (_, constants) <- get
    let startupVars = filter (\case Variable {isStartupArgument} -> isStartupArgument; _ -> False) $ map snd $ Map.toList constants
    mapM_ parseStartupArg $ zip args startupVars
    return ""
  where
    parseStartupArg :: (Exp, LuaValue) -> State (DataFlowGraph String Int, Map.Map T.Text LuaValue) String
    parseStartupArg (arg, value) = do
      (varName, luaValue) <- parseExpArg arg
      (graph, constants) <- get
      put (addFuncToDataFlowGraph graph (F.loop (read $ T.unpack luaValue) (getDefaultName value) [varName]), constants)
      return ""
    getDefaultName Variable {luaValueName} = T.unpack luaValueName ++ "^0#0"
    getDefaultName _ = undefined
processStatement _ _ = undefined

addConstant :: Exp -> State (DataFlowGraph String Int, Map.Map T.Text LuaValue) (String, T.Text)
addConstant (Number valueType valueString) = do
  (graph, constants) <- get
  case Map.lookup valueString constants of
    Just value -> do
      let newValue = updateConstant value
      put (graph, Map.insert valueString newValue constants)
      return $ (getConstantName newValue, luaValueName newValue)
    Nothing -> do
      let value = Constant {luaValueName = valueString, luaValueType = valueType, luaValueAccessCount = 0}
      put (graph, Map.insert valueString value constants)
      return (getConstantName value, luaValueName value)
  where
    updateConstant Constant {luaValueName, luaValueType, luaValueAccessCount} = Constant {luaValueName = luaValueName, luaValueType = luaValueType, luaValueAccessCount = luaValueAccessCount + 1}
    updateConstant _ = undefined
    getConstantName Constant {luaValueName, luaValueAccessCount} = "!" ++ T.unpack luaValueName ++ "#" ++ show luaValueAccessCount
    getConstantName _ = undefined
addConstant _ = undefined

addVariable name (Number valueType valueString) isStartupArg = do
  (graph, constants) <- get
  case Map.lookup name constants of
    Just value -> do
      let newValue = updateConstant value
      put (graph, Map.insert name newValue constants)
      return $ getVariableName newValue
    Nothing -> do
      let value = Variable {luaValueName = name, luaValueString = valueString, luaValueType = valueType, luaValueAccessCount = 0, luaValueAssignCount = 0, isStartupArgument = isStartupArg}
      put (graph, Map.insert name value constants)
      return $ getVariableName value
  where
    updateConstant Variable {luaValueName, luaValueString, luaValueType, luaValueAccessCount, luaValueAssignCount} =
      Variable {luaValueName = luaValueName, luaValueString = luaValueString, luaValueType = luaValueType, luaValueAccessCount = luaValueAccessCount, luaValueAssignCount = luaValueAssignCount + 1, isStartupArgument = isStartupArg}
    updateConstant _ = undefined
    getVariableName Variable {luaValueName, luaValueAccessCount, luaValueAssignCount} = T.unpack luaValueName ++ "^" ++ show luaValueAssignCount ++ "#" ++ show luaValueAccessCount
    getVariableName _ = undefined
addVariable _ _ _ = undefined

addVariableAccess name = do
  (graph, constants) <- get
  case Map.lookup name constants of
    Just value -> do
      let newValue = updateVariable value
      let oldName = getVariableName value
      put (addFuncToDataFlowGraph graph (F.constant (read $ T.unpack $ luaValueString newValue) [getVariableName newValue]), Map.insert name newValue constants)
      return (oldName, luaValueString value)
    Nothing -> error ("variable '" ++ show name ++ " not found.")
  where
    updateVariable Variable {luaValueName, luaValueString, luaValueType, luaValueAccessCount, luaValueAssignCount, isStartupArgument} =
      Variable {luaValueName = luaValueName, luaValueString = luaValueString, luaValueType = luaValueType, luaValueAccessCount = luaValueAccessCount + 1, luaValueAssignCount = luaValueAssignCount, isStartupArgument = isStartupArgument}
    updateVariable _ = undefined
    getVariableName Variable {luaValueName, luaValueAccessCount, luaValueAssignCount} = T.unpack luaValueName ++ "^" ++ show luaValueAssignCount ++ "#" ++ show luaValueAccessCount
    getVariableName _ = undefined

buildAlg syntaxTree = fst $
  flip execState st $ do
    _ <- addStartupFuncArgs startupFunctionDef startupFunctionCall
    mapM_ (processStatement startupFunctionName) $ funAssignStatements startupFunctionDef
  where
    (startupFunctionName, startupFunctionCall, startupFunctionDef) = findStartupFunction syntaxTree
    st = (DFCluster [], Map.empty)

findStartupFunction (Block statements Nothing)
  | [call] <- filter (\case FunCall {} -> True; _ -> False) statements,
    [funAssign] <- filter (\case FunAssign {} -> True; _ -> False) statements,
    (FunCall (NormalFunCall (PEVar (VarName (Name fnCall))) _)) <- call,
    (FunAssign (FunName (Name fnAssign) _ _) _) <- funAssign,
    fnCall == fnAssign =
    (fnCall, call, funAssign)
findStartupFunction _ = error "can't find startup function in lua source code"

getLuaBlockFromSources src = either (\e -> error $ "Exception while parsing Lua sources: " ++ show e) id $ parseText chunk src

parseLuaSources :: T.Text -> DataFlowGraph String Int
parseLuaSources src =
  let syntaxTree = getLuaBlockFromSources src
      alg = buildAlg syntaxTree
   in alg