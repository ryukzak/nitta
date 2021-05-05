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
  | Variable {luaValueName :: T.Text, luaValueString :: T.Text, luaValueType :: NumberType, luaValueAccessCount :: Int, luaValueAssignCount :: Int}
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
  a' <- parseExpArg a
  b' <- parseExpArg b
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

parseExpArg :: Exp -> State (DataFlowGraph String Int, Map.Map T.Text LuaValue) String
parseExpArg n@(Number _ text) = do
  a <- addConstant n
  return ""
parseExpArg (Unop Neg n) = do
  val <- parseExpArg n
  return ("-" ++ val)
parseExpArg (PrefixExp (PEVar (VarName (Name name)))) = do
  (_, constants) <- get
  return undefined
parseExpArg _ = undefined

--Lua language Stat structure parsing
--LocalAssign
processStatement _ (LocalAssign _names Nothing) = do
  return ""
processStatement fn (LocalAssign names (Just exps)) =
  processStatement fn $ Assign (map VarName names) exps
--Assign
processStatement fn (Assign lexps@[_] [Unop Neg (Number ntype ntext)]) =
  processStatement fn (Assign lexps [Number ntype ("-" <> ntext)])
processStatement _ (Assign lexps@[_] [n@(Number _ _)]) = addVariable (head $ parseLeftExp lexps) n
processStatement _ (Assign [lexp] [rexp]) = do
  parseRightExp (parseLeftExp [lexp]) rexp
processStatement startupFunctionName (Assign vars exps) | length vars == length exps = do
  mapM_ (\(var, expr) -> processStatement startupFunctionName (Assign [var] [expr])) $ zip vars exps
  return ""
--startup function recursive call
processStatement fn (FunCall (NormalFunCall (PEVar (VarName (Name fName))) (Args args)))
  | fn == fName = do
    (graph, constants) <- get
    put (graph, constants)
    return ""
processStatement _ _ = undefined

addConstant :: Exp -> State (DataFlowGraph String Int, Map.Map T.Text LuaValue) String
addConstant (Number valueType valueString) = do
  (graph, constants) <- get
  case Map.lookup valueString constants of
    Just _ -> do
      let newValue = updateConstant $ Map.lookup valueString constants
      put (addFuncToDataFlowGraph graph (F.constant (read $ T.unpack $ luaValueString newValue) [getConstantName newValue]), Map.insert valueString newValue constants)
      return $ getConstantName newValue
    Nothing -> do
      let value = Constant {luaValueName = valueString, luaValueType = valueType, luaValueAccessCount = 0}
      put (graph, Map.insert valueString value constants)
      return $ getConstantName value
  where
    updateConstant (Just Constant {luaValueName, luaValueType, luaValueAccessCount}) = Constant {luaValueName = luaValueName, luaValueType = luaValueType, luaValueAccessCount = luaValueAccessCount + 1}
    updateConstant _ = undefined
    getConstantName Constant {luaValueName, luaValueAccessCount} = T.unpack luaValueName ++ "#" ++ show luaValueAccessCount
    getConstantName _ = undefined
addConstant _ = undefined

addVariable name (Number valueType valueString) = do
  (graph, constants) <- get
  case Map.lookup name constants of
    Just _ -> do
      let newValue = updateConstant $ Map.lookup name constants
      put (addFuncToDataFlowGraph graph (F.constant (read $ T.unpack $ luaValueString newValue) [getVariableName newValue]), Map.insert name newValue constants)
      return $ getVariableName newValue
    Nothing -> do
      let value = Variable {luaValueName = name, luaValueString = valueString, luaValueType = valueType, luaValueAccessCount = 0, luaValueAssignCount = 0}
      put (graph, Map.insert name value constants)
      return $ getVariableName value
  where
    updateConstant (Just Variable {luaValueName, luaValueString, luaValueType, luaValueAccessCount, luaValueAssignCount}) = Variable {luaValueName = luaValueName, luaValueString = luaValueString, luaValueType = luaValueType, luaValueAccessCount = luaValueAccessCount, luaValueAssignCount = luaValueAssignCount + 1}
    updateConstant _ = undefined
    getVariableName Variable {luaValueName, luaValueAccessCount, luaValueAssignCount} = T.unpack luaValueName ++ "^" ++ show luaValueAssignCount ++ "#" ++ show luaValueAccessCount
    getVariableName _ = undefined
addVariable _ _ = undefined

buildAlg syntaxTree = fst $
  flip execState st $ do
    mapM_ (processStatement startupFunctionName) $ funAssignStatements startupFunctionDef
  where
    (startupFunctionName, _, startupFunctionDef) = findStartupFunction syntaxTree
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