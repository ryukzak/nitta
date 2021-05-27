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
import NITTA.Intermediate.Types

data LuaValue
  = Constant {luaValueName :: T.Text, luaValueParsedFunction :: F String Int, luaValueType :: NumberType, luaValueAccessCount :: Int}
  | Variable {luaValueName :: T.Text, luaValueParsedFunction :: F String Int, luaValueAccessCount :: Int, luaValueAssignCount :: Int, isStartupArgument :: Bool}
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
parseLeftExp (VarName (Name v)) = v
parseLeftExp _ = undefined

--right part of lua statement
parseRightExp fOut (Number _ valueString) = do
  name <- getFreeVariableName fOut
  addVariable fOut (F.constant (read (T.unpack valueString)) [name]) False
parseRightExp fOut (Binop op a b) = do
  (a', _) <- parseExpArg a
  (b', _) <- parseExpArg b
  name <- getFreeVariableName fOut
  addVariable fOut (getBinopFunc op a' b' [name]) False
  where
    getBinopFunc Add a' b' resultName = F.add a' b' resultName
    getBinopFunc Sub a' b' resultName = F.sub a' b' resultName
    getBinopFunc Mul a' b' resultName = F.multiply a' b' resultName
    getBinopFunc Div a' b' resultName = F.division a' b' [head resultName] (tail resultName)
    getBinopFunc o _ _ _ = error $ "unknown binop: " ++ show o
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
addStartupFuncArgs (FunCall (NormalFunCall _ (Args exps))) (FunAssign _ (FunBody names _ _)) = do
  
  mapM_ (\(Name name, Number _ valueString) -> addVariable name (F.constant (read (T.unpack valueString)) [T.unpack name ++ "^0#0"]) True) $ zip names exps
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
processStatement _ (Assign [lexp] [rexp]) = do
  parseRightExp (parseLeftExp lexp) rexp
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
      let constant = F.constant (read (T.unpack valueString)) [getConstantName valueString (luaValueAccessCount value)]
      let newValue = updateConstant value constant
      put (graph, Map.insert valueString newValue constants)
      return (getConstantName (luaValueName newValue) (luaValueAccessCount newValue), luaValueName newValue)
    Nothing -> do
      let constant = F.constant (read (T.unpack valueString)) [getConstantName valueString (0 :: Int)]
      let value = Constant {luaValueName = valueString, luaValueParsedFunction = constant, luaValueType = valueType, luaValueAccessCount = 0}
      put (addFuncToDataFlowGraph graph constant, Map.insert valueString value constants)
      return (getConstantName (luaValueName value) (luaValueAccessCount value), luaValueName value)
  where
    updateConstant Constant {luaValueName, luaValueType, luaValueAccessCount} constant = Constant {luaValueName = luaValueName, luaValueParsedFunction = constant, luaValueType = luaValueType, luaValueAccessCount = luaValueAccessCount + 1}
    updateConstant _ _ = undefined
    getConstantName luaValueName luaValueAccessCount = "!" ++ T.unpack luaValueName ++ "#" ++ show luaValueAccessCount
addConstant _ = undefined

getFreeVariableName name = do
  (_, constants) <- get
  case Map.lookup name constants of
    Just value -> do
      return $ getVariableName name (luaValueAccessCount value) (luaValueAssignCount value)
    Nothing -> do
      return $ getVariableName name (0 :: Integer) (0 :: Integer)
  where
    getVariableName luaValueName luaValueAccessCount luaValueAssignCount = T.unpack luaValueName ++ "^" ++ show luaValueAssignCount ++ "#" ++ show luaValueAccessCount

addVariable name func isStartupArg = do
  (graph, constants) <- get
  case Map.lookup name constants of
    Just value -> do
      let newValue = updateConstant value
      put (graph, Map.insert name newValue constants)
      return $ getVariableName newValue
    Nothing -> do
      let value = Variable {luaValueName = name, luaValueParsedFunction = func, luaValueAccessCount = 0, luaValueAssignCount = 0, isStartupArgument = isStartupArg}
      put (graph, Map.insert name value constants)
      return $ getVariableName value
  where
    updateConstant Variable {luaValueName, luaValueAccessCount, luaValueAssignCount} =
      Variable {luaValueName = luaValueName, luaValueParsedFunction = func, luaValueAccessCount = luaValueAccessCount, luaValueAssignCount = luaValueAssignCount + 1, isStartupArgument = isStartupArg}
    updateConstant _ = undefined
    getVariableName Variable {luaValueName, luaValueAccessCount, luaValueAssignCount} = T.unpack luaValueName ++ "^" ++ show luaValueAssignCount ++ "#" ++ show luaValueAccessCount
    getVariableName _ = undefined

addVariableAccess :: T.Text -> State (DataFlowGraph String Int, Map.Map T.Text LuaValue) (String, T.Text)
addVariableAccess name = do
  (graph, constants) <- get
  case Map.lookup name constants of
    Just value -> do
      let newValue = updateVariable value
      let oldName = getVariableName value
      put (addFuncToDataFlowGraph graph (luaValueParsedFunction value), Map.insert name newValue constants)
      return (oldName, T.pack "")
    Nothing -> error ("variable '" ++ show name ++ " not found. Constants list : " ++ show constants)
  where
    updateVariable Variable {luaValueName, luaValueAccessCount, luaValueAssignCount, isStartupArgument, luaValueParsedFunction} =
      Variable {luaValueName = luaValueName, luaValueParsedFunction = luaValueParsedFunction, luaValueAccessCount = luaValueAccessCount + 1, luaValueAssignCount = luaValueAssignCount, isStartupArgument = isStartupArgument}
    updateVariable _ = undefined
    getVariableName Variable {luaValueName, luaValueAccessCount, luaValueAssignCount} = T.unpack luaValueName ++ "^" ++ show luaValueAssignCount ++ "#" ++ show luaValueAccessCount
    getVariableName _ = undefined

buildAlg syntaxTree = fst $
  flip execState st $ do
    _ <- addStartupFuncArgs startupFunctionCall startupFunctionDef
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

-- DFCluster [DFLeaf 2@const#0 + x#0 = y#0,DFLeaf Loop (X 0) (O [x#0]) (I y#0),DFLeaf const(2) = 2@const#0]