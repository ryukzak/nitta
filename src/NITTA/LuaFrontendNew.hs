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

data LuaValue = Constant {luaValueName :: T.Text, luaValueString :: T.Text, luaValueType :: NumberType, luaValueAccessCount :: Int}
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
  return 0
  where
    getBinopFunc Add a' b' = F.add a' b' strFOut
    getBinopFunc Sub a' b' = F.sub a' b' strFOut
    getBinopFunc Mul a' b' = F.multiply a' b' strFOut
    getBinopFunc Div a' b' = F.division a' b' [head strFOut] (tail strFOut)
    getBinopFunc o _ _ = error $ "unknown binop: " ++ show o
    strFOut = map T.unpack fOut
parseRightExp _ _ = undefined

parseExpArg :: Exp -> State (DataFlowGraph String Int, Map.Map T.Text LuaValue) String
parseExpArg (Number _ text) = return $ T.unpack text ++ "#0"
parseExpArg (Unop Neg n) = do
  val <- parseExpArg n
  return ("-" ++ val)
parseExpArg (PrefixExp (PEVar (VarName (Name name)))) = do
  (_, constants) <- get
  return undefined
parseExpArg _ = undefined

processStatement :: T.Text -> Stat -> State (DataFlowGraph String Int, Map.Map T.Text LuaValue) Int
--Lua language Stat structure parsing
--LocalAssign
processStatement _ (LocalAssign _names Nothing) = do
  return 0
processStatement fn (LocalAssign names (Just exps)) =
  processStatement fn $ Assign (map VarName names) exps
--Assign
processStatement fn (Assign lexps@[_] [Unop Neg (Number ntype ntext)]) =
  processStatement fn (Assign lexps [Number ntype ("-" <> ntext)])
processStatement _ (Assign lexps@[_] [n@(Number _ _)]) = addConstant (head $ parseLeftExp lexps) n
processStatement _ (Assign [lexp] [rexps]) = do
  parseRightExp (parseLeftExp [lexp]) rexps
processStatement startupFunctionName (Assign vars exps) | length vars == length exps = do
  mapM_ (\(var, expr) -> processStatement startupFunctionName (Assign [var] [expr])) $ zip vars exps
  return 0
--startup function recursive call
processStatement fn (FunCall (NormalFunCall (PEVar (VarName (Name fName))) (Args args)))
  | fn == fName = do
    (graph, constants) <- get
    put (graph, constants)
    return 0
processStatement _ _ = undefined

addConstant :: T.Text -> Exp -> State (DataFlowGraph String Int, Map.Map T.Text LuaValue) Int
addConstant name (Number valueType valueString) = do
  (graph, constants) <- get
  case Map.lookup name constants of
    Just _ -> do
      let newValue = updateConstant $ Map.lookup name constants
      put (addFuncToDataFlowGraph graph (F.constant (read $ T.unpack $ luaValueString newValue) [getConstantName newValue]), Map.insert name newValue constants)
    Nothing -> do
      put (graph, Map.insert name Constant {luaValueName = name, luaValueString = valueString, luaValueType = valueType, luaValueAccessCount = 0} constants)
  return 0
  where
    updateConstant (Just Variable {luaValueName, luaValueString, luaValueType, luaValueAccessCount, luaValueAssignCount}) = Variable {luaValueName = luaValueName, luaValueString = luaValueString, luaValueType = luaValueType, luaValueAccessCount = luaValueAccessCount, luaValueAssignCount = luaValueAssignCount + 1}
    updateConstant (Just Constant {luaValueName, luaValueString, luaValueType, luaValueAccessCount}) = Constant {luaValueName = luaValueName, luaValueString = luaValueString, luaValueType = luaValueType, luaValueAccessCount = luaValueAccessCount + 1}
    updateConstant _ = undefined
    getConstantName Variable {luaValueName, luaValueAccessCount, luaValueAssignCount} = T.unpack luaValueName ++ "^" ++ show luaValueAssignCount ++ "#" ++ show luaValueAccessCount
    getConstantName Constant {luaValueName, luaValueAccessCount} = T.unpack luaValueName ++ "#" ++ show luaValueAccessCount
addConstant _ _ = undefined

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