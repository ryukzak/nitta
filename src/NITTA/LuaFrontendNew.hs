{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module NITTA.LuaFrontendNew
  ( parseLuaSources,

    -- * Internal
    Constant (..),
    findStartupFunction,
    getLuaBlockFromSources,
    processStatement,
  )
where

import Control.Monad.State
import qualified Data.Text as T
import Language.Lua
import NITTA.Intermediate.DataFlow
import qualified NITTA.Intermediate.Functions as F
import NITTA.Intermediate.Types

data Constant = Constant {cName :: T.Text, cValueString :: T.Text, cValueType :: NumberType}
  deriving (Eq, Show)

funAssignStatements (FunAssign _ (FunBody _ _ (Block statements _))) = statements
funAssignStatements _ = error "funAssignStatements : not a function assignment"

--left part of lua statement
parseLeftExp = map $ \case
  VarName (Name v) -> v
  e -> error $ "bad left expression: " ++ show e

--right part of lua statement
parseRightExp fOut (Binop op a b) = do
  (graph, constants) <- get
  put (addFuncToDataFlowGraph graph (getBinopFunc op), constants)
  return 0
  where
    getBinopFunc :: Binop -> F String Int
    getBinopFunc Add = F.add a' b' strFOut
    getBinopFunc Sub = F.sub a' b' strFOut
    getBinopFunc Mul = F.multiply a' b' strFOut
    getBinopFunc Div = F.division a' b' [head strFOut] (tail strFOut)
    getBinopFunc o = error $ "unknown binop: " ++ show o
    a' = parseExpArg a
    b' = parseExpArg b
    strFOut = map T.unpack fOut
parseRightExp _ _ = undefined

parseExpArg :: p -> String
parseExpArg _ = undefined

processStatement :: T.Text -> Stat -> State (DataFlowGraph String Int, [Constant]) Int
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

expConstant :: T.Text -> Exp -> State (DataFlowGraph String Int, [Constant]) Int
expConstant name (Number valueType valueString) = do
  (graph, constants) <- get
  put (graph, constants ++ [Constant {cName = name, cValueString = valueString, cValueType = valueType}])
  return 0
expConstant _ _ = undefined

buildAlg syntaxTree = fst $
  flip execState st $ do
    mapM_ (processStatement startupFunctionName) $ funAssignStatements startupFunctionDef
  where
    (startupFunctionName, _, startupFunctionDef) = findStartupFunction syntaxTree
    st = (DFCluster [], [])

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