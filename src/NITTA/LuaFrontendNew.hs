{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module NITTA.LuaFrontendNew (
    parseLuaSources,

    -- * Internal
    LuaValue (..),
    AlgBuilder (..),
    findStartupFunction,
    getLuaBlockFromSources,
    processStatement,
    lua2functionsNew,
) where

import Control.Monad.State
import qualified Data.Map as Map
import Data.String
import Data.String.ToString
import qualified Data.Text as T
import Language.Lua
import NITTA.Intermediate.DataFlow
import qualified NITTA.Intermediate.Functions as F
import NITTA.Intermediate.Types
import NITTA.LuaFrontend

data LuaValue s t
    = Constant {luaValueName :: T.Text, luaValueParsedFunction :: F s t, luaValueType :: NumberType, luaValueAccessCount :: Int}
    | Variable {luaValueName :: T.Text, luaValueParsedFunction :: F s t, luaValueAccessCount :: Int, luaValueAssignCount :: Int, isStartupArgument :: Bool, startupArgumentString :: T.Text}
    deriving (Show)

instance Eq (LuaValue s t) where
    (==) Constant{luaValueName = a} Constant{luaValueName = b} = a == b
    (==) Variable{luaValueName = a} Variable{luaValueName = b} = a == b
    (==) _ _ = False

instance Ord (LuaValue s t) where
    (<=) Constant{luaValueName = a} Constant{luaValueName = b} = a == b
    (<=) Constant{luaValueName = a} Variable{luaValueName = b} = a == b
    (<=) Variable{luaValueName = a} Constant{luaValueName = b} = a <= b
    (<=) Variable{luaValueName = a} Variable{luaValueName = b} = a <= b

data Func x = Func 
        { fIn :: [T.Text]
        , fOut :: [T.Text]
        , fName :: String
        , fValues :: [x]
        , fInt :: [Int]
        }

data AlgBuilder s t = AlgBuilder
    { algGraph :: [Func t]
    , algBuffer :: Map.Map T.Text (LuaValue s t)
    , algVarGen :: Map.Map T.Text Int
    }

funAssignStatements (FunAssign _ (FunBody _ _ (Block statements _))) = statements
funAssignStatements _ = error "funAssignStatements : not a function assignment"

--left part of lua statement
parseLeftExp (VarName (Name v)) = v
parseLeftExp _ = undefined

--right part of lua statement
parseRightExp fOut (Binop ShiftL a (Number IntNum s)) = do
    (a', _) <- parseExpArg fOut a
    algBuilder@AlgBuilder{algGraph} <- get
    put algBuilder{algGraph = Func{fIn = [a'], fOut = [fromString $ T.unpack fOut], fValues = [], fName = "shiftL", fInt = [readText s]} : algGraph}
    return $ fromString ""
parseRightExp fOut (Binop ShiftR a (Number IntNum s)) = do
    (a', _) <- parseExpArg fOut a
    algBuilder@AlgBuilder{algGraph} <- get
    put algBuilder{algGraph = Func{fIn = [a'], fOut = [fromString $ T.unpack fOut], fValues = [], fName = "shiftR", fInt = [readText s]} : algGraph}
    return $ fromString ""
parseRightExp fOut (Number _ valueString) = do
    name <- getFreeVariableName fOut
    addVariable fOut (F.constant (readText valueString) [name]) False $ fromString ""
parseRightExp fOut (Binop op a b) = do
    (a', _) <- parseExpArg fOut a
    (b', _) <- parseExpArg fOut b
    name <- getFreeVariableName fOut
    addVariable fOut (getBinopFunc op a' b' [name]) False ""
    where
        getBinopFunc Add a' b' resultName = F.add a' b' resultName
        getBinopFunc Sub a' b' resultName = F.sub a' b' resultName
        getBinopFunc Mul a' b' resultName = F.multiply a' b' resultName
        getBinopFunc Div a' b' resultName = F.division a' b' [head resultName] (tail resultName)
        getBinopFunc o _ _ _ = error $ "unknown binop: " ++ show o
parseRightExp fOut (PrefixExp (Paren e)) = parseRightExp fOut e
parseRightExp fOut (Unop Neg expr@(PrefixExp _)) = do
    (expr', _) <- parseExpArg fOut expr
    name <- getFreeVariableName fOut
    addVariable fOut (F.neg expr' [name]) False $ fromString ""
parseRightExp
    fOut
    ( PrefixExp
            ( PEFunCall
                    ( NormalFunCall
                            (PEVar (VarName (Name fname)))
                            (Args args)
                        )
                )
        ) = do
        fIn <- mapM (parseExpArg fOut) args
        addFunction (fromString $ T.unpack fname) (map fst fIn) $ fromString $ T.unpack fOut
        return $ fromString $ T.unpack fOut
parseRightExp _ _ = undefined

parseExpArg _ n@(Number _ _) = do
    addConstant n
parseExpArg fOut (Unop Neg n) = do
    c <- getNextTmpVarName fOut
    (name, luaValue) <- parseExpArg c n
    algBuilder@AlgBuilder{algGraph} <- get
    put algBuilder{algGraph = Func{fIn = [name], fOut = [fromText c], fValues = [], fName = "neg", fInt = []} : algGraph}
    return (fromString $ T.unpack c, luaValue)
parseExpArg _ (PrefixExp (PEVar (VarName (Name name)))) = do
    addVariableAccess name
parseExpArg fOut binop@Binop{} = do
    name <- getNextTmpVarName fOut
    _ <- parseRightExp name binop
    _ <- addVariableAccess name
    return (fromString $ T.unpack name, "")
parseExpArg fOut (PrefixExp (Paren arg)) = parseExpArg fOut arg
parseExpArg fOut call@(PrefixExp (PEFunCall _)) = do
    c <- getNextTmpVarName fOut
    varName <- parseRightExp c call
    return (fromString varName, "")
parseExpArg _ _ = undefined

getNextTmpVarName fOut
    | T.isInfixOf "#" fOut = getNextTmpVarName (T.splitOn "#" fOut !! 1)
    | otherwise = do
        algBuilder@AlgBuilder{algVarGen} <- get
        case Map.lookup fOut algVarGen of
            Just value -> do
                put algBuilder{algVarGen = Map.insert fOut (value + 1) algVarGen}
                return $ T.pack $ "_" <> show value <> "#" <> T.unpack fOut
            Nothing -> do
                put algBuilder{algVarGen = Map.insert fOut 1 algVarGen}
                return $ T.pack $ "_0#" <> T.unpack fOut
addStartupFuncArgs (FunCall (NormalFunCall _ (Args exps))) (FunAssign _ (FunBody names _ _)) = do
    mapM_ (\(Name name, Number _ valueString) -> addVariable name (F.constant (readText valueString) [fromText (name <> "^0#0")]) True valueString) $ zip names exps
    return ""
addStartupFuncArgs _ _ = undefined

--Lua language Stat structure parsing
--LocalAssign
processStatement _ (LocalAssign _names Nothing) = do
    return $ fromString ""
processStatement fn (LocalAssign names (Just exps)) =
    processStatement fn $ Assign (map VarName names) exps
--Assign
processStatement fn (Assign lexps@[_] [Unop Neg (Number ntype ntext)]) =
    processStatement fn (Assign lexps [Number ntype ("-" <> ntext)])
processStatement _ (Assign [lexp] [rexp]) = do
    parseRightExp (parseLeftExp lexp) rexp
processStatement startupFunctionName (Assign vars exps) | length vars == length exps = do
    mapM_ (\(var, expr) -> processStatement startupFunctionName (Assign [var] [expr])) $ zip vars exps
    return $ fromString ""
--startup function recursive call
processStatement fn (FunCall (NormalFunCall (PEVar (VarName (Name fName))) (Args args)))
    | fn == fName = do
        AlgBuilder{algBuffer} <- get
        let startupVars = filter (\case Variable{isStartupArgument} -> isStartupArgument; _ -> False) $ map snd $ Map.toList algBuffer
        mapM_ parseStartupArg $ zip args startupVars
        return $ fromString ""
    where
        parseStartupArg (arg, value) = do
            (varName, _) <- parseExpArg (T.pack "loop") arg
            algBuilder@AlgBuilder{algGraph} <- get
            put algBuilder{algGraph = Func{fIn = [varName], fOut = [getDefaultName value], fValues = [readText $ startupArgumentString value], fName = "loop", fInt = []} : algGraph}
            return $ fromString ""
        getDefaultName Variable{luaValueName} = fromString $ T.unpack luaValueName ++ "^0#0"
        getDefaultName _ = undefined
processStatement _ (FunCall (NormalFunCall (PEVar (VarName (Name fName))) (Args args))) = do
    fIn <- mapM (parseExpArg "tmp") args
    addFunction (fromString $ T.unpack fName) (map fst fIn) $ fromString ""
    return $ fromString ""
processStatement _ _ = undefined

addFunction funcName [i] fOut | toString funcName == "buffer" = do
    algBuilder@AlgBuilder{algGraph} <- get
    put algBuilder{algGraph = Func{fIn = [i], fOut = [fOut], fValues = [], fName = "buffer", fInt = []} : algGraph}
addFunction funcName [i] fOut | toString funcName == "brokenBuffer" = do
    algBuilder@AlgBuilder{algGraph} <- get
    put algBuilder{algGraph = Func{fIn = [i], fOut = [fOut], fValues = [], fName = "brokenBuffer", fInt = []} : algGraph}
addFunction funcName [i] _ | toString funcName == "send" = do
    algBuilder@AlgBuilder{algGraph} <- get
    put algBuilder{algGraph = Func{fIn = [i], fOut = [], fValues = [], fName = "send", fInt = []} : algGraph}
addFunction funcName _ fOut | toString funcName == "receive" = do
    algBuilder@AlgBuilder{algGraph} <- get
    put algBuilder{algGraph = Func{fIn = [], fOut = [fOut], fValues = [], fName = "receive", fInt = []} : algGraph}
addFunction fName _ _ = error $ "unknown function" <> fName

addConstant (Number valueType valueString) = do
    algBuilder@AlgBuilder{algGraph, algBuffer} <- get
    case Map.lookup valueString algBuffer of
        Just value -> do
            let constant = F.constant (readText valueString) [fromString (getConstantName valueString (luaValueAccessCount value))]
            let newValue = updateConstant value constant
            put algBuilder{algGraph = Func{fIn = [], fOut = [fromText valueString], fValues = [readText valueString], fName = "constant", fInt = []} : algGraph
                        ,  algBuffer = Map.insert valueString newValue algBuffer}
            return (fromString $ getConstantName (luaValueName newValue) (luaValueAccessCount newValue), luaValueName newValue)
        Nothing -> do
            let constant = F.constant (readText valueString) [fromString (getConstantName valueString (0 :: Int))]
            let value = Constant{luaValueName = valueString, luaValueParsedFunction = constant, luaValueType = valueType, luaValueAccessCount = 0}
            put algBuilder{algGraph = Func{fIn = [], fOut = [fromText valueString], fValues = [readText valueString], fName = "constant", fInt = []} : algGraph
                        ,  algBuffer = Map.insert valueString value algBuffer}
            return (fromString $ getConstantName (luaValueName value) (luaValueAccessCount value), luaValueName value)
    where
        updateConstant c@Constant{luaValueAccessCount} constant = c{luaValueParsedFunction = constant, luaValueAccessCount = luaValueAccessCount + 1}
        updateConstant _ _ = undefined
        getConstantName luaValueName luaValueAccessCount = "!" ++ T.unpack luaValueName ++ "#" ++ show luaValueAccessCount
addConstant _ = undefined

getFreeVariableName name
    | T.head name == '_' = return $ fromString $ T.unpack name
    | otherwise = do
        AlgBuilder{algBuffer} <- get
        case Map.lookup name algBuffer of
            Just value -> do
                return $ getVariableName name (luaValueAccessCount value) (luaValueAssignCount value)
            Nothing -> do
                return $ getVariableName name (0 :: Integer) (0 :: Integer)
    where
        getVariableName luaValueName luaValueAccessCount luaValueAssignCount = fromString $ T.unpack luaValueName ++ "^" ++ show luaValueAssignCount ++ "#" ++ show luaValueAccessCount

addVariable name func isStartupArg startupArgString = do
    algBuilder@AlgBuilder{algBuffer} <- get
    case Map.lookup name algBuffer of
        Just value -> do
            let newValue = updateConstant value
            put algBuilder{algBuffer = Map.insert name newValue algBuffer}
            return $ getVariableName newValue
        Nothing -> do
            let value = Variable{luaValueName = name, luaValueParsedFunction = func, luaValueAccessCount = 0, luaValueAssignCount = 0, isStartupArgument = isStartupArg, startupArgumentString = startupArgString}
            put algBuilder{algBuffer = Map.insert name value algBuffer}
            return $ getVariableName value
    where
        updateConstant var@Variable{luaValueAssignCount} =
            var{luaValueParsedFunction = func, luaValueAssignCount = luaValueAssignCount + 1, isStartupArgument = isStartupArg, startupArgumentString = startupArgString}
        updateConstant _ = undefined
        getVariableName Variable{luaValueName, luaValueAccessCount, luaValueAssignCount} = fromString $ T.unpack luaValueName ++ "^" ++ show luaValueAssignCount ++ "#" ++ show luaValueAccessCount
        getVariableName _ = undefined

addVariableAccess name = do
    algBuilder@AlgBuilder{algBuffer} <- get
    case Map.lookup name algBuffer of
        Just value -> do
            let newValue = updateVariable value
            let oldName = getVariableName value
            put algBuilder{algBuffer = Map.insert name newValue algBuffer}
            return (fromString oldName, startupArgumentString value)
        Nothing -> error ("variable '" ++ show name ++ " not found. Constants list : " ++ show algBuffer)
    where
        updateVariable Variable{luaValueName, luaValueAccessCount, luaValueAssignCount, isStartupArgument, luaValueParsedFunction, startupArgumentString} =
            Variable{luaValueName = luaValueName, luaValueParsedFunction = luaValueParsedFunction, luaValueAccessCount = luaValueAccessCount + 1, luaValueAssignCount = luaValueAssignCount, isStartupArgument = isStartupArgument, startupArgumentString = startupArgumentString}
        updateVariable _ = undefined
        getVariableName Variable{luaValueName, luaValueAccessCount, luaValueAssignCount} = fromString $ T.unpack luaValueName ++ "^" ++ show luaValueAssignCount ++ "#" ++ show luaValueAccessCount
        getVariableName _ = undefined

buildAlg syntaxTree =
    flip execState st $ do
        _ <- addStartupFuncArgs startupFunctionCall startupFunctionDef
        mapM_ (processStatement startupFunctionName) $ funAssignStatements startupFunctionDef
    where
        (startupFunctionName, startupFunctionCall, startupFunctionDef) = findStartupFunction syntaxTree
        st =
            AlgBuilder
                { algGraph = []
                , algBuffer = Map.empty
                , algVarGen = Map.empty
                }

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
     in alg2graph $ buildAlg syntaxTree

alg2graph AlgBuilder{algGraph, algBuffer} = flip execState (DFCluster []) $ do 
    mapM addToGraph algGraph
    where
        addToGraph item = do
            kek <- get 
            put (addFuncToDataFlowGraph kek $ function2nitta item)
            return $ fromString ""
        function2nitta Func{fName = "buffer", fIn = [i], fOut = [o], fValues = [], fInt = []} = F.buffer (fromText i) $ output o
        function2nitta Func{fName = "brokenBuffer", fIn = [i], fOut = [o], fValues = [], fInt = []} = F.brokenBuffer (fromText i) $ output o
        function2nitta Func{fName = "constant", fIn = [], fOut = [o], fValues = [x], fInt = []} = F.constant x $ output o
        function2nitta Func{fName = "send", fIn = [i], fOut = [], fValues = [], fInt = []} = F.send (fromText i)
        function2nitta Func{fName = "add", fIn = [a, b], fOut = [c], fValues = [], fInt = []} = F.add (fromText a) (fromText b) $ output c
        function2nitta Func{fName = "sub", fIn = [a, b], fOut = [c], fValues = [], fInt = []} = F.sub (fromText a) (fromText b) $ output c
        function2nitta Func{fName = "multiply", fIn = [a, b], fOut = [c], fValues = [], fInt = []} = F.multiply (fromText a) (fromText b) $ output c
        function2nitta Func{fName = "divide", fIn = [d, n], fOut = [q, r], fValues = [], fInt = []} = F.division (fromText d) (fromText n) (output q) (output r)
        function2nitta Func{fName = "neg", fIn = [i], fOut = [o], fValues = [], fInt = []} = F.neg (fromText i) $ output o
        function2nitta Func{fName = "receive", fIn = [], fOut = [o], fValues = [], fInt = []} = F.receive $ output o
        function2nitta Func{fName = "shiftL", fIn = [a], fOut = [c], fValues = [], fInt = [s]} = F.shiftL s (fromText a) $ output c
        function2nitta Func{fName = "shiftR", fIn = [a], fOut = [c], fValues = [], fInt = [s]} = F.shiftR s (fromText a) $ output c
        function2nitta Func{fName = "loop", fIn = [a], fOut = [c], fValues = [x], fInt = []} = F.loop x (fromText a) $ output c
        function2nitta Func{fName} = error $ "function not found: " ++ show fName
        output v
            | T.head v == '_' = []
            | otherwise =
                case Map.lookup v algBuffer of 
                    Just Variable{luaValueName, luaValueAccessCount, luaValueAssignCount} -> 
                        [ fromString (combineName luaValueName luaValueAssignCount i) | i <- [0 .. luaValueAccessCount]] 
                    Just Constant{luaValueName, luaValueAccessCount} -> 
                        [ fromString ("!" <> T.unpack luaValueName <> "#" <> show i) | i <- [0 .. luaValueAccessCount]] 
                    _ -> error $ "variable not found : " <> show v <> ", buffer : " <> show algBuffer
        combineName name assignCount accessCount = T.unpack name ++ "^" ++ show assignCount ++ "#" ++ show accessCount


getBuilder src =
    let syntaxTree = getLuaBlockFromSources src
     in buildAlg syntaxTree

lua2functionsNew src =
    FrontendResult{frDataFlow = parseLuaSources src, frTrace = [], frPrettyLog = undefined}

fromText t = fromString $ T.unpack t 
readText t = read $ T.unpack t 

