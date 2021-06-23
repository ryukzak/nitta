{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module NITTA.LuaFrontendNew (
    parseLuaSources,

    -- * Internal
    AlgBuilder (..),
    Func (..),
    findStartupFunction,
    getLuaBlockFromSources,
    processStatement,
    lua2functionsNew,
) where

import Control.Monad.State
import qualified Data.HashMap.Strict as Map
import Data.String
import Data.String.ToString
import qualified Data.Text as T
import Language.Lua
import NITTA.Intermediate.DataFlow
import qualified NITTA.Intermediate.Functions as F
import NITTA.LuaFrontend
import NITTA.Utils.Base
import Data.Hashable

getUniqueLuaVariableName LuaValueVersion{luaValueVersionName, luaValueVersionAssignCount} luaValueAccessCount
    | T.head luaValueVersionName == '_' = fromText luaValueVersionName
    | otherwise = fromText $ luaValueVersionName <> "^" <> showText luaValueVersionAssignCount <> "#" <> showText luaValueAccessCount

data Func x = Func
    { fIn :: [T.Text]
    , fOut :: [LuaValueVersion]
    , fName :: String
    , fValues :: [x]
    , fInt :: [Int]
    }
    deriving (Show, Eq)

data LuaValueVersion = LuaValueVersion {
    luaValueVersionName :: T.Text,
    luaValueVersionAssignCount :: Int,
    luaValueVersionIsConstant :: Bool
} deriving (Show, Eq)

instance Hashable LuaValueVersion where
    hashWithSalt i LuaValueVersion{luaValueVersionName, luaValueVersionAssignCount, luaValueVersionIsConstant} =
        ((hashWithSalt i luaValueVersionName * 31)
        + hashWithSalt i luaValueVersionAssignCount * 31)
        + hashWithSalt i luaValueVersionIsConstant * 31


data AlgBuilder s t = AlgBuilder
    { algGraph :: [Func t]
    , algBuffer :: Map.HashMap T.Text LuaValueVersion
    , algVarGen :: Map.HashMap T.Text Int
    , algVars :: Map.HashMap LuaValueVersion [T.Text]
    , algStartupArgs :: Map.HashMap Int T.Text
    }
    deriving (Show)

funAssignStatements (FunAssign _ (FunBody _ _ (Block statements _))) = statements
funAssignStatements _ = error "funAssignStatements : not a function assignment"

--left part of lua statement
parseLeftExp (VarName (Name v)) = v
parseLeftExp _ = undefined

--right part of lua statement
parseRightExp ::  (MonadState (AlgBuilder s t7) m, Read t7) => T.Text -> Exp -> m T.Text
parseRightExp fOut (Binop ShiftL a (Number IntNum s)) = do
    varName <- parseExpArg fOut a
    algBuilder@AlgBuilder{algGraph} <- get
    put algBuilder{algGraph = Func{fIn = [varName], fOut = [], fValues = [], fName = "shiftL", fInt = [readText s]} : algGraph}
    return varName
parseRightExp fOut (Binop ShiftR a (Number IntNum s)) = do
    varName <- parseExpArg fOut a
    algBuilder@AlgBuilder{algGraph, algBuffer} <- get
    let (Just lvv) = getLuaValueByName fOut algBuffer
    put algBuilder{algGraph = Func{fIn = [varName], fOut = [lvv], fValues = [], fName = "shiftR", fInt = [readText s]} : algGraph}
    return varName
parseRightExp fOut (Number _ valueString) = do
    addVariable [] fOut [readText valueString] "constant" []
parseRightExp fOut (Binop op a b) = do
    varNameA <- parseExpArg fOut a
    varNameB <- parseExpArg fOut b
    addVariable [varNameA, varNameB] fOut [] (getBinopFuncName op) []
    where
        getBinopFuncName Add = "add"
        getBinopFuncName Sub = "sub"
        getBinopFuncName Mul = "multiply"
        getBinopFuncName Div = "divide"
        getBinopFuncName o = error $ "unknown binop: " ++ show o
parseRightExp fOut (PrefixExp (Paren e)) = parseRightExp fOut e
parseRightExp fOut (Unop Neg expr@(PrefixExp _)) = do
    varName <- parseExpArg fOut expr
    addVariable [varName] fOut [] "neg" []
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
        addFunction (fromText fname) fIn $ fromText fOut
        return $ head fIn
parseRightExp fOut (PrefixExp (PEVar (VarName (Name name)))) = do
    addAlias fOut name
parseRightExp _ expr = error $ "unknown expression : " <> show expr

parseExpArg _ n@(Number _ _) = do
    addConstant n
parseExpArg fOut expr@(Unop Neg _) = do
    name <- getNextTmpVarName fOut
    _ <- parseRightExp name expr
    addVariableAccess name
parseExpArg _ (PrefixExp (PEVar (VarName (Name name)))) = do
    addVariableAccess name
parseExpArg fOut binop@Binop{} = do
    name <- getNextTmpVarName fOut
    _ <- parseRightExp name binop
    addVariableAccess name
parseExpArg fOut (PrefixExp (Paren arg)) = parseExpArg fOut arg
parseExpArg fOut call@(PrefixExp (PEFunCall _)) = do
    name <- getNextTmpVarName fOut
    _ <- parseRightExp name call
    addVariableAccess name
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
    mapM_ (\(Name name, Number _ valueString) -> addToBuffer name valueString) $ zip names exps
    return ""
    where
        addToBuffer name _valueString = do
            algBuilder@AlgBuilder{algVars, algBuffer} <- get
            let value = LuaValueVersion{luaValueVersionName = name, luaValueVersionAssignCount = 0, luaValueVersionIsConstant = False}
            put algBuilder{algBuffer = Map.insert name value algBuffer, algVars = Map.insert value [] algVars}
            return value
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
    _ <- parseRightExp (parseLeftExp lexp) rexp
    return $ fromString ""
processStatement startupFunctionName (Assign vars exps) | length vars == length exps = do
    mapM_ (\(VarName (Name name), expr) -> processStatement startupFunctionName (Assign [VarName (Name (getTempAlias name))] [expr])) $ zip vars exps
    mapM_ (\(VarName (Name name)) -> addAlias name (getTempAlias name)) vars
    return $ fromString ""
    where
        getTempAlias name = name <> "&"
--startup function recursive call
processStatement fn (FunCall (NormalFunCall (PEVar (VarName (Name fName))) (Args args)))
    | fn == fName = do
        AlgBuilder{algStartupArgs} <- get
        let startupVarsNames = map ((\(Just x) -> x) . (`Map.lookup` algStartupArgs)) [0..(Map.size algStartupArgs)]
        let startupVarsVersions = map (\x -> LuaValueVersion{luaValueVersionName = x, luaValueVersionAssignCount = 0, luaValueVersionIsConstant = False}) startupVarsNames
        mapM_ parseStartupArg $ zip3 args startupVarsVersions [0..]
        return $ fromString ""
    where
        parseStartupArg (arg, valueVersion, index) = do
            varName <- parseExpArg (T.pack "loop") arg
            algBuilder@AlgBuilder{algGraph} <- get
            put algBuilder{algGraph = Func{fIn = [varName], fOut = [valueVersion], fValues = [index], fName = "loop", fInt = []} : algGraph}
            return $ fromString ""
processStatement _ (FunCall (NormalFunCall (PEVar (VarName (Name fName))) (Args args))) = do
    fIn <- mapM (parseExpArg "tmp") args
    addFunction (fromString $ T.unpack fName) fIn $ fromString ""
    return $ fromString ""
processStatement _ _ = undefined

addFunction :: (MonadState (AlgBuilder s t1) m) => [Char] -> [T.Text] -> T.Text -> m ()
addFunction funcName [i] fOut | toString funcName == "buffer" = do
    _ <- addVariable [i] fOut [] "buffer" []
    return ()
addFunction funcName [i] fOut | toString funcName == "brokenBuffer" = do
    _ <- addVariable [i] fOut [] "brokenBuffer" []
    return ()
addFunction funcName [i] _ | toString funcName == "send" = do
    algBuilder@AlgBuilder{algGraph} <- get
    put algBuilder{algGraph = Func{fIn = [i], fOut = [], fValues = [], fName = "send", fInt = []} : algGraph}
addFunction funcName _ fOut | toString funcName == "receive" = do
    _ <- addVariable [] fOut  [] "brokenBuffer" []
    return ()
addFunction fName _ _ = error $ "unknown function" <> fName

addConstant (Number _valueType valueString) = do
    algBuilder@AlgBuilder{algGraph, algVars} <- get
    let lvv = LuaValueVersion{luaValueVersionName = valueString, luaValueVersionAssignCount = 0, luaValueVersionIsConstant = True}
    case Map.lookup lvv algVars of
        Just value -> do
            let resultName = getUniqueLuaVariableName lvv (length value)
            put
                algBuilder
                    { algGraph = Func{fIn = [], fOut = [lvv], fValues = [readText valueString], fName = "constant", fInt = []} : algGraph
                    , algVars = Map.insert lvv ( resultName : value) algVars
                    }
            return resultName
        Nothing -> do
            let resultName = getUniqueLuaVariableName lvv 0
            put
                algBuilder
                    { algVars = Map.insert lvv [resultName] algVars }
            return resultName
addConstant _ = undefined

addVariable fIn fOut fValues fName fInt = do
    algBuilder@AlgBuilder{algGraph, algBuffer, algVars} <- get
    case getLuaValueByName fOut algBuffer of
        Just lvv@LuaValueVersion{luaValueVersionAssignCount} -> do
            let luaValueVersion = lvv{luaValueVersionAssignCount = luaValueVersionAssignCount + 1}
            let func = Func{fIn, fValues, fName, fInt, fOut = [luaValueVersion]}
            put
                algBuilder
                    { algGraph = func : algGraph
                    , algBuffer = Map.insert fOut luaValueVersion algBuffer
                    , algVars = Map.insert lvv [] algVars
                    }
            return $ getUniqueLuaVariableName luaValueVersion 0
        Nothing -> do
            let lvv = LuaValueVersion{luaValueVersionName = fOut, luaValueVersionAssignCount = 0, luaValueVersionIsConstant = False}
            let func = Func{fIn, fValues, fName, fInt, fOut = [lvv]}
            put
                algBuilder
                    { algGraph = func : algGraph
                    , algBuffer = Map.insert fOut lvv algBuffer
                    , algVars = Map.insert lvv [] algVars
                    }
            return $ getUniqueLuaVariableName lvv 0

addVariableAccess name = do
    algBuilder@AlgBuilder{algVars} <- get
    luaValueVersion <- getLatestLuaValueVersionByName name
    case Map.lookup luaValueVersion algVars of
        Just value -> do
            let len = length value
            let resultName = getUniqueLuaVariableName luaValueVersion len
            put algBuilder{algVars = Map.insert luaValueVersion (resultName : value) algVars}
            return resultName
        Nothing -> error ("variable '" ++ show (luaValueVersionName luaValueVersion) ++ " not found. Constants list : " ++ show algVars)

getLatestLuaValueVersionByName name = do
    AlgBuilder{algBuffer} <- get
    case Map.lookup name algBuffer of
        Just value -> return value
        Nothing -> error $ "variable not found : '" ++ show name ++ "'."

addAlias from to = do
    algBuilder@AlgBuilder{algBuffer} <- get
    case getLuaValueByName to algBuffer of
        Just value -> do
            put algBuilder{ algBuffer = Map.insert from value algBuffer }
            return $ fromString ""
        Nothing -> error ("variable '" ++ show to ++ " not found. Constants list : " ++ show algBuffer)

getLuaValueByName name buffer = Map.lookup name buffer

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
                , algVars = Map.empty
                , algStartupArgs = Map.empty
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

alg2graph AlgBuilder{algGraph, algBuffer, algVars} = flip execState (DFCluster []) $ do
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
        output v =
            case Map.lookup v algVars of
                Just names -> names
                _ -> error $ "variable not found : " <> show v <> ", buffer : " <> show algBuffer

--getBuilder src =
--    let syntaxTree = getLuaBlockFromSources src
--`    in buildAlg syntaxTree

lua2functionsNew src =
    FrontendResult{frDataFlow = parseLuaSources src, frTrace = [], frPrettyLog = undefined}

fromText t = fromString $ T.unpack t
readText t = read $ T.unpack t