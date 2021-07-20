{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module NITTA.LuaFrontendNew (
    lua2functionsNew,

    -- * Internal
    AlgBuilder (..),
    Func (..),
    LuaValueVersion (..),
    findStartupFunction,
    getLuaBlockFromSources,
    processStatement
) where

import Control.Monad.State
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import Data.Hashable
import Data.String
import Data.Maybe
import Data.String.ToString
import qualified Data.Text as T
import Language.Lua
import NITTA.Intermediate.DataFlow
import qualified NITTA.Intermediate.Functions as F
import NITTA.LuaFrontend
import NITTA.Utils.Base
import Text.Printf

getUniqueLuaVariableName LuaValueVersion{luaValueVersionName, luaValueVersionAssignCount, luaValueVersionIsConstant} luaValueAccessCount
    | luaValueVersionIsConstant = fromText $ "!" <> luaValueVersionName <> "#" <> showText luaValueAccessCount
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

data LuaValueVersion = LuaValueVersion
    { luaValueVersionName :: T.Text
    , luaValueVersionAssignCount :: Int
    , luaValueVersionIsConstant :: Bool
    }
    deriving (Show, Eq)

instance Hashable LuaValueVersion where
    hashWithSalt i LuaValueVersion{luaValueVersionName, luaValueVersionAssignCount, luaValueVersionIsConstant} =
        ( (hashWithSalt i luaValueVersionName * 31)
            + hashWithSalt i luaValueVersionAssignCount * 31
        )
            + hashWithSalt i luaValueVersionIsConstant * 31

data AlgBuilder s t = AlgBuilder
    { algGraph :: [Func t]
    , algBuffer :: HashMap.HashMap T.Text LuaValueVersion
    , algVarGen :: HashMap.HashMap T.Text Int
    , algVars :: HashMap.HashMap LuaValueVersion [T.Text]
    , algStartupArgs :: HashMap.HashMap Int (T.Text, T.Text)
    , algTraceFuncs :: [([T.Text], T.Text)]
    }
    deriving (Show)

defaultFmt = "%.3f"

funAssignStatements (FunAssign _ (FunBody _ _ (Block statements _))) = statements
funAssignStatements _ = error "funAssignStatements : not a function assignment"

--left part of lua statement
parseLeftExp (VarName (Name v)) = v
parseLeftExp _ = undefined

--right part of lua statement
parseRightExp [fOut] (Binop ShiftL a (Number IntNum s)) = do
    varName <- parseExpArg fOut a
    addVariable [varName] [fOut] [] "shiftL" [readText s]
parseRightExp [fOut] (Binop ShiftR a (Number IntNum s)) = do
    varName <- parseExpArg fOut a
    addVariable [varName] [fOut] [] "shiftR" [readText s]
parseRightExp [fOut] (Number _ valueString) = do
    addVariable [] [fOut] [readText valueString] "constant" []
parseRightExp fOut@(x : _) (Binop op a b) = do
    varNameA <- parseExpArg x a
    varNameB <- parseExpArg x b
    addVariable [varNameA, varNameB] fOut [] (getBinopFuncName op) []
    where
        getBinopFuncName Add = "add"
        getBinopFuncName Sub = "sub"
        getBinopFuncName Mul = "multiply"
        getBinopFuncName Div = "divide"
        getBinopFuncName o = error $ "unknown binop: " ++ show o
parseRightExp fOut (PrefixExp (Paren e)) = parseRightExp fOut e
parseRightExp fOut (Unop Neg (Number numType name)) = parseRightExp fOut (Number numType ("-" <> name))
parseRightExp [fOut] (Unop Neg expr@(PrefixExp _)) = do
    varName <- parseExpArg fOut expr
    addVariable [varName] [fOut] [] "neg" []
parseRightExp
    [fOut]
    ( PrefixExp
            ( PEFunCall
                    ( NormalFunCall
                            (PEVar (VarName (Name fname)))
                            (Args args)
                        )
                )
        ) = do
        fIn <- mapM (parseExpArg fOut) args
        addFunction (fromText fname) fIn [fromText fOut]
        return $ head fIn
parseRightExp [fOut] (PrefixExp (PEVar (VarName (Name name)))) = do
    addAlias fOut name
parseRightExp _ expr = error $ "unknown expression : " <> show expr

parseExpArg _ n@(Number _ _) = do
    addConstant n
parseExpArg fOut expr@(Unop Neg _) = do
    name <- getNextTmpVarName fOut
    _ <- parseRightExp [name] expr
    addVariableAccess name
parseExpArg _ (PrefixExp (PEVar (VarName (Name name)))) = do
    addVariableAccess name
parseExpArg fOut binop@Binop{} = do
    name <- getNextTmpVarName fOut
    _ <- parseRightExp [name] binop
    addVariableAccess name
parseExpArg fOut (PrefixExp (Paren arg)) = parseExpArg fOut arg
parseExpArg fOut call@(PrefixExp (PEFunCall _)) = do
    name <- getNextTmpVarName fOut
    _ <- parseRightExp [name] call
    addVariableAccess name
parseExpArg _ _ = undefined

getNextTmpVarName fOut
    | T.isInfixOf "#" fOut = getNextTmpVarName (T.splitOn "#" fOut !! 1)
    | otherwise = do
        algBuilder@AlgBuilder{algVarGen} <- get
        case HashMap.lookup fOut algVarGen of
            Just value -> do
                put algBuilder{algVarGen = HashMap.insert fOut (value + 1) algVarGen}
                return $ T.pack $ "_" <> show value <> "#" <> T.unpack fOut
            Nothing -> do
                put algBuilder{algVarGen = HashMap.insert fOut 1 algVarGen}
                return $ T.pack $ "_0#" <> T.unpack fOut

addStartupFuncArgs (FunCall (NormalFunCall _ (Args exps))) (FunAssign _ (FunBody names _ _)) = do
    mapM_ (\(Name name, Number _ valueString, serialNumber) -> addToBuffer name valueString serialNumber) $ zip3 names exps [0 ..]
    return ""
    where
        addToBuffer name valueString serialNumber = do
            algBuilder@AlgBuilder{algVars, algBuffer, algStartupArgs} <- get
            let value = LuaValueVersion{luaValueVersionName = name, luaValueVersionAssignCount = 0, luaValueVersionIsConstant = False}
            put algBuilder{algBuffer = HashMap.insert name value algBuffer, algVars = HashMap.insert value [] algVars, algStartupArgs = HashMap.insert serialNumber (name, valueString) algStartupArgs}
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
processStatement _ (Assign lexp [rexp]) = do
    _ <- parseRightExp (map parseLeftExp lexp) rexp
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
        let startupVarsNames = map ((\(Just x) -> x) . (`HashMap.lookup` algStartupArgs)) [0 .. (HashMap.size algStartupArgs)]
        let startupVarsVersions = map (\x -> LuaValueVersion{luaValueVersionName = fst x, luaValueVersionAssignCount = 0, luaValueVersionIsConstant = False}) startupVarsNames
        mapM_ parseStartupArg $ zip3 args startupVarsVersions (map (readText . snd) startupVarsNames)
        return $ fromString ""
    where
        parseStartupArg (arg, valueVersion, index) = do
            varName <- parseExpArg (T.pack "loop") arg
            algBuilder@AlgBuilder{algGraph} <- get
            put algBuilder{algGraph = Func{fIn = [varName], fOut = [valueVersion], fValues = [index], fName = "loop", fInt = []} : algGraph}
            return $ fromString ""
processStatement _ (FunCall (NormalFunCall (PEVar (VarName (Name fName))) (Args args))) = do
    fIn <- mapM (parseExpArg "tmp") args
    addFunction (fromString $ T.unpack fName) fIn [fromString ""]
    return $ fromString ""
processStatement _fn (FunCall (NormalFunCall (PEVar (SelectName (PEVar (VarName (Name "debug"))) (Name fName))) (Args args))) = do
    fIn <- mapM (parseExpArg "debug") args
    algBuilder@AlgBuilder{algTraceFuncs} <- get
    case (fName, fIn) of
        ("trace", tFmt : vs)
            | T.isPrefixOf "\"" tFmt && T.isPrefixOf "\"" tFmt -> do
                put algBuilder{algTraceFuncs = (vs, T.replace "\"" "" tFmt) : algTraceFuncs}
        ("trace", vs) -> do
                put algBuilder{algTraceFuncs = (vs, defaultFmt) : algTraceFuncs}
        _ -> error $ "unknown debug method: " ++ show fName ++ " " ++ show args
    return ""
processStatement _ _stat = error $ "unknown statement: " <> show _stat

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
    _ <- addVariable [] fOut [] "receive" []
    return ()
addFunction fName _ _ = error $ "unknown function" <> fName

addConstant (Number _valueType valueString) = do
    algBuilder@AlgBuilder{algGraph, algVars} <- get
    let lvv = LuaValueVersion{luaValueVersionName = valueString, luaValueVersionAssignCount = 0, luaValueVersionIsConstant = True}
    case HashMap.lookup lvv algVars of
        Just value -> do
            let resultName = getUniqueLuaVariableName lvv (length value)
            put algBuilder { algVars = HashMap.insert lvv (resultName : value) algVars }
            return resultName
        Nothing -> do
            let resultName = getUniqueLuaVariableName lvv 0
            put
                algBuilder
                    { algGraph = Func{fIn = [], fOut = [lvv], fValues = [readText valueString], fName = "constant", fInt = []} : algGraph
                    , algVars = HashMap.insert lvv [resultName] algVars
                    }
            return resultName
addConstant _ = undefined

addVariable :: (MonadState (AlgBuilder s t) m, IsString b) => [T.Text] -> [T.Text] -> [t] -> [Char] -> [Int] -> m b
addVariable fIn fOut fValues fName fInt = do
    AlgBuilder{algBuffer} <- get
    let luaValueVersions = map (\x -> nameToLuaValueVersion algBuffer x) fOut
    let func = Func{fIn, fValues, fName, fInt, fOut = luaValueVersions}
    mapM_ (uncurry addItemToBuffer) $ zip fOut luaValueVersions
    mapM_ addItemToVars luaValueVersions
    algBuilder@AlgBuilder{algGraph} <- get
    put algBuilder{algGraph = func : algGraph}
    return $ getUniqueLuaVariableName (head luaValueVersions) 0
    where
        nameToLuaValueVersion algBuffer name =
            case getLuaValueByName name algBuffer of
                Just lvv@LuaValueVersion{luaValueVersionAssignCount} -> lvv{luaValueVersionAssignCount = luaValueVersionAssignCount + 1}
                Nothing -> LuaValueVersion{luaValueVersionName = name, luaValueVersionAssignCount = 0, luaValueVersionIsConstant = False}
        addItemToBuffer name lvv = do
            algBuilder@AlgBuilder{algBuffer} <- get
            put algBuilder{algBuffer = HashMap.insert name lvv algBuffer}
        addItemToVars name = do
            algBuilder@AlgBuilder{algVars} <- get
            put algBuilder{algVars = HashMap.insert name [] algVars}

addVariableAccess name = do
    algBuilder@AlgBuilder{algVars} <- get
    luaValueVersion <- getLatestLuaValueVersionByName name
    case HashMap.lookup luaValueVersion algVars of
        Just value -> do
            let len = length value
            let resultName = getUniqueLuaVariableName luaValueVersion len
            put algBuilder{algVars = HashMap.insert luaValueVersion (resultName : value) algVars}
            return resultName
        Nothing -> error ("variable '" ++ show (luaValueVersionName luaValueVersion) ++ " not found. Constants list : " ++ show algVars)

getLatestLuaValueVersionByName name = do
    AlgBuilder{algBuffer} <- get
    case HashMap.lookup name algBuffer of
        Just value -> return value
        Nothing -> error $ "variable not found : '" ++ show name ++ "'."

addAlias from to = do
    algBuilder@AlgBuilder{algBuffer} <- get
    case getLuaValueByName to algBuffer of
        Just value -> do
            put algBuilder{algBuffer = HashMap.insert from value algBuffer}
            return $ fromString ""
        Nothing -> error ("variable '" ++ show to ++ " not found. Constants list : " ++ show algBuffer)

getLuaValueByName name buffer = HashMap.lookup name buffer

buildAlg syntaxTree =
    flip execState st $ do
        _ <- addStartupFuncArgs startupFunctionCall startupFunctionDef
        mapM_ (processStatement startupFunctionName) $ funAssignStatements startupFunctionDef
    where
        (startupFunctionName, startupFunctionCall, startupFunctionDef) = findStartupFunction syntaxTree
        st =
            AlgBuilder
                { algGraph = []
                , algBuffer = HashMap.empty
                , algVarGen = HashMap.empty
                , algVars = HashMap.empty
                , algStartupArgs = HashMap.empty
                , algTraceFuncs = []
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
        function2nitta f = error $ "function not found: " ++ show f
        output v =
            case HashMap.lookup v algVars of
                Just names -> map fromText names
                _ -> error $ "variable not found : " <> show v <> ", buffer : " <> show algBuffer

lua2functionsNew src =
    let syntaxTree = getLuaBlockFromSources src
        algBuilder = buildAlg syntaxTree
        frTrace = getFrTrace $ algTraceFuncs algBuilder
     in 
        FrontendResult{frDataFlow = alg2graph algBuilder, frTrace = frTrace, frPrettyLog = prettyLog frTrace}


getFrTrace traceFuncs = [ TraceVar fmt var | (vars, fmt) <- traceFuncs, var <- vars ]
                    
prettyLog traceVars hms = map prettyHM hms
    where
        prettyHM hm = HashMap.fromList $ map (fromMaybe undefined) $ filter isJust $ map prettyX $ HashMap.toList hm
        prettyX (v0, x) = do
            -- variables names end on #0, #1..., so we trim this suffix
            let v = takeWhile (/= '#') $ toString v0
            fmt <- v2fmt Map.!? v
            Just (toString v, printx (T.unpack fmt) x)
        v2fmt = Map.fromList $ map (\(TraceVar fmt v) -> (toString v, fmt)) traceVars
        printx p x
            | 'f' `elem` p = printf p (fromRational (toRational x) :: Double)
            | 's' `elem` p = printf p $ show x
            | otherwise = printf p x

fromText t = fromString $ T.unpack t
readText t = read $ T.unpack t
