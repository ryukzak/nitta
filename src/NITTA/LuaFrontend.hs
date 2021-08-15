{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

module NITTA.LuaFrontend (
    lua2functions,
    FrontendResult (..),
    TraceVar (..),

    -- * Internal
    LuaAlgBuilder (..),
    LuaStatement (..),
    LuaValueVersion (..),
    findStartupFunction,
    getLuaBlockFromSources,
    processStatement,
) where

import Control.Monad.State
import qualified Data.HashMap.Strict as HM
import Data.Hashable
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.String
import Data.String.ToString
import qualified Data.Text as T
import Language.Lua
import NITTA.Intermediate.DataFlow
import qualified NITTA.Intermediate.Functions as F
import NITTA.Utils.Base
import Text.Printf

getUniqueLuaVariableName LuaValueVersion{ luaValueVersionName, luaValueVersionIsConstant=True } luaValueAccessCount = "!" <> luaValueVersionName <> "#" <> showText luaValueAccessCount
getUniqueLuaVariableName LuaValueVersion{ luaValueVersionName, luaValueVersionAssignCount} luaValueAccessCount
    | T.head luaValueVersionName == '_' = luaValueVersionName
    | otherwise = luaValueVersionName <> "^" <> showText luaValueVersionAssignCount <> "#" <> showText luaValueAccessCount

data LuaStatement x = LuaStatement
    { fIn :: [T.Text]
    , fOut :: [LuaValueVersion]
    , fName :: T.Text
    , fValues :: [x]
    , fInt :: [Int]
    }
    deriving (Show, Eq)

-- | Stores information about a particular version of a variable. The version of a variable changes after assigning a new value to it.
data LuaValueVersion = LuaValueVersion
    { luaValueVersionName :: T.Text
    , luaValueVersionAssignCount :: Int
    , luaValueVersionIsConstant :: Bool
    }
    deriving (Show, Eq)

data FrontendResult v x = FrontendResult
    { frDataFlow :: DataFlowGraph v x
    , frTrace :: [TraceVar]
    , frPrettyLog :: [HM.HashMap v x] -> [HM.HashMap String String]
    }

data TraceVar = TraceVar {tvFmt, tvVar :: T.Text}
    deriving (Show)

instance Hashable LuaValueVersion where
    hashWithSalt i LuaValueVersion{luaValueVersionName, luaValueVersionAssignCount, luaValueVersionIsConstant} =
        ( (hashWithSalt i luaValueVersionName * 31)
            + hashWithSalt i luaValueVersionAssignCount * 31
        )
            + hashWithSalt i luaValueVersionIsConstant * 31

data LuaAlgBuilder s t = LuaAlgBuilder
    { -- | A list containing all expressions to be added to the final graph.
      algGraph :: [LuaStatement t]
    , -- | A table that maps a variable name to the most recent corresponding LuaValueVersion.
      algLatestLuaValueVersion :: HM.HashMap T.Text LuaValueVersion
    , -- | A table needed to generate unique temporary variable names.
      algVarCounters :: HM.HashMap T.Text Int
    , -- | A table lists all uses of a particular LuaValueVersion.
      algVars :: HM.HashMap LuaValueVersion [T.Text]
    , -- | A table correlating the ordinal number of an argument with a variable storing its value and startup value of this variable.
      algStartupArgs :: HM.HashMap Int (T.Text, T.Text)
    , -- | A list that stores debug information about monitored variables and their display formats.
      algTraceFuncs :: [([T.Text], T.Text)]
    }
    deriving (Show)

defaultFmt = "%.3f"

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
        addFunction fname fIn [fOut]
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
        luaAlgBuilder@LuaAlgBuilder{algVarCounters} <- get
        case HM.lookup fOut algVarCounters of
            Just value -> do
                put luaAlgBuilder{algVarCounters = HM.insert fOut (value + 1) algVarCounters}
                return $ "_" <> showText value <> "#" <> fOut
            Nothing -> do
                put luaAlgBuilder{algVarCounters = HM.insert fOut 1 algVarCounters}
                return $ "_0#" <> fOut

addStartupFuncArgs (FunCall (NormalFunCall _ (Args exps))) (FunAssign _ (FunBody names _ _)) = do
    mapM_ (\(Name name, Number _ valueString, serialNumber) -> addToBuffer name valueString serialNumber) $ zip3 names exps [0 ..]
    return ""
    where
        addToBuffer name valueString serialNumber = do
            luaAlgBuilder@LuaAlgBuilder{algVars, algLatestLuaValueVersion, algStartupArgs} <- get
            let value = LuaValueVersion{luaValueVersionName = name, luaValueVersionAssignCount = 0, luaValueVersionIsConstant = False}
            put luaAlgBuilder{algLatestLuaValueVersion = HM.insert name value algLatestLuaValueVersion, algVars = HM.insert value [] algVars, algStartupArgs = HM.insert serialNumber (name, valueString) algStartupArgs}
            return value
addStartupFuncArgs _ _ = undefined

--Lua language Stat structure parsing
--LocalAssign
processStatement _ (LocalAssign _names Nothing) = do
    return ()
processStatement fn (LocalAssign names (Just exps)) =
    processStatement fn $ Assign (map VarName names) exps
--Assign
processStatement fn (Assign lexps@[_] [Unop Neg (Number ntype ntext)]) =
    processStatement fn (Assign lexps [Number ntype ("-" <> ntext)])
processStatement _ (Assign lexp [rexp]) = do
    parseRightExp (map parseLeftExp lexp) rexp
processStatement startupFunctionName (Assign vars exps) | length vars == length exps = do
    mapM_ (\(VarName (Name name), expr) -> processStatement startupFunctionName (Assign [VarName (Name (getTempAlias name))] [expr])) $ zip vars exps
    mapM_ (\(VarName (Name name)) -> addAlias name (getTempAlias name)) vars
    where
        getTempAlias name = name <> "&"
--startup function recursive call
processStatement fn (FunCall (NormalFunCall (PEVar (VarName (Name fName))) (Args args)))
    | fn == fName = do
        LuaAlgBuilder{algStartupArgs} <- get
        let startupVarsNames = map ((\(Just x) -> x) . (`HM.lookup` algStartupArgs)) [0 .. (HM.size algStartupArgs)]
        let startupVarsVersions = map (\x -> LuaValueVersion{luaValueVersionName = fst x, luaValueVersionAssignCount = 0, luaValueVersionIsConstant = False}) startupVarsNames
        mapM_ parseStartupArg $ zip3 args startupVarsVersions (map (readText . snd) startupVarsNames)
    where
        parseStartupArg (arg, valueVersion, index) = do
            varName <- parseExpArg (T.pack "loop") arg
            luaAlgBuilder@LuaAlgBuilder{algGraph} <- get
            put luaAlgBuilder{algGraph = LuaStatement{fIn = [varName], fOut = [valueVersion], fValues = [index], fName = "loop", fInt = []} : algGraph}
processStatement _ (FunCall (NormalFunCall (PEVar (VarName (Name fName))) (Args args))) = do
    fIn <- mapM (parseExpArg "tmp") args
    addFunction (fromString $ T.unpack fName) fIn [fromString ""]
processStatement _fn (FunCall (NormalFunCall (PEVar (SelectName (PEVar (VarName (Name "debug"))) (Name fName))) (Args args))) = do
    let fIn = map parseTraceArg args
    luaAlgBuilder@LuaAlgBuilder{algTraceFuncs, algLatestLuaValueVersion} <- get
    case (fName, fIn) of
        ("trace", tFmt : vs)
            | T.isPrefixOf "\"" tFmt && T.isPrefixOf "\"" tFmt -> do
                let vars = map (\x -> T.pack $ takeWhile (/= '#') $ T.unpack $ getUniqueLuaVariableName (fromMaybe undefined $ HM.lookup x algLatestLuaValueVersion) 0) vs
                put luaAlgBuilder{algTraceFuncs = (vars, T.replace "\"" "" tFmt) : algTraceFuncs}
        ("trace", vs) -> do
            let vars = map (\x -> T.pack $ takeWhile (/= '#') $ T.unpack $ getUniqueLuaVariableName (fromMaybe undefined $ HM.lookup x algLatestLuaValueVersion) 0) vs
            put luaAlgBuilder{algTraceFuncs = (vars, defaultFmt) : algTraceFuncs}
        _ -> error $ "unknown debug method: " ++ show fName ++ " " ++ show args
    where
        parseTraceArg (String s) = s
        parseTraceArg (PrefixExp (PEVar (VarName (Name name)))) = name
        parseTraceArg _ = undefined
processStatement _ _stat = error $ "unknown statement: " <> show _stat

addFunction funcName [i] fOut | toString funcName == "buffer" = do
    addVariable [i] fOut [] "buffer" []
addFunction funcName [i] fOut | toString funcName == "brokenBuffer" = do
    addVariable [i] fOut [] "brokenBuffer" []
addFunction funcName [i] _ | toString funcName == "send" = do
    luaAlgBuilder@LuaAlgBuilder{algGraph} <- get
    put luaAlgBuilder{algGraph = LuaStatement{fIn = [i], fOut = [], fValues = [], fName = "send", fInt = []} : algGraph}
addFunction funcName _ fOut | toString funcName == "receive" = do
    addVariable [] fOut [] "receive" []
addFunction fName _ _ = error $ "unknown function" <> T.unpack fName

addConstant (Number _valueType valueString) = do
    luaAlgBuilder@LuaAlgBuilder{algGraph, algVars} <- get
    let lvv = LuaValueVersion{luaValueVersionName = valueString, luaValueVersionAssignCount = 0, luaValueVersionIsConstant = True}
    case HM.lookup lvv algVars of
        Just value -> do
            let resultName = getUniqueLuaVariableName lvv (length value)
            put luaAlgBuilder{algVars = HM.insert lvv (resultName : value) algVars}
            return resultName
        Nothing -> do
            let resultName = getUniqueLuaVariableName lvv 0
            put
                luaAlgBuilder
                    { algGraph = LuaStatement{fIn = [], fOut = [lvv], fValues = [readText valueString], fName = "constant", fInt = []} : algGraph
                    , algVars = HM.insert lvv [resultName] algVars
                    }
            return resultName
addConstant _ = undefined

addVariable fIn fOut fValues fName fInt = do
    LuaAlgBuilder{algLatestLuaValueVersion} <- get
    let luaValueVersions = map (\x -> nameToLuaValueVersion algLatestLuaValueVersion x) fOut
    let func = LuaStatement{fIn, fValues, fName, fInt, fOut = luaValueVersions}
    mapM_ (uncurry addItemToBuffer) $ zip fOut luaValueVersions
    mapM_ addItemToVars luaValueVersions
    luaAlgBuilder@LuaAlgBuilder{algGraph} <- get
    put luaAlgBuilder{algGraph = func : algGraph}
    where
        nameToLuaValueVersion algLatestLuaValueVersion name =
            case getLuaValueByName name algLatestLuaValueVersion of
                Just lvv@LuaValueVersion{luaValueVersionAssignCount} -> lvv{luaValueVersionAssignCount = luaValueVersionAssignCount + 1}
                Nothing -> LuaValueVersion{luaValueVersionName = name, luaValueVersionAssignCount = 0, luaValueVersionIsConstant = False}
        addItemToBuffer name lvv = do
            luaAlgBuilder@LuaAlgBuilder{algLatestLuaValueVersion} <- get
            put luaAlgBuilder{algLatestLuaValueVersion = HM.insert name lvv algLatestLuaValueVersion}
        addItemToVars name = do
            luaAlgBuilder@LuaAlgBuilder{algVars} <- get
            put luaAlgBuilder{algVars = HM.insert name [] algVars}

addVariableAccess name = do
    luaAlgBuilder@LuaAlgBuilder{algVars} <- get
    luaValueVersion <- getLatestLuaValueVersionByName name
    case HM.lookup luaValueVersion algVars of
        Just value -> do
            let len = length value
            let resultName = getUniqueLuaVariableName luaValueVersion len
            put luaAlgBuilder{algVars = HM.insert luaValueVersion (resultName : value) algVars}
            return resultName
        Nothing -> error ("variable '" ++ show (luaValueVersionName luaValueVersion) ++ " not found. Constants list : " ++ show algVars)

getLatestLuaValueVersionByName name = do
    LuaAlgBuilder{algLatestLuaValueVersion} <- get
    case HM.lookup name algLatestLuaValueVersion of
        Just value -> return value
        Nothing -> error $ "variable not found : '" ++ show name ++ "'."

addAlias from to = do
    luaAlgBuilder@LuaAlgBuilder{algLatestLuaValueVersion} <- get
    case getLuaValueByName to algLatestLuaValueVersion of
        Just value -> do
            put luaAlgBuilder{algLatestLuaValueVersion = HM.insert from value algLatestLuaValueVersion}
        Nothing -> error ("variable '" ++ show to ++ " not found. Constants list : " ++ show algLatestLuaValueVersion)

getLuaValueByName name buffer = HM.lookup name buffer

buildAlg syntaxTree =
    flip execState emptyLuaAlgBuilder $ do
        let (startupFunctionName, startupFunctionCall, startupFunctionDef) = findStartupFunction syntaxTree
            statements = funAssignStatements startupFunctionDef
        _ <- addStartupFuncArgs startupFunctionCall startupFunctionDef
        mapM_ (processStatement startupFunctionName) statements
    where
        emptyLuaAlgBuilder =
            LuaAlgBuilder
                { algGraph = []
                , algLatestLuaValueVersion = HM.empty
                , algVarCounters = HM.empty
                , algVars = HM.empty
                , algStartupArgs = HM.empty
                , algTraceFuncs = []
                }
        funAssignStatements (FunAssign _ (FunBody _ _ (Block statements _))) = statements
        funAssignStatements _ = error "funAssignStatements : not a function assignment"

findStartupFunction (Block statements Nothing)
    | [call] <- filter (\case FunCall{} -> True; _ -> False) statements
      , [funAssign] <- filter (\case FunAssign{} -> True; _ -> False) statements
      , (FunCall (NormalFunCall (PEVar (VarName (Name fnCall))) _)) <- call
      , (FunAssign (FunName (Name fnAssign) _ _) _) <- funAssign
      , fnCall == fnAssign =
        (fnCall, call, funAssign)
findStartupFunction _ = error "can't find startup function in lua source code"

getLuaBlockFromSources src = either (\e -> error $ "Exception while parsing Lua sources: " ++ show e) id $ parseText chunk src

alg2graph LuaAlgBuilder{algGraph, algLatestLuaValueVersion, algVars} = flip execState (DFCluster []) $ do
    mapM addToGraph algGraph
    where
        addToGraph item = do
            graph <- get
            put (addFuncToDataFlowGraph (function2nitta item) graph)
            return $ fromString ""
        function2nitta LuaStatement{fName = "buffer", fIn = [i], fOut = [o], fValues = [], fInt = []} = F.buffer i $ output o
        function2nitta LuaStatement{fName = "brokenBuffer", fIn = [i], fOut = [o], fValues = [], fInt = []} = F.brokenBuffer i $ output o
        function2nitta LuaStatement{fName = "constant", fIn = [], fOut = [o], fValues = [x], fInt = []} = F.constant x $ output o
        function2nitta LuaStatement{fName = "send", fIn = [i], fOut = [], fValues = [], fInt = []} = F.send i
        function2nitta LuaStatement{fName = "add", fIn = [a, b], fOut = [c], fValues = [], fInt = []} = F.add a b $ output c
        function2nitta LuaStatement{fName = "sub", fIn = [a, b], fOut = [c], fValues = [], fInt = []} = F.sub a b $ output c
        function2nitta LuaStatement{fName = "multiply", fIn = [a, b], fOut = [c], fValues = [], fInt = []} = F.multiply a b $ output c
        function2nitta LuaStatement{fName = "divide", fIn = [d, n], fOut = [q, r], fValues = [], fInt = []} = F.division d n (output q) (output r)
        function2nitta LuaStatement{fName = "neg", fIn = [i], fOut = [o], fValues = [], fInt = []} = F.neg i $ output o
        function2nitta LuaStatement{fName = "receive", fIn = [], fOut = [o], fValues = [], fInt = []} = F.receive $ output o
        function2nitta LuaStatement{fName = "shiftL", fIn = [a], fOut = [c], fValues = [], fInt = [s]} = F.shiftL s a $ output c
        function2nitta LuaStatement{fName = "shiftR", fIn = [a], fOut = [c], fValues = [], fInt = [s]} = F.shiftR s a $ output c
        function2nitta LuaStatement{fName = "loop", fIn = [a], fOut = [c], fValues = [x], fInt = []} = F.loop x a $ output c
        function2nitta f = error $ "function not found: " ++ show f
        output v =
            case HM.lookup v algVars of
                Just names -> names
                _ -> error $ "variable not found : " <> show v <> ", buffer : " <> show algLatestLuaValueVersion

lua2functions src =
    let syntaxTree = getLuaBlockFromSources src
        luaAlgBuilder = buildAlg syntaxTree
        frTrace = getFrTrace $ algTraceFuncs luaAlgBuilder
     in FrontendResult{frDataFlow = alg2graph luaAlgBuilder, frTrace = frTrace, frPrettyLog = prettyLog frTrace}

getFrTrace traceFuncs = [TraceVar fmt var | (vars, fmt) <- traceFuncs, var <- vars]

prettyLog traceVars hms = map prettyHM hms
    where
        prettyHM hm = HM.fromList $ map (fromMaybe undefined) $ filter isJust $ map prettyX $ HM.toList hm
        prettyX (v0, x) = do
            -- variables names end on #0, #1..., so we trim this suffix
            let v = takeWhile (/= '#') $ toString v0
            fmt <- v2fmt M.!? v
            Just (toString (takeWhile (/= '^') v), printx (T.unpack fmt) x)
        v2fmt = M.fromList $ map (\(TraceVar fmt v) -> (toString v, fmt)) traceVars
        printx p x
            | 'f' `elem` p = printf p (fromRational (toRational x) :: Double)
            | 's' `elem` p = printf p $ show x
            | otherwise = printf p x
