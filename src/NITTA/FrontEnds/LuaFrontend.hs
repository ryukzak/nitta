{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

{- |
Module      : NITTA.FrontEnds.LuaFrontend
Description : Lua frontend prototype
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

This module analyzes an abstract syntax tree of the Lua language source code, provided by Language.Lua module,
and stores it into a NITTA's data flow graph.

Supported Lua costructions are:

* Simple math operators (addition, subtraction, multiplication and division);

* Variable assignments;

* Bitwise left and right shifts;

* Recursive calls.
-}
module NITTA.FrontEnds.LuaFrontend (
    lua2functions,
    FrontendResult (..),
    TraceVar (..),

    -- * Internal
    LuaAlgBuilder (..),
    LuaStatement (..),
    LuaValueInstance (..),
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

getUniqueLuaVariableName LuaValueInstance{lviName, lviIsConstant = True} luaValueAccessCount = "!" <> lviName <> "#" <> showText luaValueAccessCount
getUniqueLuaVariableName LuaValueInstance{lviName, lviAssignCount} luaValueAccessCount
    | T.head lviName == '_' = lviName
    | otherwise = lviName <> "^" <> showText lviAssignCount <> "#" <> showText luaValueAccessCount

data LuaStatement x = LuaStatement
    { fIn :: [T.Text]
    , fOut :: [LuaValueInstance]
    , fName :: T.Text
    , fValues :: [x]
    , fInt :: [Int]
    }
    deriving (Show, Eq)

-- | Stores information about a particular version of a variable. The version of a variable changes after assigning a new value to it.
data LuaValueInstance = LuaValueInstance
    { lviName :: T.Text
    , lviAssignCount :: Int
    , lviIsConstant :: Bool
    }
    deriving (Show, Eq)

data FrontendResult v x = FrontendResult
    { frDataFlow :: DataFlowGraph v x
    , frTrace :: [TraceVar]
    , frPrettyLog :: [HM.HashMap v x] -> [HM.HashMap String String]
    }

data TraceVar = TraceVar {tvFmt, tvVar :: T.Text}
    deriving (Show)

instance Hashable LuaValueInstance where
    hashWithSalt i LuaValueInstance{lviName, lviAssignCount, lviIsConstant} =
        ( (hashWithSalt i lviName * 31)
            + hashWithSalt i lviAssignCount
        )
            * 31
            + hashWithSalt i lviIsConstant

data LuaAlgBuilder x = LuaAlgBuilder
    { -- | A list containing all expressions to be added to the final graph.
      algGraph :: [LuaStatement x]
    , -- | A table that maps a variable name to the most recent corresponding LuaValueInstance.
      algLatestLuaValueInstance :: HM.HashMap T.Text LuaValueInstance
    , -- | A table needed to generate unique temporary variable names.
      algVarCounters :: HM.HashMap T.Text Int
    , -- | A table lists all uses of a particular LuaValueInstance.
      algVars :: HM.HashMap LuaValueInstance [T.Text]
    , -- | A table correlating the ordinal number of an argument with a variable storing its value and startup value of this variable.
      algStartupArgs :: HM.HashMap Int (T.Text, T.Text)
    , -- | A table correlating constant with LuaValueInstance which store this constant.
      algConstants :: HM.HashMap T.Text LuaValueInstance
    , -- | A list that stores debug information about monitored variables and their display formats.
      algTraceFuncs :: [([T.Text], T.Text)]
    }
    deriving (Show)

defaultFmt = "%.3f"

--left part of lua statement
parseLeftExp (VarName (Name v)) = v
parseLeftExp var = error $ "unexpected lua variable declaration format : " <> show var

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
        getBinopFuncName o = error $ "unknown binop: " <> show o
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
            luaAlgBuilder@LuaAlgBuilder{algVars, algLatestLuaValueInstance, algStartupArgs} <- get
            let value = LuaValueInstance{lviName = name, lviAssignCount = 0, lviIsConstant = False}
            put luaAlgBuilder{algLatestLuaValueInstance = HM.insert name value algLatestLuaValueInstance, algVars = HM.insert value [] algVars, algStartupArgs = HM.insert serialNumber (name, valueString) algStartupArgs}
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
        let startupVarsVersions = map (\x -> LuaValueInstance{lviName = fst x, lviAssignCount = 0, lviIsConstant = False}) startupVarsNames
        mapM_ parseStartupArg $ zip3 args startupVarsVersions (map (readText . snd) startupVarsNames)
    where
        parseStartupArg (arg, valueVersion, index) = do
            varName <- parseExpArg "loop" arg
            luaAlgBuilder@LuaAlgBuilder{algGraph} <- get
            put luaAlgBuilder{algGraph = LuaStatement{fIn = [varName], fOut = [valueVersion], fValues = [index], fName = "loop", fInt = []} : algGraph}
processStatement _ (FunCall (NormalFunCall (PEVar (VarName (Name fName))) (Args args))) = do
    fIn <- mapM (parseExpArg "tmp") args
    addFunction (fromText fName) fIn [fromString ""]
processStatement _fn (FunCall (NormalFunCall (PEVar (SelectName (PEVar (VarName (Name "debug"))) (Name fName))) (Args args))) = do
    let fIn = map parseTraceArg args
    luaAlgBuilder@LuaAlgBuilder{algTraceFuncs, algLatestLuaValueInstance} <- get
    case (fName, fIn) of
        ("trace", tFmt : vs)
            | T.isPrefixOf "\"" tFmt && T.isPrefixOf "\"" tFmt -> do
                let vars = map (\x -> T.pack $ takeWhile (/= '#') $ T.unpack $ getUniqueLuaVariableName (fromMaybe undefined $ HM.lookup x algLatestLuaValueInstance) 0) vs
                put luaAlgBuilder{algTraceFuncs = (vars, T.replace "\"" "" tFmt) : algTraceFuncs}
        ("trace", vs) -> do
            let vars = map (\x -> T.pack $ takeWhile (/= '#') $ T.unpack $ getUniqueLuaVariableName (fromMaybe undefined $ HM.lookup x algLatestLuaValueInstance) 0) vs
            put luaAlgBuilder{algTraceFuncs = (vars, defaultFmt) : algTraceFuncs}
        _ -> error $ "unknown debug method: " <> show fName <> " " <> show args
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
    luaAlgBuilder@LuaAlgBuilder{algGraph, algVars, algConstants} <- get
    let lvv = LuaValueInstance{lviName = valueString, lviAssignCount = 0, lviIsConstant = True}
    case HM.lookup valueString algConstants of
        Just value -> do
            let names = fromMaybe (error "lua constants parsing error") $ HM.lookup value algVars
            let resultName = getUniqueLuaVariableName lvv $ length names
            put luaAlgBuilder{algVars = HM.insert value (resultName : names) algVars}
            return resultName
        Nothing -> do
            let resultName = getUniqueLuaVariableName lvv 0
            put
                luaAlgBuilder
                    { algGraph = LuaStatement{fIn = [], fOut = [lvv], fValues = [readText valueString], fName = "constant", fInt = []} : algGraph
                    , algVars = HM.insert lvv [resultName] algVars
                    , algConstants = HM.insert valueString lvv algConstants
                    }
            return resultName
addConstant _ = undefined

addVariable fIn fOut fValues fName fInt = do
    LuaAlgBuilder{algLatestLuaValueInstance} <- get
    let luaValueInstances = map (\x -> nameToLuaValueInstance algLatestLuaValueInstance x) fOut
    let func = LuaStatement{fIn, fValues, fName, fInt, fOut = luaValueInstances}
    mapM_ (uncurry addItemToBuffer) $ zip fOut luaValueInstances
    mapM_ addItemToVars luaValueInstances
    luaAlgBuilder@LuaAlgBuilder{algGraph, algConstants, algLatestLuaValueInstance = algLatestLuaValueInstance'} <- get
    case fName of
        "constant" -> do
            case HM.lookup (showText $ head fValues) algConstants of
                Just lvv -> do
                    put luaAlgBuilder{algLatestLuaValueInstance = HM.insert (head fOut) lvv algLatestLuaValueInstance'}
                Nothing -> do
                    put luaAlgBuilder{algGraph = func : algGraph, algConstants = HM.insert (showText $ head fValues) (head luaValueInstances) algConstants}
        _ -> do
            put luaAlgBuilder{algGraph = func : algGraph}
    where
        nameToLuaValueInstance algLatestLuaValueInstance name =
            case getLuaValueByName name algLatestLuaValueInstance of
                Just lvv@LuaValueInstance{lviAssignCount} -> lvv{lviAssignCount = lviAssignCount + 1}
                Nothing -> LuaValueInstance{lviName = name, lviAssignCount = 0, lviIsConstant = False}
        addItemToBuffer name lvv = do
            luaAlgBuilder@LuaAlgBuilder{algLatestLuaValueInstance} <- get
            put luaAlgBuilder{algLatestLuaValueInstance = HM.insert name lvv algLatestLuaValueInstance}
        addItemToVars name = do
            luaAlgBuilder@LuaAlgBuilder{algVars} <- get
            put luaAlgBuilder{algVars = HM.insert name [] algVars}

addVariableAccess name = do
    luaAlgBuilder@LuaAlgBuilder{algVars} <- get
    luaValueInstance <- getLatestLuaValueInstanceByName name
    case HM.lookup luaValueInstance algVars of
        Just value -> do
            let len = length value
            let resultName = getUniqueLuaVariableName luaValueInstance len
            put luaAlgBuilder{algVars = HM.insert luaValueInstance (resultName : value) algVars}
            return resultName
        Nothing -> error ("variable '" <> show (lviName luaValueInstance) <> " not found. Constants list : " <> show algVars)

getLatestLuaValueInstanceByName name = do
    LuaAlgBuilder{algLatestLuaValueInstance} <- get
    case HM.lookup name algLatestLuaValueInstance of
        Just value -> return value
        Nothing -> error $ "variable not found : '" <> show name <> "'."

addAlias from to = do
    luaAlgBuilder@LuaAlgBuilder{algLatestLuaValueInstance} <- get
    case getLuaValueByName to algLatestLuaValueInstance of
        Just value -> do
            put luaAlgBuilder{algLatestLuaValueInstance = HM.insert from value algLatestLuaValueInstance}
        Nothing -> error ("variable '" <> show to <> " not found. Constants list : " <> show algLatestLuaValueInstance)

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
                , algLatestLuaValueInstance = HM.empty
                , algVarCounters = HM.empty
                , algVars = HM.empty
                , algStartupArgs = HM.empty
                , algConstants = HM.empty
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

getLuaBlockFromSources src = either (\e -> error $ "Exception while parsing Lua sources: " <> show e) id $ parseText chunk src

alg2graph LuaAlgBuilder{algGraph, algLatestLuaValueInstance, algVars} = flip execState (DFCluster []) $ do
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
        function2nitta f = error $ "function not found: " <> show f
        output v =
            case HM.lookup v algVars of
                Just names -> names
                _ -> error $ "variable not found : " <> show v <> ", buffer : " <> show algLatestLuaValueInstance

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
