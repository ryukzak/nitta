{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}

{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-cse #-}

{-|
Module      : NITTA.LuaFrontend
Description : Lua frontend prototype
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.LuaFrontend
    ( lua2functions
    , DebugData(..)
    , DebugFunctionT(..)
    ) where

import           Control.Monad                 (when)
import           Control.Monad.Identity
import           Control.Monad.State
import           Data.List                     (find, group, sort)
import qualified Data.Map                      as M
import           Data.Maybe                    (fromMaybe)
import qualified Data.String.Utils             as S
import           Data.Text                     (Text, pack, unpack)
import qualified Data.Text                     as T
import           Language.Lua
import qualified NITTA.Intermediate.Functions  as F
import           NITTA.Model.TargetSystem
import           NITTA.Utils                   (modify'_)
import           Text.InterpolatedString.Perl6 (qq)

type VarDict = M.Map Text ([String], [String])

-- |Data type for collecting functions for debug
data DebugFunctionT = DebugFunctionT
      { inputVars  :: [Text]
      , outputVars :: [Text]
      , name       :: String
      } deriving (Show)

-- |Data type that contains all information for debugging functions
data DebugData = DebugData [DebugFunctionT] VarDict deriving (Show)

toDebugData debugFunctions varDict = let
        toDebugFunctionT (DebugFunction inputVars outputVars name _) = DebugFunctionT inputVars outputVars name
        toDebugFunctionT _                                           = error "error in pattern mathing DebugFunction"
    in
        DebugData (map toDebugFunctionT debugFunctions) varDict

lua2functions src
    = let
        ast = either (\e -> error $ "can't parse lua src: " ++ show e) id $ parseText chunk src
        AlgBuilder{ algItems } = buildAlg ast
        fs = filter (\case Function{} -> True; _ -> False) algItems
        debugFunctions = filter (\case DebugFunction{} -> True; _ -> False) algItems
        varDict :: VarDict
        varDict = M.fromList
            $ map varRow
            $ group $ sort $ concatMap fIn fs
        alg = snd $ execState (mapM_ (store <=< function2nitta) fs) (varDict, [])
        dataFlowG = fsToDataFlowGraph alg
        debugData = toDebugData debugFunctions varDict
    in (dataFlowG, debugData)
    where
        varRow lst@(x:_)
            = let vs = zipWith f lst [0..]
            in (x, (vs, vs))
        varRow _ = undefined
        f v i = let
                v' = unpack v
            in [qq|{v'}#{i}|]


buildAlg ast
    = let
        Right ( mainName, mainCall, mainFunDef ) = findMain ast
        alg0 = AlgBuilder
            { algItems=[]
            , algBuffer=[]
            , algVarGen=map (pack . show) [0..]
            , algVars=[]
            }
        builder = do
            addMainInputs mainFunDef mainCall
            mapM_ (processStatement mainName) $ funAssignStatments mainFunDef
            addConstants
    in execState builder alg0


data AlgBuilder x
    = AlgBuilder
        { algItems  :: [AlgBuilderItem x]
        , algBuffer :: [AlgBuilderItem x]
        , algVarGen :: [Text]
        , algVars   :: [Text]
        }

instance ( Show x ) => Show (AlgBuilder x) where
    show AlgBuilder{ algItems, algBuffer, algVars }
        = "AlgBuilder\n{ algItems=\n"
        ++ S.join "\n" (map show $ reverse algItems)
        ++ "\nalgBuffer: " ++ show algBuffer
        ++ "\nalgVars: " ++ show algVars
        ++ "\n}"

data AlgBuilderItem x
    = InputVar
        { iIx  :: Int -- ^Index
        , iVar :: Text  -- ^Symbol
        , iX   :: x -- ^Initial value
        }
    | Constant{ cVar :: Text, cX :: x, cTextX :: Text }
    | Alias{ aFrom :: Text, aTo :: Text }
    | Renamed{ rFrom :: Text, rTo :: Text }
    | Function
        { fIn     :: [Text]
        , fOut    :: [Text]
        , fName   :: String
        , fValues :: [x]
        }
    | DebugFunction
        { fIn     :: [Text]
        , fOut    :: [Text]
        , fName   :: String
        , fValues :: [x]
        }
    -- FIXME: check all local variable declaration
    deriving ( Show )



-- *Translate AlgBuiler functions to nitta functions
function2nitta Function{ fName="loop",     fIn=[i],    fOut=[o],    fValues=[x] } = F.loop x <$> input i <*> output o
function2nitta Function{ fName="reg",      fIn=[i],    fOut=[o],    fValues=[]  } = F.reg <$> input i <*> output o
function2nitta Function{ fName="constant", fIn=[],     fOut=[o],    fValues=[x] } = F.constant x <$> output o
function2nitta Function{ fName="send",     fIn=[i],    fOut=[],     fValues=[]  } = F.send <$> input i
function2nitta Function{ fName="add",      fIn=[a, b], fOut=[c],    fValues=[]  } = F.add <$> input a <*> input b <*> output c
function2nitta Function{ fName="sub",      fIn=[a, b], fOut=[c],    fValues=[]  } = F.sub <$> input a <*> input b <*> output c
function2nitta Function{ fName="multiply", fIn=[a, b], fOut=[c],    fValues=[]  } = F.multiply <$> input a <*> input b <*> output c
function2nitta Function{ fName="divide",   fIn=[d, n], fOut=[q, r], fValues=[]  } = F.division <$> input d <*> input n <*> output q <*> output r
function2nitta Function{ fName="receive",  fIn=[],     fOut=[o],    fValues=[]  } = F.receive <$> output o
-- function2nitta Function{ fName="shiftL",   fIn=[a],    fOut=[c],    fValues=[]  } = F.shiftL <$> input a <*> output c
-- function2nitta Function{ fName="shiftR",   fIn=[a],    fOut=[c],    fValues=[]  } = F.shiftR <$> input a <*> output c
function2nitta f = error $ "frontend don't known function: " ++ show f



input v = do
    (dict, fs) <- get
    let (x:xs, lst) = dict M.! v
    put (M.insert v (xs, lst) dict, fs)
    return x

output v
    | T.head v == '_' = return []
    | otherwise = gets $ \(dict, _fs) ->
        snd (fromMaybe (error [qq|unknown variable: $v|]) (dict M.!? v))

store f = modify'_ $ \(dict, fs) -> (dict, f:fs)



-- *AST inspection and algorithm builder

findMain (Block statements Nothing)
    | [call] <- filter (\case FunCall{} -> True; _ -> False) statements
    , [funAssign] <- filter (\case FunAssign{} -> True; _ -> False) statements
    , (FunCall (NormalFunCall (PEVar (VarName (Name fnCall))) _)) <- call
    , (FunAssign (FunName (Name fnAssign) _ _) _) <- funAssign
    , fnCall == fnAssign
    = Right (fnCall, call, funAssign)
findMain _ = error "can't find main function in lua source code"


addMainInputs
        (FunAssign (FunName (Name _funName) _ _) (FunBody declArgs _ _))
        (FunCall (NormalFunCall _ (Args callArgs))) = do
    let vars = map (\case (Name v) -> v) declArgs

    let values = map (\case (Number _ s) -> unpack s; _ -> error "lua: wrong main argument") callArgs
    let values' = map read values
    when (length vars /= length values')
        $ error "a different number of arguments in main a function declaration and call"

    forM_ (zip3 [0..] vars values')
        $ \(iIx, iVar, iX) -> addItem InputVar{ iIx, iVar, iX } [iVar]

addMainInputs _ _ = error "bad main function description"


addConstants = do
    AlgBuilder{ algItems } <- get
    let constants = filter (\case Constant{} -> True; _ -> False) algItems
    forM_ constants $ \Constant{ cVar, cX } ->
        addFunction Function{ fName="constant", fIn=[], fOut=[cVar], fValues=[cX] }



processStatement fn (LocalAssign names (Just [rexp])) = do
    processStatement fn $ LocalAssign names Nothing
    processStatement fn $ Assign (map VarName names) [rexp]

processStatement _fn (LocalAssign names Nothing) = do
    AlgBuilder{ algVars } <- get
    forM_ names $ \(Name n) ->
        when (n `elem` algVars) $ error "local variable already defined"

processStatement _fn st@(Assign lexps rexps)
    = if
        | length lexps == length rexps -> do
            let outs = map (\case (VarName (Name v)) -> [v]; l -> error $ "bad left expression: " ++ show l) lexps
            diff <- concat <$> mapM renameVarsIfNeeded outs
            zipWithM_ (rightExp diff) outs rexps
            flushBuffer
        | length lexps > 1 && length rexps == 1 -> do
            let outs = map (\case (VarName (Name v)) -> v; l -> error $ "bad left expression: " ++ show l) lexps
            diff <- renameVarsIfNeeded outs
            rightExp diff outs $ head rexps
            flushBuffer
        | otherwise -> error $ "processStatement: " ++ show st

processStatement fn (FunCall (NormalFunCall (PEVar (VarName (Name fName))) (Args args)))
    | fn == fName
    = do
        AlgBuilder{ algItems } <- get
        let algIn = reverse $ filter (\case InputVar{} -> True; _ -> False) algItems
        mapM_ (uncurry f) $ zip algIn args
        where
            f InputVar{ iX, iVar } rexp = do
                i <- expArg [] rexp
                let loop = Function{ fName="loop", fIn=[i], fOut=[iVar], fValues=[iX] }
                let traceLoop = DebugFunction{ fName="trace", fIn=[i], fOut=[], fValues=[] }
                modify'_ $ \alg@AlgBuilder{ algItems } -> alg{ algItems=traceLoop : loop : algItems }
            f _ _ = undefined

processStatement _fn (FunCall (NormalFunCall (PEVar (SelectName (PEVar (VarName (Name "debug"))) (Name fName))) (Args args))) = do
    fIn <- mapM (expArg []) args

    if fName == "trace"
      then addFunction DebugFunction{ fName=unpack fName, fIn, fOut=[], fValues=[] }
      else addFunction Function{ fName=unpack fName, fIn, fOut=[], fValues=[] }

processStatement _fn (FunCall (NormalFunCall (PEVar (VarName (Name fName))) (Args args))) = do
    fIn <- mapM (expArg []) args
    addFunction Function{ fName=unpack fName, fIn, fOut=[], fValues=[] }

processStatement _fn st = error $ "statement: " ++ show st



rightExp diff fOut (Binop op a b) = do
    a' <- expArg diff a
    b' <- expArg diff b
    let f = Function{ fName=binop op, fIn=[a', b'], fOut, fValues=[] }
    patchAndAddFunction f diff
    where
        binop Add = "add"
        binop Sub = "sub"
        binop Mul = "multiply"
        binop Div = "divide"
        binop o   = error $ "unknown binop: " ++ show o

rightExp
        diff
        fOut
        (PrefixExp (PEFunCall (NormalFunCall
            (PEVar (VarName (Name fn)))
            (Args args)
        ))) = do
    fIn <- mapM (expArg diff) args
    let f = Function{ fName=unpack fn, fIn, fOut, fValues=[] }
    patchAndAddFunction f diff

rightExp diff [a] (PrefixExp (PEVar (VarName (Name b)))) -- a = b
    = addItemToBuffer Alias{ aFrom=a, aTo=applyPatch diff b }

rightExp diff out (PrefixExp (Paren e)) -- a = (...)
    = rightExp diff out e

rightExp diff [a] n@(Number _ _) = do -- a = 42
    b <- expConstant (T.concat ["@", a, "@const"]) n
    addItemToBuffer Alias{ aFrom=a, aTo=applyPatch diff b }

rightExp diff [a] (Unop Neg (Number numType n)) = rightExp diff [a] $ Number numType $ T.cons '-' n

rightExp diff [a] (Unop Neg expr@(PrefixExp _)) =
    -- FIXME: add negative function
    let binop = Binop Sub (Number IntNum "0") expr
    in rightExp diff [a] binop

rightExp _diff _out rexp = error $ "rightExp: " ++ show rexp



expArg _diff n@(Number _ _) = expConstant "@const" n

expArg _diff (PrefixExp (PEVar (VarName (Name var)))) = findAlias var

expArg diff call@(PrefixExp (PEFunCall _)) = do
    c <- genVar "tmp"
    rightExp diff [c] call
    return c

expArg diff (PrefixExp (Paren arg)) = expArg diff arg

expArg diff binop@Binop{} = do
    c <- genVar "tmp"
    rightExp diff [c] binop
    return c

expArg _diff (Unop Neg (Number numType n)) = expConstant "@const" $ Number numType $ T.cons '-' n

expArg diff (Unop Neg expr@(PrefixExp _)) = do
    c <- genVar "tmp"
    let binop = Binop Sub (Number IntNum "0") expr
    rightExp diff [c] binop
    return c

expArg _diff a = error $ "expArg: " ++ show a



-- *Internal

expConstant suffix (Number _ textX) = do
    AlgBuilder{ algItems } <- get
    case find (\case Constant{ cTextX } | cTextX == textX -> True; _ -> False) algItems of
        Just Constant{ cVar } -> return cVar
        Nothing -> do
            let cVar = T.concat [ textX, suffix ]
            addItem Constant
                { cX=read $ unpack textX
                , cVar
                , cTextX=textX
                }
                []
            return cVar
        Just _ -> error "internal error"
expConstant _ _ = undefined



addFunction f@Function{ fOut } = do
    diff <- renameVarsIfNeeded fOut
    patchAndAddFunction f diff
addFunction f@DebugFunction{ fOut } = do
    diff <- renameVarsIfNeeded fOut
    patchAndAddFunction f diff
addFunction _ = error "addFunction error"



patchAndAddFunction f@Function{ fIn } diff = do
    let fIn' = map (applyPatch diff) fIn
    modify'_ $ \alg@AlgBuilder{ algItems } ->
        alg{ algItems=f{ fIn=fIn' } : algItems }
patchAndAddFunction f@DebugFunction{ fIn } diff = do
    let fIn' = map (applyPatch diff) fIn
    modify'_ $ \alg@AlgBuilder{ algItems } ->
        alg{ algItems=f{ fIn=fIn' } : algItems }
patchAndAddFunction _ _ = undefined



renameVarsIfNeeded fOut = do
    AlgBuilder{ algVars } <- get
    mapM autoRename $ filter (`elem` algVars) fOut

autoRename var = do
    var' <- genVar var
    renameFromTo var var'
    return (var, var')

renameFromTo rFrom rTo = do
    alg@AlgBuilder{ algItems, algVars } <- get
    put alg
        { algItems=Renamed{ rFrom, rTo } : patch algItems
        , algVars=rTo : algVars
        }
    where
        patch [] = []
        patch (i@InputVar{ iVar } : xs) = i{ iVar=rn iVar } : patch xs
        patch (Constant{ cX, cVar, cTextX } : xs) = Constant{ cX, cVar=rn cVar, cTextX } : patch xs
        patch (Alias{ aFrom, aTo } : xs) = Alias (rn aFrom) (rn aTo) : patch xs
        patch (f@Function{ fIn, fOut } : xs) = f{ fIn=map rn fIn, fOut=map rn fOut } : patch xs
        patch (x:xs) = x : patch xs

        rn v
            | v == rFrom = rTo
            | otherwise = v



funAssignStatments (FunAssign _ (FunBody _ _ (Block statments _))) = statments
funAssignStatments _                                               = error "funAssignStatments : not function assignment"


flushBuffer = modify'_
    $ \alg@AlgBuilder{ algBuffer, algItems } -> alg
        { algItems=algBuffer ++ algItems
        , algBuffer=[]
        }


addItemToBuffer item = modify'_
    $ \alg@AlgBuilder{ algBuffer } -> alg
        { algBuffer=item : algBuffer
        }


addItem item vars = modify'_
    $ \alg@AlgBuilder{ algItems, algVars } -> alg
        { algItems=item : algItems
        , algVars=vars ++ algVars
        }


genVar prefix = do
    alg@AlgBuilder{ algVarGen } <- get
    when (null algVarGen) $ error "internal error"
    put alg{ algVarGen=tail algVarGen }
    return $ T.concat [ prefix, "_", head algVarGen ]


findAlias var0 = do
    AlgBuilder{ algItems } <- get
    return $ inner var0 algItems
    where
        inner var [] = var
        inner var (Alias{ aFrom, aTo }:xs)
            | aFrom == var = inner aTo xs
        inner var (_:xs) = inner var xs


applyPatch diff v
    = case find ((== v) . fst) diff of
        Just (_, v') -> v'
        _            -> v
