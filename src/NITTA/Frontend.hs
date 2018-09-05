{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-cse #-}

{-|
Module      : Frontend
Description : Lua frontend prototype
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}

module NITTA.Frontend 
    ( frontendSmokeTest
    ) where

import           Control.Monad.State
import           Data.Default                  (def)
import           Data.List                     (find)
import           Data.Maybe                    (catMaybes)
import qualified Data.String.Utils             as S
import           Data.Text                     (Text, pack, unpack)
import qualified Data.Text                     as T
import           Debug.Trace
import           Language.Lua
import qualified NITTA.Functions               as F
import           Text.InterpolatedString.Perl6 (qq)

data AlgBuilder
    = InputVar Int Int Text -- ix initial var
    | Constant Int Text
    | AliasFromTo Text Text
    | RenamedFromTo Text Text
    | Function
        { fIn     :: [Text]
        , fOut    :: [Text]
        , fName   :: String
        , fValues :: [Int]
        }
    deriving ( Show )


patch var var' = inner
    where
        inner [] = []
        inner (InputVar ix x v : xs) | v == var = InputVar ix x var' : inner xs
        inner (Constant x v : xs) | v == var = Constant x var' : inner xs
        inner (AliasFromTo f t : xs) = AliasFromTo (rn f) (rn t) : inner xs
        inner (f@Function{ fIn, fOut } : xs) = f{ fIn=map rn fIn, fOut=map rn fOut } : inner xs
        inner (x:xs) = x : inner xs

        rn v
            | v == var = var'
            | otherwise = v


data Alg
    = Alg
        { algBuilder :: [AlgBuilder]
        , algVarGen  :: [Text]
        , algVars    :: [Text]
        }

instance Show Alg where
    show Alg{ algBuilder, algVars }
        = "Alg\n{ algBuilder=\n"
        ++ S.join "\n" (map show $ reverse algBuilder)
        ++ "\nalgVars: " ++ show algVars
        ++ "\n}"

runAlgBuilder proc
    = runState proc Alg
        { algBuilder=[]
        , algVarGen=map (pack . ("#" ++) . show) [(0::Int)..]
        , algVars=[]
        }


frontendSmokeTest = do
    let src = [qq|
            function fib(i, a, b)
                i = i + 1
                a, b = b, a + b
                fib(i, a, b)
            end

            fib(0, 0, 1)
            |]
    let Right block@(Block _statments Nothing) = parseText chunk src
    let Right (call, funAssign) = findMain block
    let s = show $ runAlgBuilder $ do
            defineLoopedVars call funAssign
            let statements = funStatements funAssign
            mapM_ statement statements
            addConstants
            -- recCall $ last statements0
    print $ length s
    putStrLn s

    putStrLn "-- the end --"


findMain (Block statements Nothing)
    = let
        [call] = filter (\case FunCall{} -> True; _ -> False) statements
        [funAssign] = filter (\case FunAssign{} -> True; _ -> False) statements
    in Right (call, funAssign)
findMain _ = undefined

defineLoopedVars call funAssign = do
    let FunAssign (FunName (Name _funName) _ _) (FunBody args _ _) = funAssign
    let vars = map (\case (Name v) -> v) args
    let FunCall (NormalFunCall _ (Args callArgs)) = call
    let values = map (\case (Number _ s) -> read (T.unpack s); _ -> undefined) callArgs
    mapM_ addInput $ zip3 [0..] vars values

addInput (ix, var, value) = do
    alg@Alg{ algBuilder, algVars } <- get
    put alg
        { algBuilder=InputVar ix value var : algBuilder
        , algVars=var : algVars
        }



funStatements (FunAssign _ (FunBody _ _ (Block statments _))) = statments
funStatements _ = undefined

statement (Assign lexps rexps) = do
    work <- mapM (uncurry assign) $ zip lexps rexps
    let (renames, adds) = foldl (\(as, bs) (a, b) -> (a ++ as, b ++ bs)) ([], []) work
    diff <- concat <$> mapM id renames
    mapM_ (\f -> f diff) adds

statement (FunCall (NormalFunCall (PEVar (VarName (Name _funName))) (Args args))) = do
    Alg{ algBuilder } <- get
    let algIn = reverse $ filter (\case InputVar{} -> True; _ -> False) algBuilder
    mapM_ (uncurry f) $ zip algIn args
    where
        f (InputVar _ix value o) rexp = do
            i <- arg rexp
            let fun = trace [qq|> loop: $i $o|] $ Function{ fName="loop", fIn=[i], fOut=[o], fValues=[value] }
            alg@Alg{ algBuilder } <- get
            put alg{ algBuilder=fun : algBuilder }
        f _ _ = undefined

statement st =
    case trace ("statement >> " ++ show st) $ show st of
        "" -> undefined
        _  -> return ()


assign (VarName (Name v)) (Binop Add a b) = do
    a' <- arg a
    b' <- arg b
    let f = Function{ fName="Add", fIn=[a', b'], fOut=[v], fValues=[] }
    return
        ( [ renameVars [v] ]
        , [ addFunction' f ]
        )
assign (VarName (Name a)) (PrefixExp (PEVar (VarName (Name b))))
    = return
        ( [ renameVars [a] ]
        , [ \diff -> do
            let b' = applyDiff diff b
            alg@Alg{ algBuilder } <- get
            put alg
                { algBuilder=AliasFromTo a b' : algBuilder
                }
          ]
        )

assign lexp rexp = case trace ("FAIL Assign >> " ++ show (lexp, rexp)) $ show lexp of
    "" -> undefined
    _  -> return ([],[])


applyDiff diff v
    = case find ((== v) . fst) diff of
        Just (_, v') -> v'
        _            -> v

renameVars fOut = do
    Alg{ algVars } <- get
    mapM autoRename $ filter (`elem` algVars) fOut

addFunction' f@Function{ fIn } diff = do
    let fIn' = map
            ( \v -> case find ((== v) . fst) diff of
                Just (_, v') -> v'
                _            -> v )
            fIn
    alg@Alg{ algBuilder } <- get
    put alg
        { algBuilder=f{ fIn=fIn' } : algBuilder
        }
addFunction' _ _ = undefined

arg (Number IntNum valueT) = do
    let value = read $ T.unpack valueT
    alg@Alg{ algBuilder, algVarGen=g:gs } <- get
    case filter (\case (Constant v _) | v == value -> True; _ -> False) algBuilder of
        [] -> do
            put alg
                { algBuilder=Constant value g : algBuilder
                , algVarGen=gs
                }
            return g
        [Constant _ var] -> return var
        _ -> undefined
arg (PrefixExp (PEVar (VarName (Name var)))) = do
    Alg{ algBuilder } <- get
    return $ case find (\case (AliasFromTo v _) | v == var -> True; _ -> False) algBuilder of
        Just (AliasFromTo _ var') -> var'
        _                         -> var

arg a = error $ "arg: " ++ show a


addFunction f@Function{ fIn, fOut } = do
    Alg{ algVars } <- get
    diff <- mapM autoRename $ filter (`elem` algVars) fOut
    let fIn' = map
            ( \v -> case find ((== v) . fst) diff of
                Just (_, v') -> v'
                _            -> v )
            fIn
    alg@Alg{ algBuilder } <- get
    put alg
        { algBuilder=f{ fIn=fIn' } : algBuilder
        , algVars=fOut ++ algVars
        }
addFunction _ = undefined 


renameFromTo var var' = do
    alg@Alg{ algBuilder, algVars } <- get
    put alg
        { algBuilder=RenamedFromTo var var' : patch var var' algBuilder
        , algVars=var' : algVars
        }


autoRename var = do
    alg@Alg{ algVarGen=g:gs } <- get
    let var' = T.concat [var, g]
    put alg { algVarGen=gs }
    renameFromTo var var'
    return (var, var')


addConstants = do
    Alg{ algBuilder } <- get
    let constants = filter (\case Constant{} -> True; _ -> False) algBuilder
    mapM_ (\(Constant x var) ->  addFunction Function{ fName="constant", fIn=[], fOut=[var], fValues=[x] } ) constants
