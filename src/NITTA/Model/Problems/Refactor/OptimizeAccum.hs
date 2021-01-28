{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : NITTA.Model.Problems.Refactor.OptimizeAccum
Description : Optimize an algorithm for Accum processor unit
Copyright   : (c) Daniil Prohorov, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Problems.Refactor.OptimizeAccum (
    OptimizeAccum (..),
    OptimizeAccumProblem (..),
) where

import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import GHC.Generics
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Types

{- |OptimizeAccum example:

> OptimizeAccum [+a +tmp_1 => d; +b +c => tmp_1] [+a +b +c => d]

before:

> [+a +tmp_1 => d; +b +c => tmp_1]

after:

> [+a +b +c => d]
-}
data OptimizeAccum v x = OptimizeAccum
    { refOld :: [F v x]
    , refNew :: [F v x]
    }
    deriving (Generic, Show, Eq)

class OptimizeAccumProblem u v x | u -> v x where
    -- |Function takes algorithm in 'DataFlowGraph' and return list of 'Refactor' that can be done
    optimizeAccumOptions :: u -> [OptimizeAccum v x]
    optimizeAccumOptions _ = []

    -- |Function takes 'OptimizeAccum' and modify 'DataFlowGraph'
    optimizeAccumDecision :: u -> OptimizeAccum v x -> u
    optimizeAccumDecision _ _ = error "not implemented"

instance (Var v, Val x) => OptimizeAccumProblem [F v x] v x where
    optimizeAccumOptions fs =
        [ OptimizeAccum{refOld, refNew}
        | refOld <- selectClusters $ filter isSupportByAccum fs
        , let refNew = optimizeCluster refOld
        , S.fromList refOld /= S.fromList refNew
        ]

    optimizeAccumDecision fs OptimizeAccum{refOld, refNew} =
        filter (`notElem` refOld) fs ++ refNew

selectClusters fs =
    L.nubBy
        (\a b -> S.fromList a == S.fromList b)
        [ cluster
        | f <- fs
        , let cluster = selectClusterFor f
        , length cluster > 1
        ]
    where
        selectClusterFor f =
            f :
                [ f'
                | f' <- fs
                , f' /= f
                , not $ null (variables f `S.intersection` variables f')
                ]

isSupportByAccum f
    | Just Add{} <- castF f = True
    | Just Sub{} <- castF f = True
    | Just Acc{} <- castF f = True
    | otherwise = False

-- |Create Map String (HistoryTree (F v x)), where Key is input label and Value is FU that contain this input label
containerMapCreate fs =
    M.unions $
        map
            ( \f ->
                foldl
                    ( \dataMap k ->
                        M.insertWith (++) k [f] dataMap
                    )
                    M.empty
                    (S.toList $ inputs f)
            )
            fs

-- |Takes container and refactor it, if it can be
optimizeCluster fs = concatMap refactored fs
    where
        containerMap = containerMapCreate fs

        refactored f =
            concatMap
                ( \o ->
                    case M.findWithDefault [] o containerMap of
                        [] -> []
                        matchedFUs -> concatMap (refactorFunction f) matchedFUs
                )
                (S.toList $ outputs f)

refactorFunction f' f
    | Just (Acc lst') <- castF f'
      , Just (Acc lst) <- castF f
      , let multipleOutBool = (1 <) $ length $ outputs f'
            isOutInpIntersect =
                any
                    ( \case
                        Push _ (I v) -> elem v $ outputs f'
                        _ -> False
                    )
                    lst
            makeRefactor = not multipleOutBool && isOutInpIntersect
         in makeRefactor =
        let subs _ (Push Minus _) (Push Plus v) = Just $ Push Minus v
            subs _ (Push Minus _) (Push Minus v) = Just $ Push Plus v
            subs _ (Push Plus _) push@(Push _ _) = Just push
            subs v _ pull@(Pull _) = deleteFromPull v pull
            subs _ _ _ = error "Pull can not be here"

            refactorAcc _ _ (Pull o) = [Pull o]
            refactorAcc accList accNew accOld@(Push s i@(I v))
                | elem v $ outputs accNew = mapMaybe (subs v accOld) accList
                | s == Minus = [Push Minus i]
                | s == Plus = [Push Plus i]
            refactorAcc _ _ (Push _ (I _)) = undefined
         in [packF $ Acc $ concatMap (refactorAcc lst' f') lst]
    | Just f1 <- fromAddSub f'
      , Just f2 <- fromAddSub f = case refactorFunction f1 f2 of
        [fNew] -> [fNew]
        _ -> [f, f']
    | otherwise = [f, f']

deleteFromPull v (Pull (O s))
    | S.null deleted = Nothing
    | otherwise = Just $ Pull $ O deleted
    where
        deleted = S.delete v s
deleteFromPull _ (Push _ _) = error "delete only Pull"

fromAddSub f
    | Just (Add in1 in2 (O out)) <- castF f =
        Just $
            acc $
                [Push Plus in1, Push Plus in2] ++ [Pull $ O $ S.fromList [o] | o <- S.toList out]
    | Just (Sub in1 in2 (O out)) <- castF f =
        Just $
            acc $
                [Push Plus in1, Push Minus in2] ++ [Pull $ O $ S.fromList [o] | o <- S.toList out]
    | otherwise = Nothing
