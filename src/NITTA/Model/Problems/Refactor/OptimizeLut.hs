-- All extensions should be enabled explicitly due to doctest in this module.
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}


module NITTA.Model.Problems.Refactor.OptimizeLut (
    OptimizeLut (..),
    OptimizeLutProblem (..),
) where

import Data.List qualified as L
import Data.Map qualified as M
import Data.Set qualified as S
import GHC.Generics
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Types


data OptimizeLut v x = OptimizeLut
    { rOld :: [F v x]
    , rNew :: [F v x]
    }
    deriving (Generic, Show, Eq)

class OptimizeLutProblem u v x | u -> v x where
    -- | Function takes algorithm in 'DataFlowGraph' and return list of 'Refactor' that can be done
    optimizeLutOptions :: u -> [OptimizeLut v x]
    optimizeLutOptions _ = []

    -- | Function takes 'OptimizeLut' and modify 'DataFlowGraph'
    optimizeLutDecision :: u -> OptimizeLut v x -> u
    optimizeLutDecision _ _ = error "not implemented"

instance (Var v, Val x) => OptimizeLutProblem [F v x] v x where
    optimizeLutOptions fs = 
        L.nub 
            [ OptimizeLut{rOld, rNew}
            | rOld <- selectClusters $ filter isSupportedByLut fs
            , let rNew = optimizeCluster rOld
            , S.fromList rOld /= S.fromList rNew
            ]

    optimizeLutDecision fs OptimizeLut{rOld, rNew} = rNew <> (fs L.\\ rOld)

selectClusters fs =
    L.nubBy
        (\a b -> S.fromList a == S.fromList b)
        [ [f, f']
        | f <- fs
        , f' <- fs
        , f' /= f
        , inputOutputIntersect f f'
        ]
    where
        inputOutputIntersect f1 f2 = not $ S.disjoint (inputs f1) (outputs f2) || S.disjoint (inputs f2) (outputs f1)


isSupportedByLut f
    | Just LogicAnd{} <- castF f = True
    | Just LogicOr{} <- castF f = True
    | Just LogicNot{} <- castF f = True
    | Just LUT{} <- castF f = True
    | otherwise = False

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
            | Just LogicAnd{} <- castF f =
                [ andLut (S.elemAt 0 $ inputs f) (S.elemAt 1 $ inputs f) (S.elemAt 0 $ outputs f) ]
            | Just LogicOr{} <- castF f =
                [ orLut (S.elemAt 0 $ inputs f) (S.elemAt 1 $ inputs f) (S.elemAt 0 $ outputs f) ]
            | Just LogicNot{} <- castF f =
                [ notLut (S.elemAt 0 $ inputs f) (S.elemAt 0 $ outputs f) ]
            | otherwise = [f, f']

andLut a b c =
    packF $ LUT
        (M.fromList
            [ ([f, f], f)
            , ([f, t], f)
            , ([t, f], f)
            , ([t, t], t)
            ])
        [I a, I b] (O $ S.singleton c)
    where
    (t, f) = (True, False)

orLut a b c =
    packF $ LUT
      (M.fromList
        [ ([f, f], f)
        , ([f, t], t)
        , ([t, f], t)
        , ([t, t], t)
        ])
        [I a, I b] (O $ S.singleton c)
  where
    (t, f) = (True, False)

notLut a c =
    packF $ LUT
      (M.fromList
        [ ([f], t)
        , ([t], f)
        ])
        [I a] (O $ S.singleton c)
  where
    (t, f) = (True, False)
