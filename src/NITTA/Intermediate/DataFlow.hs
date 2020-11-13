{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : NITTA.Intermediate.DataFlow
Description : DataFlow graph
Copyright   : (c) Aleksandr Penskoi, 2020
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Intermediate.DataFlow
    ( DataFlowGraph(..)
    , fsToDataFlowGraph, dataFlowGraphToFs
    ) where

import           Control.Exception ( assert )
import qualified Data.Set as S
import           GHC.Generics
import           NITTA.Intermediate.Functions
import           NITTA.Intermediate.Types
import           NITTA.Utils.Base


-- |Data flow graph - intermediate representation of application algorithm.
-- Right now can be replaced by @[F v x]@, but for future features like
-- conduction statement, we don't do that.
data DataFlowGraph v x
    = DFLeaf (F v x)
    | DFCluster [ DataFlowGraph v x ]
    deriving ( Show, Generic )

instance Eq ( DataFlowGraph v x ) where
    (DFCluster c1) == (DFCluster c2) = S.fromList (map show c1) == S.fromList (map show c2)
    (DFLeaf f1) == (DFLeaf f2) = f1 == f2
    _ == _ = False

instance ( Var v ) => Variables ( DataFlowGraph v x ) v where
    variables (DFLeaf fb)   = variables fb
    variables (DFCluster g) = unionsMap variables g

instance WithFunctions (DataFlowGraph v x) (F v x) where
    functions (DFLeaf f)    = [ f ]
    functions (DFCluster g) = concatMap functions g

instance ( Var v, Val x ) => Patch (DataFlowGraph v x) (v, v) where
    patch diff@(v, v') (DFCluster cluster) = let
            newReg = DFLeaf $ reg v [v']
            cluster' = map (patch diff) cluster
        in assert (all (\case DFLeaf _ -> True; _ -> False) cluster) -- patch DataFlowGraph with subclusters is not support
            $ DFCluster $ newReg : cluster'
    patch diff@(v, _) n@(DFLeaf f)
        | v `S.member` inputs f = DFLeaf $ patch diff f
        | otherwise = n


-- |Convert @[ F v x ]@ to 'DataFlowGraph'.
fsToDataFlowGraph alg = DFCluster $ map DFLeaf alg

-- |Convert 'DataFlowGraph' to @[ F v x ]@.
dataFlowGraphToFs (DFCluster leafs) = map
    (\case
        DFLeaf f -> f
        _        -> error "Data flow graph structure error"
    )
    leafs
dataFlowGraphToFs _ = error "Data flow graph structure error"
