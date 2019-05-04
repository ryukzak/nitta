{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Model
Description : Model of target system for synthesis and so on.
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model
    ( ModelState(..)
    , DataFlowGraph(..)
    , isSynthesisFinish
    , targetProcessDuration
    , endpointOptionToDecision
    , isPUSynthesisFinite
    ) where

import           Control.Exception (assert)
import           Data.Set          (fromList, member)
import           Data.Typeable
import           GHC.Generics
import           NITTA.BusNetwork
import           NITTA.Functions   (reg)
import           NITTA.Types
import           NITTA.Utils
import           NITTA.Utils.Lens
import           Numeric.Interval  ((...))


-- |Model of target unit, which is a main subject of synthesis process and
-- synthesis graph.
data ModelState u v x
    = ModelState
        { mUnit          :: u -- ^model of target unit
        , mDataFlowGraph :: DataFlowGraph v x -- ^whole application algorithm
        }
    deriving ( Generic )

instance WithFunctions (ModelState (BusNetwork title v x t) v x) (F v x) where
    functions ModelState{ mUnit, mDataFlowGraph }
        = assert (fromList (functions mUnit) == fromList (functions mDataFlowGraph)) -- inconsistent ModelState
            $ functions mUnit

instance ( Title title, VarValTime v x t ) =>
        DecisionProblem (BindingDT title v x)
              BindingDT (ModelState (BusNetwork title v x t) v x)
        where
    options _ ModelState{ mUnit }      = options binding mUnit
    decision _ f@ModelState{ mUnit } d = f{ mUnit=decision binding mUnit d }

instance ( Title title, VarValTime v x t
         ) => DecisionProblem (DataFlowDT title v t)
                   DataFlowDT (ModelState (BusNetwork title v x t) v x)
         where
    options _ ModelState{ mUnit }      = options dataFlowDT mUnit
    decision _ f@ModelState{ mUnit } d = f{ mUnit=decision dataFlowDT mUnit d }



-- |Data flow graph - intermediate representation of application algorithm.
-- Right now can be replaced by @[F v x]@, but for future features like
-- conduction statement, we don't do that.
data DataFlowGraph v x
    = DFLeaf (F v x)
    | DFCluster [ DataFlowGraph v x ]
    deriving ( Show, Generic )

instance ( Var v, Typeable x ) => Patch (DataFlowGraph v x) (v, v) where
    patch diff@(v, v') (DFCluster cluster) = let
            newReg = DFLeaf $ reg v [v']
            cluster' = map (patch diff) cluster
        in assert (all (\case DFLeaf _ -> True; _ -> False) cluster) -- patch DataFlowGraph with subclusters is not support
            $ DFCluster $ newReg : cluster'
    patch diff@(v, _) n@(DFLeaf f)
        | v `member` inputs f = DFLeaf $ patch diff f
        | otherwise = n

instance ( Var v ) => Variables (DataFlowGraph v x) v where
    variables (DFLeaf fb)   = variables fb
    variables (DFCluster g) = unionsMap variables g

instance WithFunctions (DataFlowGraph v x) (F v x) where
    functions (DFLeaf f)    = [ f ]
    functions (DFCluster g) = concatMap functions g



-- |Synthesis process is finish when all variable from data flow are
-- transferred.
isSynthesisFinish :: ( ProcessorUnit u v x t ) => ModelState u v x -> Bool
isSynthesisFinish ModelState{ mUnit, mDataFlowGraph } = let
        inWork = transferred mUnit
        inAlg = variables mDataFlowGraph
    in inWork == inAlg


targetProcessDuration ModelState{ mUnit } = nextTick $ process mUnit


-- |The simplest way to convert an endpoint synthesis option to a endpoint
-- decision.
endpointOptionToDecision o@EndpointO{ epoRole }
    = let
        a = o^.at.avail.infimum
        -- "-1" - is necessary for reduction transfer time
        b = o^.at.avail.infimum + o^.at.dur.infimum - 1
    in EndpointD epoRole (a ... b)


-- |Processor unit synthesis is finite when all binded function can be
-- processed. Checking is very simple and don't work correct for pipelined
-- units.
isPUSynthesisFinite pu
    = case options endpointDT pu of
        [] -> let
                algVars = unionsMap variables $ functions pu
                processVars = unionsMap variables $ getEndpoints $ process pu
            in algVars == processVars
        o:_ -> let
                d = endpointOptionToDecision o
                pu' = decision endpointDT pu d
            in isPUSynthesisFinite pu'
