{- FOURMOLU_DISABLE -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : NITTA.Synthesis.Tree
Description : Synthesis graph representation
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

Synthesis can be represented as a graph (tree), where each 'Node' describes the
target system 'ModelState' and each 'Edge' synthesis decision.

A synthesis graph is very large and calculating and storing it in memory is very
bad idea. Also, working with synthesis graph usually making from the specific
node, not from the root. As a result, synthesis graph design as a explicit lazy
mutable structure implemented by 'TVar'.

From this point of view, the synthesis process is a finding of the best tree
leaf (lowest process duration for finished synthesis), and the best synthesis
method - a method which directly walks over the tree to the best leaf without
wrong steps.
-}
module NITTA.Synthesis.Tree
    ( -- *Synthesis graph
      NId(..), G
    , Node(..), mkRootNodeIO, getNodeIO, getNodePathIO
    , Edge(..), getEdgesIO, getPositiveEdgesIO
    ) where

import           Control.Concurrent.STM
import           Control.Monad ( forM, unless )
import           Data.Default
import           Data.List.Split
import           GHC.Generics
import           NITTA.Model.Networks.Bus
import           NITTA.Model.Problems
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Model.TargetSystem
import           NITTA.Model.Types
import           NITTA.Synthesis.Estimate
import           Numeric.Interval ( Interval )


-- |Type alias for Graph parts, where `e` - graph element (Node or Edge) should
-- be 'Node' or 'Edge';
type G e tag v x t
    = e
        (TargetSystem (BusNetwork tag v x t) v x)
        (SynthesisStatement tag v x (TimeConstrain t))
        (SynthesisStatement tag v x (Interval t))


-- |Synthesis graph ID. ID is a relative path, encoded as a sequence of an
-- option index.
newtype NId = NId [Int]

-- |NId separator for @Show NId@ and @Read NId@.
nIdSep = '-'

instance Show NId where
    show (NId []) = [nIdSep]
    show (NId is) = show' is
        where
            show' []     = ""
            show' (x:xs) = nIdSep : show x ++ show' xs

instance Read NId where
    readsPrec _ [x] | x == nIdSep    = [(NId [], "")]
    readsPrec d (x:xs)
        | x == nIdSep
        , let is = map (readsPrec d) $ splitOn [nIdSep] xs
        , not $ any null is
        = [(NId $ map fst $ concat is, "")]
    readsPrec _ _ = []

instance Semigroup NId where
    (NId a) <> (NId b) = NId (a <> b)

instance Monoid NId where
    mempty = NId []
    mappend = (<>)


-----------------------------------------------------------
-- *Node

data Node m o d
    = Node
        { nId         :: NId
        , nModel      :: m
            -- ^model of target processor
        , nIsComplete :: Bool
            -- ^true when synthesis options are over and all algorithm are
            -- scheduled
        , nOrigin     :: Maybe (Edge m o d)
            -- ^if 'Node' is root - 'Nothing'; if 'Node' is not root - 'Just'
            -- input 'Edge'.
        , nEdges      :: TVar (Maybe [Edge m o d])
            -- ^lazy mutable field with different synthesis options and sub nodes
        }
    deriving ( Generic )


-- |Make synthesis graph (root node).
mkRootNodeIO model = atomically $ do
    nEdges <- newTVar Nothing
    return $ mkNode mempty model Nothing nEdges


mkNode nId nModel nOrigin nEdges = Node
    { nId, nModel
    , nIsComplete=isSynthesisFinish nModel
    , nOrigin
    , nEdges
    }


-- |Get specific by @nId@ node from a synthesis tree.
getNodeIO :: ( UnitTag tag, VarValTime v x t, Semigroup v
    ) => G Node tag v x t -> NId -> IO ( G Node tag v x t )
getNodeIO node (NId []) = return node
getNodeIO node nId@(NId (i:is)) = do
    edges <- getEdgesIO node
    unless (i < length edges) $ error $ "getNode - wrong nId: " ++ show nId
    getNodeIO (eTarget $ edges !! i) (NId is)


-----------------------------------------------------------
-- *Edge

data Edge m o d
    = Edge
        { eTarget                 :: Node m o d
            -- ^edge target
        , eSource                 :: Node m o d
            -- ^edge source
        , eOption                 :: o
        , eDecision               :: d
        , eParameters             :: Parameters
            -- ^parameters of the 'Edge'
        , eObjectiveFunctionValue :: Float
            -- ^objective function value for the 'Edge', which representing
            -- parameters as a number
        }
    deriving ( Generic )


mkEdges
    :: ( UnitTag tag, VarValTime v x t )
    => G Node tag v x t -> STM [ G Edge tag v x t ]
mkEdges eSource@Node{ nId, nModel, nOrigin } = do
    let conf = def
        cntx = prepareEstimationCntx nModel $ nStepBackDecisionRepeated eSource

    forM (zip [0..] $ synthesisOptions nModel) $ \(i, opt) ->
        newTVar Nothing >>= \nEdges ->
        let
            eOption = opt
            eDecision = option2decision eOption
            eParameters = estimateParameters conf cntx eOption
            eObjectiveFunctionValue = objectiveFunction conf cntx eParameters

            eTarget = mkNode (nId <> NId [i]) (synthesisDecision nModel eDecision) (Just origin `asTypeOf` nOrigin) nEdges
            origin = Edge{ eOption, eDecision, eParameters, eObjectiveFunctionValue, eSource, eTarget }
        in return origin


-- |Get all available edges for the node. Edges calculated only for the first
-- call.
getEdgesIO
    :: ( UnitTag tag, VarValTime v x t )
    => G Node tag v x t -> IO [ G Edge tag v x t ]
getEdgesIO node@Node{ nEdges } = atomically $
    readTVar nEdges >>= \case
        Just edges -> return edges
        Nothing -> do
            edges <- mkEdges node
            writeTVar nEdges $ Just edges
            return edges


-- |For synthesis method is more usefull, because throw away all useless edges
-- (objective function value less than zero).
getPositiveEdgesIO n = filter ((> 0) . eObjectiveFunctionValue) <$> getEdgesIO n


-- |Get list of all nodes from root to selected.
getNodePathIO node (NId []) = return [ node ]
getNodePathIO node nId@(NId (i:is)) = do
    edges <- getEdgesIO node
    unless (i < length edges) $ error $ "getNode - wrong nId: " ++ show nId
    nodes <- getNodePathIO (eTarget $ edges !! i) (NId is)
    return $ node : nodes


-- |Is the last decision repeated? If yes - @Just n@, where n how many steps
-- back its happened if not - @Nothing@.
nStepBackDecisionRepeated Node{ nOrigin=Just Edge{ eSource, eDecision } }
    = nStepBackDecisionRepeated' (1 :: Int) eDecision eSource
nStepBackDecisionRepeated _ = Nothing
nStepBackDecisionRepeated' _ _ Node{ nOrigin=Nothing } = Nothing
nStepBackDecisionRepeated' acc d Node{ nOrigin=Just Edge{ eDecision, eSource } }
    | d == eDecision = Just acc
    | otherwise = nStepBackDecisionRepeated' (acc + 1) d eSource
