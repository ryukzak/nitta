{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{- |
Module      : NITTA.Synthesis.Explore
Description : Explore synthesis tree
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Synthesis.Explore (
    synthesisTreeRootIO,
    getTreeIO,
    getTreePathIO,
    subForestIO,
    positiveSubForestIO,
) where

import Control.Concurrent.STM
import Control.Monad (forM, unless, when)
import Data.Default
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import NITTA.Intermediate.Analysis (buildProcessWaves, estimateVarWaves)
import NITTA.Intermediate.Types
import NITTA.Model.Networks.Bus
import NITTA.Model.Problems.Allocation
import NITTA.Model.Problems.Bind
import NITTA.Model.Problems.Dataflow
import NITTA.Model.Problems.Refactor
import NITTA.Model.TargetSystem
import NITTA.Synthesis.Steps ()
import NITTA.Synthesis.Types
import NITTA.Utils
import System.Log.Logger

-- | Make synthesis tree
synthesisTreeRootIO = atomically . rootSynthesisTreeSTM

rootSynthesisTreeSTM model = do
    sSubForestVar <- newEmptyTMVar
    let sState = nodeCtx Nothing model
    return
        Tree
            { sID = def
            , sState
            , sDecision = Root
            , sSubForestVar
            , isLeaf = isLeaf' sState
            , isComplete = isComplete' sState
            }

-- | Get specific by @nId@ node from a synthesis tree.
getTreeIO tree (Sid []) = return tree
getTreeIO tree (Sid (i : is)) = do
    subForest <- subForestIO tree
    unless (i < length subForest) $ error "getTreeIO - wrong Sid"
    getTreeIO (subForest !! i) (Sid is)

-- | Get list of all nodes from root to selected.
getTreePathIO _ (Sid []) = return []
getTreePathIO tree (Sid (i : is)) = do
    h <- getTreeIO tree $ Sid [i]
    t <- getTreePathIO h $ Sid is
    return $ h : t

{- | Get all available edges for the node. Edges calculated only for the first
call.
-}
subForestIO
    tree@Tree
        { sSubForestVar
        , sID
        , sDecision
        } = do
        (firstTime, subForest) <-
            atomically $
                tryReadTMVar sSubForestVar >>= \case
                    Just subForest -> return (False, subForest)
                    Nothing -> do
                        subForest <- exploreSubForestVar tree
                        putTMVar sSubForestVar subForest
                        return (True, subForest)
        when firstTime $ do
            debugM "NITTA.Synthesis" $
                "explore: "
                    <> show sID
                    <> " score: "
                    <> ( case sDecision of
                            SynthesisDecision{scores} -> show scores
                            _ -> "-"
                       )
                    <> " decision: "
                    <> ( case sDecision of
                            SynthesisDecision{decision} -> show decision
                            _ -> "-"
                       )

        return subForest

{- | For synthesis method is more usefull, because throw away all useless trees in
subForest (objective function value less than zero).
-}
positiveSubForestIO tree = filter ((> 0) . defScore . sDecision) <$> subForestIO tree

isLeaf'
    SynthesisState
        { sAllocationOptions = []
        , sBindOptions = []
        , sDataflowOptions = []
        , sBreakLoopOptions = []
        , sResolveDeadlockOptions = []
        , sOptimizeAccumOptions = []
        , sConstantFoldingOptions = []
        } = True
isLeaf' _ = False

isComplete' = isSynthesisComplete . sTarget

-- * Internal

exploreSubForestVar parent@Tree{sID, sState} =
    let edges =
            concat
                ( map (decisionAndContext parent) (sAllocationOptions sState)
                    ++ map (decisionAndContext parent) (sBindOptions sState)
                    ++ map (decisionAndContext parent) (sDataflowOptions sState)
                    ++ map (decisionAndContext parent) (sBreakLoopOptions sState)
                    ++ map (decisionAndContext parent) (sResolveDeadlockOptions sState)
                    ++ map (decisionAndContext parent) (sOptimizeAccumOptions sState)
                    ++ map (decisionAndContext parent) (sConstantFoldingOptions sState)
                )
     in forM (zip [0 ..] edges) $ \(i, (desc, ctx')) -> do
            sSubForestVar <- newEmptyTMVar
            return
                Tree
                    { sID = sID <> Sid [i]
                    , sState = ctx'
                    , sDecision = desc
                    , sSubForestVar
                    , isLeaf = isLeaf' ctx'
                    , isComplete = isComplete' ctx'
                    }

decisionAndContext parent@Tree{sState = ctx} o =
    [ (SynthesisDecision o d p e, nodeCtx (Just parent) model)
    | (d, model) <- decisions ctx o
    , let p = parameters ctx o d
          e = M.singleton "default" $ estimate ctx o d p
    ]

nodeCtx parent nModel =
    let sBindOptions = bindOptions nModel
        sDataflowOptions = dataflowOptions nModel
        fs = functions $ mDataFlowGraph nModel
        processWaves = buildProcessWaves [] fs
     in SynthesisState
            { sTarget = nModel
            , sParent = parent
            , sAllocationOptions = allocationOptions nModel
            , sBindOptions
            , sDataflowOptions
            , sResolveDeadlockOptions = resolveDeadlockOptions nModel
            , sBreakLoopOptions = breakLoopOptions nModel
            , sConstantFoldingOptions = constantFoldingOptions nModel
            , sOptimizeAccumOptions = optimizeAccumOptions nModel
            , bindingAlternative =
                foldl
                    ( \st b -> case b of
                        (SingleBind uTag f) -> M.alter (return . maybe [uTag] (uTag :)) f st
                        _ -> st
                    )
                    M.empty
                    sBindOptions
            , possibleDeadlockBinds =
                S.fromList
                    [ f
                    | (SingleBind uTag f) <- sBindOptions
                    , Lock{lockBy} <- locks f
                    , lockBy `S.member` unionsMap variables (bindedFunctions uTag $ mUnit nModel)
                    ]
            , bindWaves = estimateVarWaves (S.elems (variables (mUnit nModel) S.\\ unionsMap variables sBindOptions)) fs
            , processWaves
            , numberOfProcessWaves = length processWaves
            , numberOfDataflowOptions = length sDataflowOptions
            , transferableVars =
                S.unions
                    [ variables ep
                    | (DataflowSt _ targets) <- sDataflowOptions
                    , (_, ep) <- targets
                    ]
            , unitWorkloadInFunction =
                let
                    BusNetwork{bnBinded, bnPus} = mUnit nModel
                 in
                    M.fromList
                        $ map
                            ( \uTag -> (uTag, maybe 0 length $ bnBinded M.!? uTag)
                            )
                        $ M.keys bnPus
            }
