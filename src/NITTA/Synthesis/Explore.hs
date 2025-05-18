{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiWayIf #-}
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
import Control.Exception
import Control.Monad (foldM, forM, unless, when)
import Data.Default
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Text qualified as T
import Debug.Trace (trace)
import NITTA.Intermediate.Analysis (buildProcessWaves, estimateVarWaves)
import NITTA.Intermediate.Types
import NITTA.Model.Networks.Bus
import NITTA.Model.Problems.Allocation
import NITTA.Model.Problems.Bind
import NITTA.Model.Problems.Dataflow
import NITTA.Model.Problems.Refactor
import NITTA.Model.TargetSystem
import NITTA.Synthesis.MlBackend.Client
import NITTA.Synthesis.MlBackend.ServerInstance
import NITTA.Synthesis.Types
import NITTA.UIBackend.Types
import NITTA.UIBackend.ViewHelper
import NITTA.Utils
import Network.HTTP.Simple
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
getTreeIO _ctx tree (Sid []) = return tree
getTreeIO ctx tree (Sid (i : is)) = do
    subForest <- subForestIO ctx tree
    unless (i < length subForest) $ error "getTreeIO - wrong Sid"
    getTreeIO ctx (subForest !! i) (Sid is)

-- | Get list of all nodes from root to selected.
getTreePathIO _ctx _tree (Sid []) = return []
getTreePathIO ctx tree (Sid (i : is)) = do
    h <- getTreeIO ctx tree $ Sid [i]
    t <- getTreePathIO ctx h $ Sid is
    return $ h : t

{- | Get all available edges for the node. Edges calculated only for the first
call.
-}
subForestIO
    BackendCtx{nodeScores, mlBackendGetter}
    tree@Tree{sSubForestVar} = do
        (firstTime, subForest) <-
            atomically $
                tryReadTMVar sSubForestVar >>= \case
                    Just subForest -> return (False, subForest)
                    Nothing -> do
                        subForest <- exploreSubForestVar tree
                        putTMVar sSubForestVar subForest
                        return (True, subForest)

        when firstTime $ traceProcessedNode tree

        -- FIXME: ML scores are evaluated here every time subForestIO is called. how to cache it like the default score? IO in STM isn't possible.
        -- also it looks inelegant, is there a way to refactor it?
        let modelNames = mapMaybe (T.stripPrefix mlScoreKeyPrefix) nodeScores
        if
            | null subForest -> return subForest
            | null nodeScores -> return subForest
            | null modelNames -> return subForest
            | otherwise -> do
                MlBackendServer{baseUrl} <- mlBackendGetter
                case baseUrl of
                    Nothing -> return subForest
                    Just mlBackendBaseUrl -> do
                        -- (addMlScoreToSubforestSkipErrorsIO subForestAccum modelName) gets called for each modelName
                        foldM (addMlScoreToSubforestSkipErrorsIO mlBackendBaseUrl) subForest modelNames
        where
            traceProcessedNode Tree{sID, sDecision} =
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

addMlScoreToSubforestSkipErrorsIO mlBackendBaseUrl subForest modelName = do
    addMlScoreToSubforestIO mlBackendBaseUrl subForest modelName
        `catch` \e -> do
            errorM "NITTA.Synthesis" $
                "ML backend error: "
                    <> ( case e of
                            JSONConversionException _ resp _ -> show resp
                            _ -> show e
                       )
            return subForest

addMlScoreToSubforestIO mlBackendBaseUrl subForest modelName = do
    let input = ScoringInput{scoringTarget = ScoringTargetAll, nodes = [view node | node <- subForest]}
    allInputsScores <- predictScoresIO modelName mlBackendBaseUrl [input]
    -- +20 shifts "useless node" threshold, since model outputs negative values much more often
    -- FIXME: make models' output consist of mostly >0 values and treat 0 as a "useless node" threshold? training data changes required
    let mlScores = map (+ 20) $ head allInputsScores
        scoreKey = mlScoreKeyPrefix <> modelName

    return $
        map
            (addNewScoreToSubforest scoreKey)
            (zip subForest mlScores)

addNewScoreToSubforest scoreKey (node@Tree{sDecision = sDes@SynthesisDecision{scores = origScores}}, newScore) =
    node{sDecision = sDes{scores = M.insert scoreKey newScore origScores}}
addNewScoreToSubforest scoreKey (node@Tree{sDecision = Root}, _) =
    trace ("adding new score to Root, shouldn't happen, scoreKey: " ++ fromText scoreKey) node

{- | For synthesis method is more usefull, because throw away all useless trees in
subForest (objective function value less than zero).
-}
positiveSubForestIO ctx tree = filter ((> 0) . defScore . sDecision) <$> subForestIO ctx tree

isLeaf'
    SynthesisState
        { sAllocationOptions = []
        , sBindOptions = []
        , sDataflowOptions = []
        , sBreakLoopOptions = []
        , sResolveDeadlockOptions = []
        , sOptimizeAccumOptions = []
        , sOptimizeLutOptions = []
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
                    ++ map (decisionAndContext parent) (sOptimizeLutOptions sState)
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
            , sOptimizeLutOptions = optimizeLutOptions nModel
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
                    , lockBy `S.member` unionsMap variables (boundFunctions uTag $ mUnit nModel)
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
                    BusNetwork{bnBound, bnPus} = mUnit nModel
                 in
                    M.fromList
                        $ map
                            (\uTag -> (uTag, maybe 0 length $ bnBound M.!? uTag))
                        $ M.keys bnPus
            }
