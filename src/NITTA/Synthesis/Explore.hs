{-# LANGUAGE GADTs #-}
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
import Data.Text qualified as T
import NITTA.Intermediate.Analysis (buildProcessWaves, estimateVarWaves)
import NITTA.Intermediate.Types
import NITTA.Model.Networks.Bus
import NITTA.Model.Problems.Allocation
import NITTA.Model.Problems.Bind
import NITTA.Model.Problems.Dataflow
import NITTA.Model.Problems.Refactor
import NITTA.Model.TargetSystem
import NITTA.Synthesis.MlBackend.Api
import NITTA.Synthesis.Steps ()
import NITTA.Synthesis.Types
import NITTA.UIBackend.ViewHelper
import NITTA.Utils
import System.Log.Logger

-- | Make synthesis tree
synthesisTreeRootIO = atomically . rootSynthesisTreeSTM

rootSynthesisTreeSTM model = do
    sSubForestVar <- newEmptyTMVar
    return
        Tree
            { sID = def
            , sState = nodeCtx Nothing model
            , sDecision = Root
            , sSubForestVar
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
                            SynthesisDecision{score} -> show score
                            _ -> "-"
                       )
                    <> " decision: "
                    <> ( case sDecision of
                            SynthesisDecision{decision} -> show decision
                            _ -> "-"
                       )

        if null subForest
            then return subForest
            else do
                -- mlBackend <- mlBackendGetter
                -- let mlBackendBaseUrl = baseUrl mlBackend
                let mlBackendBaseUrl = Just (T.pack "http://localhost:8080")
                let modelName = T.pack "production"
                case mlBackendBaseUrl of
                    Nothing -> return subForest
                    Just onlineUrl -> mapSubforestScoreViaMlBackendIO subForest onlineUrl modelName

mapSubforestScoreViaMlBackendIO subForest mlBackendBaseUrl modelName = do
    let input = ScoringInput{scoringTarget = ScoringTargetAll, nodes = [view node | node <- subForest]}
    scores <- predictScoresIO modelName mlBackendBaseUrl [input]
    return $ map (\(node@Tree{sDecision = sDes}, score) -> node{sDecision = sDes{score}}) (zip subForest scores)

{- | For synthesis method is more usefull, because throw away all useless trees in
subForest (objective function value less than zero).
-}
positiveSubForestIO tree = filter ((> 0) . score . sDecision) <$> subForestIO tree

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
                    }

decisionAndContext parent@Tree{sState = ctx} o =
    [ (SynthesisDecision o d p e, nodeCtx (Just parent) model)
    | (d, model) <- decisions ctx o
    , let p = parameters ctx o d
          e = estimate ctx o d p
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
                    (\st (Bind f tag) -> M.alter (return . maybe [tag] (tag :)) f st)
                    M.empty
                    sBindOptions
            , possibleDeadlockBinds =
                S.fromList
                    [ f
                    | (Bind f tag) <- sBindOptions
                    , Lock{lockBy} <- locks f
                    , lockBy `S.member` unionsMap variables (bindedFunctions tag $ mUnit nModel)
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
            }
