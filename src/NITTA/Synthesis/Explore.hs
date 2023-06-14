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
import Control.Exception
import Control.Monad (foldM, forM, unless, when)
import Data.Default
import Data.Map.Strict qualified as M
import Data.Maybe
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
getTreeIO _ tree (Sid []) = return tree
getTreeIO ctx tree (Sid (i : is)) = do
    subForest <- subForestIO ctx tree
    unless (i < length subForest) $ error "getTreeIO - wrong Sid"
    getTreeIO ctx (subForest !! i) (Sid is)

-- | Get list of all nodes from root to selected.
getTreePathIO _ _ (Sid []) = return []
getTreePathIO ctx tree (Sid (i : is)) = do
    h <- getTreeIO ctx tree $ Sid [i]
    t <- getTreePathIO ctx h $ Sid is
    return $ h : t

{- | Get all available edges for the node. Edges calculated only for the first
call.
-}
subForestIO
    ctx
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

        -- FIXME: ML scores are evaluated here every time subForestIO is called. how to cache it like the default score? IO in STM isn't possible.
        -- also it looks inelegant, is there a way to refactor it?
        if null subForest
            then return subForest
            else
                ( let nodeScores' = nodeScores ctx
                   in if null nodeScores'
                        then return subForest
                        else do
                            let mlScoreKeys = filter (\name -> mlScoreKeyPrefix `T.isPrefixOf` name) nodeScores'
                            let modelNames = map (fromJust . T.stripPrefix mlScoreKeyPrefix) mlScoreKeys
                            if null modelNames
                                then return subForest
                                else do
                                    mlBackend <- mlBackendGetter ctx
                                    let mlBackendBaseUrl = baseUrl mlBackend
                                    case mlBackendBaseUrl of
                                        Nothing -> return subForest
                                        Just onlineUrl -> do
                                            -- (addMlScoreToSubforestSkipErrorsIO subForestAccum modelName) gets called for each modelName
                                            foldM (addMlScoreToSubforestSkipErrorsIO onlineUrl) subForest modelNames
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
    let scoreKey = mlScoreKeyPrefix <> modelName
    return $
        map
            ( \(node@Tree{sDecision = sDes@SynthesisDecision{scores = origScores}}, mlScore) ->
                node{sDecision = sDes{scores = M.insert scoreKey mlScore origScores}}
            )
            (zip subForest mlScores)

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
