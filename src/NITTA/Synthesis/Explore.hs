{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
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
    isComplete,
) where

import Control.Concurrent.STM
import Control.Monad (forM, unless, when)
import Data.Default
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import NITTA.Intermediate.Types
import NITTA.Model.Networks.Bus
import NITTA.Model.Problems.Bind
import NITTA.Model.Problems.Dataflow
import NITTA.Model.Problems.Refactor
import NITTA.Model.TargetSystem
import NITTA.Synthesis.Bind ()
import NITTA.Synthesis.Dataflow ()
import NITTA.Synthesis.Refactor ()
import NITTA.Synthesis.Types
import NITTA.Utils
import System.Log.Logger

-- |Make synthesis tree
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

-- |Get specific by @nId@ node from a synthesis tree.
getTreeIO tree (SID []) = return tree
getTreeIO tree (SID (i : is)) = do
    subForest <- subForestIO tree
    unless (i < length subForest) $ error "getTreeIO - wrong SID"
    getTreeIO (subForest !! i) (SID is)

-- |Get list of all nodes from root to selected.
getTreePathIO _ (SID []) = return []
getTreePathIO tree (SID (i : is)) = do
    h <- getTreeIO tree $ SID [i]
    t <- getTreePathIO h $ SID is
    return $ h : t

{- |Get all available edges for the node. Edges calculated only for the first
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
                "explore: " <> show sID
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

        return subForest

{- |For synthesis method is more usefull, because throw away all useless trees in
subForest (objective function value less than zero).
-}
positiveSubForestIO tree = filter ((> 0) . score . sDecision) <$> subForestIO tree

-- |Is synthesis tree completed
isComplete
    Tree
        { sState =
            SynthesisState
                { sBindOptions = []
                , sDataflowOptions = []
                , sBreakLoopOptions = []
                , sResolveDeadlockOptions = []
                , sOptimizeAccumOptions = []
                , sTarget
                }
        }
        | isSynthesisFinish sTarget = True
isComplete _ = False

-- *Internal

exploreSubForestVar parent@Tree{sID, sState} =
    let edges =
            concat
                ( map (decisonAndContext parent) (sBindOptions sState)
                    ++ map (decisonAndContext parent) (sDataflowOptions sState)
                    ++ map (decisonAndContext parent) (sBreakLoopOptions sState)
                    ++ map (decisonAndContext parent) (sResolveDeadlockOptions sState)
                    ++ map (decisonAndContext parent) (sOptimizeAccumOptions sState)
                )
     in forM (zip [0 ..] edges) $ \(i, (desc, ctx')) -> do
            sSubForestVar <- newEmptyTMVar
            return
                Tree
                    { sID = sID <> SID [i]
                    , sState = ctx'
                    , sDecision = desc
                    , sSubForestVar
                    }

decisonAndContext parent@Tree{sState = ctx} o =
    [ (SynthesisDecision o d p e, nodeCtx (Just parent) model)
    | (d, model) <- decisions ctx o
    , let p = parameters ctx o d
          e = estimate ctx o d p
    ]

nodeCtx parent nModel =
    let sBindOptions = bindOptions nModel
        sDataflowOptions = dataflowOptions nModel
     in SynthesisState
            { sTarget = nModel
            , sParent = parent
            , sBindOptions
            , sDataflowOptions
            , sResolveDeadlockOptions = resolveDeadlockOptions nModel
            , sBreakLoopOptions = breakLoopOptions nModel
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
            , bindWaves =
                estimateWaves
                    (functions $ mDataFlowGraph nModel)
                    (S.elems (variables (mUnit nModel) S.\\ unionsMap variables sBindOptions))
            , numberOfDataflowOptions = length sDataflowOptions
            , transferableVars =
                S.unions
                    [ variables ep
                    | (DataflowSt _ targets) <- sDataflowOptions
                    , (_, ep) <- targets
                    ]
            }

-- |see usage for 'bindWaves' above
estimateWaves fs alreadyVars =
    let io =
            [ (is, os)
            | f <- fs
            , let is = S.elems (inputs f) L.\\ alreadyVars
                  os = S.elems $ outputs f
            ]
     in inner io 0 def
    where
        inner [] _n acc = acc
        inner io n acc =
            let (first, another) = L.partition (null . fst) io
                firstVs = concatMap snd first
                io' = map (\(is, os) -> (is L.\\ firstVs, os)) another
                acc' = M.union acc $ M.fromList $ map (\v -> (v, n)) firstVs
             in if null first
                    then acc -- in case of cycle (maybe some loops are not broken)
                    else inner io' (n + 1) acc'
