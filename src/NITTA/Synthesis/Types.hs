{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{- |
Module      : NITTA.Synthesis.Types
Description : Synthesis tree representation
Copyright   : (c) Aleksandr Penskoi, 2021
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
module NITTA.Synthesis.Types (
    SynthesisDecisionCls (..),
    Tree (..),
    SynthesisDecision (..),
    SynthesisState (..),
    SynthesisMethodConstraints,
    Sid (..),
    DefTree,
    SynthesisMethod,
    (<?>),
    targetUnit,
    targetDFG,
    defScore,
    mlScoreKeyPrefix,
) where

import Control.Concurrent.STM (TMVar)
import Data.Aeson (ToJSON, toJSON)
import Data.Default
import Data.List.Split
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Text (Text, isPrefixOf)
import Data.Typeable
import NITTA.Intermediate.Analysis (ProcessWave)
import NITTA.Intermediate.Types
import NITTA.Model.Networks.Bus
import NITTA.Model.Problems.Allocation
import NITTA.Model.Problems.Bind
import NITTA.Model.Problems.Dataflow
import NITTA.Model.Problems.Refactor
import NITTA.Model.Problems.ViewHelper
import NITTA.Model.ProcessorUnits.Types (UnitTag)
import NITTA.Model.TargetSystem
import NITTA.Model.Time
import NITTA.UIBackend.ViewHelperCls
import NITTA.Utils.Base
import Servant

-- | Default synthesis tree type.
type DefTree tag v x t =
    Tree (TargetSystem (BusNetwork tag v x t) tag v x t) tag v x t

{- | The synthesis method is a function, which manipulates a synthesis tree. It
receives a node and explores it deeply by IO.
-}
type SynthesisMethod tag v x t = DefTree tag v x t -> IO (DefTree tag v x t)

{- | Shortcut for constraints in signatures of synthesis method functions.
This used to be (VarValTime v x t, UnitTag tag). See below for more info.
-}
type SynthesisMethodConstraints tag v x t = (VarValTimeJSON v x t, ToJSON tag, UnitTag tag)

{- | Synthesis node ID. ID is a relative path, encoded as a sequence of an option
index.
-}
newtype Sid = Sid [Int]

-- | Sid separator for @Show Sid@ and @Read Sid@.
sidSep = '-'

instance Show Sid where
    show (Sid []) = [sidSep]
    show (Sid is) = show' is
        where
            show' [] = ""
            show' (x : xs) = sidSep : show x ++ show' xs

instance Read Sid where
    readsPrec _ [x] | x == sidSep = [(Sid [], "")]
    readsPrec d (x : xs)
        | x == sidSep
        , let is = map (readsPrec d) $ splitOn [sidSep] xs
        , not $ any null is =
            [(Sid $ map fst $ concat is, "")]
    readsPrec _ _ = []

instance Default Sid where
    def = Sid []

instance Semigroup Sid where
    (Sid a) <> (Sid b) = Sid (a <> b)

instance Monoid Sid where
    mempty = Sid []
    mappend = (<>)

instance ToJSON Sid where
    toJSON sid = toJSON $ show sid

instance FromHttpApiData Sid where
    parseUrlPiece = Right . readText

-- | Synthesis tree
data Tree m tag v x t = Tree
    { sID :: Sid
    , sState :: SynthesisState m tag v x t
    , sDecision :: SynthesisDecision (SynthesisState m tag v x t) m
    , sSubForestVar :: TMVar [Tree m tag v x t]
    -- ^ lazy mutable field with different synthesis options and sub nodes
    , isLeaf :: Bool
    , isComplete :: Bool
    }

targetUnit = mUnit . sTarget . sState
targetDFG = mDataFlowGraph . sTarget . sState

data SynthesisDecision ctx m where
    Root :: SynthesisDecision ctx m
    SynthesisDecision ::
        (Typeable p, SynthesisDecisionCls ctx m o d p, Show d, ToJSON p, Viewable d DecisionView) =>
        {option :: o, decision :: d, metrics :: p, scores :: Map Text Float} ->
        SynthesisDecision ctx m

mlScoreKeyPrefix = "ml_"

defScore :: SynthesisDecision ctx m -> Float
defScore sDecision =
    let allScores = scores sDecision
        mlScores = filter (isPrefixOf mlScoreKeyPrefix . fst) $ M.assocs allScores
     in case mlScores of
            [] -> allScores M.! "default"
            (_key, mlScore) : _ -> mlScore

class SynthesisDecisionCls ctx m o d p | ctx o -> m d p where
    decisions :: ctx -> o -> [(d, m)]
    parameters :: ctx -> o -> d -> p
    estimate :: ctx -> o -> d -> p -> Float

data SynthesisState m tag v x t = SynthesisState
    { sParent :: Maybe (Tree m tag v x t)
    , sTarget :: m
    , sAllocationOptions :: [Allocation tag]
    -- ^ PU allocation options cache
    , sBindOptions :: [Bind tag v x]
    -- ^ bind options cache
    , sResolveDeadlockOptions :: [ResolveDeadlock v x]
    , sOptimizeAccumOptions :: [OptimizeAccum v x]
    , sOptimizeLutOptions :: [OptimizeLut v x]
    , sConstantFoldingOptions :: [ConstantFolding v x]
    , sBreakLoopOptions :: [BreakLoop v x]
    , sDataflowOptions :: [DataflowSt tag v (TimeConstraint t)]
    -- ^ dataflow options cache
    , bindingAlternative :: M.Map (F v x) [tag]
    -- ^ a map from functions to possible processor unit tags
    , possibleDeadlockBinds :: S.Set (F v x)
    -- ^ a function set, which binding may cause dead lock
    , bindWaves :: M.Map v Int
    {- ^ if algorithm will be represented as a graph, where nodes -
    variables of not bound functions, edges - casuality, wave is a
    minimal number of a step from an initial node to selected
    -}
    , processWaves :: [ProcessWave v x]
    -- ^ Execution waves of the algorithm. See detailed description in NITTA.Intermediate.Analysis module.
    , numberOfProcessWaves :: Int
    -- ^ Number of execution waves of the algorithm.
    , numberOfDataflowOptions :: Int
    -- ^ number of dataflow options
    , transferableVars :: S.Set v
    {- ^ a variable set, which can be transferred on the current
    synthesis step
    -}
    , unitWorkloadInFunction :: M.Map tag Int
    -- ^ dictionary with number of bound functions for each unit
    }

-- * Utils

True <?> v = v
False <?> _ = 0
