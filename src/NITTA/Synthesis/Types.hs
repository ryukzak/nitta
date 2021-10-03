{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
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
    SID (..),
    DefTree,
    SynthesisMethod,
    (<?>),
    targetUnit,
    targetDFG,
) where

import Control.Concurrent.STM (TMVar)
import Data.Aeson (ToJSON, toJSON)
import Data.Default
import Data.List.Split
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Typeable
import NITTA.Intermediate.Types
import NITTA.Model.Networks.Bus
import NITTA.Model.Problems.Bind
import NITTA.Model.Problems.Dataflow
import NITTA.Model.Problems.Refactor
import NITTA.Model.Problems.ViewHelper
import NITTA.Model.TargetSystem
import NITTA.Model.Time
import NITTA.UIBackend.ViewHelperCls
import NITTA.Utils.Base
import Servant

-- |Default synthesis tree type.
type DefTree tag v x t =
    Tree (TargetSystem (BusNetwork tag v x t) tag v x t) tag v x t

{- |The synthesis method is a function, which manipulates a synthesis tree. It
receives a node and explores it deeply by IO.
-}
type SynthesisMethod tag v x t = DefTree tag v x t -> IO (DefTree tag v x t)

{- |Synthesis node ID. ID is a relative path, encoded as a sequence of an option
index.
-}
newtype SID = SID [Int]

-- |SID separator for @Show SID@ and @Read SID@.
sidSep = '-'

instance Show SID where
    show (SID []) = [sidSep]
    show (SID is) = show' is
        where
            show' [] = ""
            show' (x : xs) = sidSep : show x ++ show' xs

instance Read SID where
    readsPrec _ [x] | x == sidSep = [(SID [], "")]
    readsPrec d (x : xs)
        | x == sidSep
          , let is = map (readsPrec d) $ splitOn [sidSep] xs
          , not $ any null is =
            [(SID $ map fst $ concat is, "")]
    readsPrec _ _ = []

instance Default SID where
    def = SID []

instance Semigroup SID where
    (SID a) <> (SID b) = SID (a <> b)

instance Monoid SID where
    mempty = SID []
    mappend = (<>)

instance ToJSON SID where
    toJSON sid = toJSON $ show sid

instance FromHttpApiData SID where
    parseUrlPiece = Right . readText

-- |Synthesis tree
data Tree m tag v x t = Tree
    { sID :: SID
    , sState :: SynthesisState m tag v x t
    , sDecision :: SynthesisDecision (SynthesisState m tag v x t) m
    , -- |lazy mutable field with different synthesis options and sub nodes
      sSubForestVar :: TMVar [Tree m tag v x t]
    }

targetUnit = mUnit . sTarget . sState
targetDFG = mDataFlowGraph . sTarget . sState

data SynthesisDecision ctx m where
    Root :: SynthesisDecision ctx m
    SynthesisDecision ::
        (Typeable p, SynthesisDecisionCls ctx m o d p, Show d, ToJSON p, Viewable d DecisionView) =>
        {option :: o, decision :: d, metrics :: p, score :: Float} ->
        SynthesisDecision ctx m

class SynthesisDecisionCls ctx m o d p | ctx o -> m d p where
    decisions :: ctx -> o -> [(d, m)]
    parameters :: ctx -> o -> d -> p
    estimate :: ctx -> o -> d -> p -> Float

data SynthesisState m tag v x t = SynthesisState
    { sParent :: Maybe (Tree m tag v x t)
    , sTarget :: m
    , -- |bind options cache
      sBindOptions :: [Bind tag v x]
    , sResolveDeadlockOptions :: [ResolveDeadlock v x]
    , sOptimizeAccumOptions :: [OptimizeAccum v x]
    , sConstantFoldingOptions :: [ConstantFolding v x]
    , sBreakLoopOptions :: [BreakLoop v x]
    , -- |dataflow options cache
      sDataflowOptions :: [DataflowSt tag v (TimeConstraint t)]
    , -- |a map from functions to possible processor unit tags
      bindingAlternative :: M.Map (F v x) [tag]
    , -- |a function set, which binding may cause dead lock
      possibleDeadlockBinds :: S.Set (F v x)
    , -- |if algorithm will be represented as a graph, where nodes -
      -- variables of not binded functions, edges - casuality, wave is a
      -- minimal number of a step from an initial node to selected
      bindWaves :: M.Map v Int
    , -- |number of dataflow options
      numberOfDataflowOptions :: Int
    , -- |a variable set, which can be transferred on the current
      -- synthesis step
      transferableVars :: S.Set v
    }

-- * Utils

True <?> v = v
False <?> _ = 0
