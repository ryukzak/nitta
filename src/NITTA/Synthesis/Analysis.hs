{- |
Module      : NITTA.Synthesis.Analysis
Description : Analysis synthesis proccess.
Copyright   : (c) Daniil Prohorov, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Synthesis.Analysis (
    getTreeInfo,
    TreeInfo (..),
) where

import Control.Concurrent.STM
import Data.HashMap.Strict qualified as HM
import GHC.Generics
import NITTA.Model.TargetSystem (processDuration)
import NITTA.Synthesis.Explore (isComplete, isLeaf)
import NITTA.Synthesis.Types

-- | Metrics of synthesis tree process
data TreeInfo = TreeInfo
    { nodes :: Int
    , success :: Int
    , failed :: Int
    , notProcessed :: Int
    , durationSuccess :: HM.HashMap Int Int
    , stepsSuccess :: HM.HashMap Int Int
    }
    deriving (Generic, Show)

instance Semigroup TreeInfo where
    (<>) synthesisInfo1 synthesisInfo2 =
        let synthesisInfoList = [synthesisInfo1, synthesisInfo2]
            durationSuccessList = map durationSuccess synthesisInfoList
            stepsSuccessList = map stepsSuccess synthesisInfoList
         in TreeInfo
                { nodes = sum $ map nodes synthesisInfoList
                , success = sum $ map success synthesisInfoList
                , failed = sum $ map failed synthesisInfoList
                , notProcessed = sum $ map notProcessed synthesisInfoList
                , durationSuccess = if not $ null durationSuccessList then foldl1 (HM.unionWith (+)) durationSuccessList else HM.empty
                , stepsSuccess = if not $ null stepsSuccessList then foldl1 (HM.unionWith (+)) stepsSuccessList else HM.empty
                }

instance Monoid TreeInfo where
    mempty =
        TreeInfo
            { nodes = 0
            , success = 0
            , failed = 0
            , notProcessed = 0
            , durationSuccess = HM.empty
            , stepsSuccess = HM.empty
            }

getTreeInfo (TVar treeMap) = do
    let nodeList = M.elems treeMap
    subForestInfo <- mconcat <$> mapM getNodeInfo nodeList
    return subForestInfo

getNodeInfo node@Node{sID = Sid sid, sState} = do
    let isSuccess = isComplete node && isLeaf node
    let isFail = (not . isComplete) node && isLeaf node
    let duration = fromEnum $ processDuration $ sTarget $ sState
    let successDepends value field =
            if not isSuccess
                then field
                else HM.alter (Just . maybe 1 (+ 1)) value field
    return $
        TreeInfo
            { nodes = 1
            , success = if isSuccess then 1 else 0
            , failed = if isFail then 1 else 0
            , notProcessed = if isLeaf node then 0 else 1
            , durationSuccess = successDepends duration (durationSuccess mempty)
            , stepsSuccess = successDepends (length sid) (stepsSuccess mempty)
            }
