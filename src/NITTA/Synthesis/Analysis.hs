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
import Data.Maybe (isJust)
import GHC.Generics
import NITTA.Model.ProcessorUnits.Types (NextTick)
import NITTA.Model.TargetSystem (TargetSystem, processDuration)
import NITTA.Model.Time (VarValTime)
import NITTA.Synthesis.Types

-- | Metrics of synthesis tree process
data TreeInfo = TreeInfo
    { nodesVisited :: !Int
    , nodesSuccess :: !Int
    , nodesFailed :: !Int
    , nodesNotProcessed :: !Int
    , targetProcessDuration :: HM.HashMap Int Int
    , synthesisStepsForSuccess :: HM.HashMap Int Int
    }
    deriving (Generic, Show)

instance Semigroup TreeInfo where
    a <> b =
        let ab = [a, b]
            durationSuccessList = map targetProcessDuration ab
            stepsSuccessList = map synthesisStepsForSuccess ab
         in TreeInfo
                { nodesVisited = sum $ map nodesVisited ab
                , nodesSuccess = sum $ map nodesSuccess ab
                , nodesFailed = sum $ map nodesFailed ab
                , nodesNotProcessed = sum $ map nodesNotProcessed ab
                , targetProcessDuration = if not $ null durationSuccessList then foldr1 (HM.unionWith (+)) durationSuccessList else HM.empty
                , synthesisStepsForSuccess = if not $ null stepsSuccessList then foldr1 (HM.unionWith (+)) stepsSuccessList else HM.empty
                }

instance Monoid TreeInfo where
    mempty =
        TreeInfo
            { nodesVisited = 0
            , nodesSuccess = 0
            , nodesFailed = 0
            , nodesNotProcessed = 0
            , targetProcessDuration = HM.empty
            , synthesisStepsForSuccess = HM.empty
            }

getTreeInfo ::
    (VarValTime v x t, NextTick u t) =>
    Tree (TargetSystem u tag v x t) tag v x t ->
    IO TreeInfo
getTreeInfo tree@Tree{sID = Sid sid, sSubForestVar} = do
    subForestM <- atomically $ tryReadTMVar sSubForestVar
    let isProcessed = isJust subForestM
    TreeInfo
        { nodesVisited
        , nodesSuccess
        , nodesFailed
        , targetProcessDuration
        , synthesisStepsForSuccess
        , nodesNotProcessed
        } <-
        maybe (return mempty) (fmap mconcat . mapM getTreeInfo) subForestM

    let (isSuccess, isFail)
            | isLeaf tree = if isComplete tree then (True, False) else (False, True)
            | otherwise = (False, False)

    let duration = fromEnum $ processDuration $ sTarget $ sState tree

    let registerIfSuccess stat value
            | not isSuccess = stat
            | otherwise = HM.alter (Just . maybe 1 (+ 1)) value stat

    return $
        TreeInfo
            { nodesVisited = nodesVisited + 1
            , nodesSuccess = nodesSuccess + if isSuccess then 1 else 0
            , nodesFailed = nodesFailed + if isFail then 1 else 0
            , nodesNotProcessed = nodesNotProcessed + if isProcessed then 0 else 1
            , targetProcessDuration = registerIfSuccess targetProcessDuration duration
            , synthesisStepsForSuccess = registerIfSuccess synthesisStepsForSuccess $ length sid
            }
