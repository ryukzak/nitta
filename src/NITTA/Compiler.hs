{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Compiler
Description : Simple compiler implementation
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}

module NITTA.Compiler
    ( simpleSynthesis, simpleSynthesisIO
    ) where

import           Control.Concurrent.STM
import           Data.List              (find)
import           NITTA.Types.Synthesis
import           NITTA.Utils



-- FIXME: Dijkstra algorithm as a Synthesiser method.

-- |Schedule process by simple synthesis.
simpleSynthesisIO m = atomically $ simpleSynthesis m

simpleSynthesis model = do
    node <- synthesisNode model >>= obliousBindThread >>= allBestThread 1
    return $ sModel node



bestThread node@SynthesisNode{} = do
    subNodes <- getSynthesisSubNodes node
    case subNodes of
        [] -> return node
        _  -> bestThread $ subNode $ maximumOn characteristic subNodes



obliousBindThread node = do
    subNodes <- getSynthesisSubNodes node
    let obliousBind = find
            ((\case
                BindingMetrics{ alternative } -> alternative == 1
                _                             -> False
            ) . characteristics)
            subNodes
    case obliousBind of
        Just SubNode{ subNode } -> obliousBindThread subNode
        Nothing                 -> return node



allBestThread (0 :: Int) node = bestThread node
allBestThread n node = do
    subNodes <- getSynthesisSubNodes node
    lastThreadNodes <- mapM (\SubNode{ subNode } -> allBestThread (n-1) subNode) subNodes
    return $ minimumOn (targetProcessDuration . sModel)
        $ filter isComplete lastThreadNodes



-----------------------------------------------------------
-- LEGACY


-- -- |Get specific by @nid@ node from a synthesis tree.
-- getSynthesisNode (Nid []) n = return n
-- getSynthesisNode nid@(Nid (i:is)) SynthesisNode{ sOptions } = do
--     opts <- liftSTM $ do
--         cached <- isEmptyTMVar sOptions
--         unless cached $ putTMVar sOptions $ optionsWithMetrics simple sModel
--         takeTMVar sOptions
--     when (length opts <= i) $ error $ "getSynthesisNode: wrong nid: " ++ show nid
--     getSynthesisNode (Nid is) (sOptions !! i)



-- simpleSynthesisStep info SynthesisStep{ ix } Synthesis{ sModel }
--     = case optionsWithMetrics simple sModel of
--         [] -> Nothing
--         opts ->
--             let
--                 ix' = fromMaybe (fst $ maximumOn (mIntegral . snd) $ zip [0..] opts) ix
--                 sModel' = decision compiler sModel $ mDecision (opts !! ix')
--             in Just Synthesis
--                 { sModel=sModel'
--                 , sCntx=[ comment info ]
--                 , sStatus=status sModel'
--                 , sCache=def
--                 }
--     where
--         status m
--             | isSchedulingComplete m = Finished
--             | not (isSchedulingComplete m)
--             , null (options compiler m)
--             = DeadEnd
--             | otherwise = InProgress
