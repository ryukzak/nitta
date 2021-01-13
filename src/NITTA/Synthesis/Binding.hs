{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -fno-warn-orphans #-}

{- |
Module      : NITTA.Synthesis.Binding
Description :
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Synthesis.Binding (
    BindMetrics (..),
    isBind,
) where

import Data.Aeson (ToJSON)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Data.Typeable
import GHC.Generics
import NITTA.Intermediate.Types
import NITTA.Model.Networks.Bus
import NITTA.Model.Problems.Binding
import NITTA.Model.Problems.Dataflow
import NITTA.Model.Problems.Endpoint
import NITTA.Model.ProcessorUnits
import NITTA.Model.TargetSystem
import NITTA.Model.Types
import NITTA.Synthesis.Types
import NITTA.Utils
import Numeric.Interval (inf)

data BindMetrics = BindMetrics
    { -- |Can this binding block another one (for example, one 'Loop' can
      -- take the last free register)?
      pCritical :: Bool
    , -- |How many alternative binding we have?
      pAlternative :: Float
    , -- |How many ticks requires for executing the function?
      pRestless :: Float
    , pOutputNumber :: Float
    , -- |How many transactions can be executed with this function?
      pAllowDataFlow :: Float
    , -- |May this binding cause deadlock?
      pPossibleDeadlock :: Bool
    , pNumberOfBindedFunctions :: Float
    , -- |number of binded input variables / number of all input variables
      pPercentOfBindedInputs :: Float
    , pWave :: Maybe Float
    }
    deriving (Generic)

instance ToJSON BindMetrics

instance
    (UnitTag tag, VarValTime v x t) =>
    SynthesisDecisionCls
        (SynthesisState (TargetSystem (BusNetwork tag v x t) v x) tag v x t)
        (TargetSystem (BusNetwork tag v x t) v x)
        (Bind tag v x)
        (Bind tag v x)
        BindMetrics
    where
    decisions SynthesisState{sTarget} o = [(o, bindDecision sTarget o)]

    parameters
        SynthesisState
            { bindingAlternative
            , sTarget = sTarget@TargetSystem{mUnit}
            , possibleDeadlockBinds
            , bindWaves
            }
        (Bind f tag)
        _ =
            BindMetrics
                { pCritical = isInternalLockPossible f
                , pAlternative = fromIntegral $ length (bindingAlternative M.! f)
                , pAllowDataFlow = fromIntegral $ length $ unionsMap variables $ filter isTarget $ optionsAfterBind f tag sTarget
                , pRestless = fromMaybe 0 $ do
                    (_var, tcFrom) <- L.find (\(v, _) -> v `elem` variables f) $ waitingTimeOfVariables sTarget
                    return $ fromIntegral tcFrom
                , pOutputNumber = fromIntegral $ length $ S.elems $ outputs f
                , pPossibleDeadlock = f `S.member` possibleDeadlockBinds
                , pNumberOfBindedFunctions = fromIntegral $ length $ bindedFunctions tag mUnit
                , pPercentOfBindedInputs =
                    let is = inputs f
                        n = fromIntegral $ length $ S.intersection is $ variables mUnit
                        nAll = fromIntegral $ length is
                     in if nAll == 0 then 1 else n / nAll
                , pWave = fmap fromIntegral $ case map (bindWaves M.!?) $ S.elems $ inputs f of
                    [] -> Just 0
                    waves | all isJust waves -> Just $ maximum $ catMaybes waves
                    _ -> Nothing
                }

    estimate _ctx _o _d BindMetrics{pPossibleDeadlock = True} = 500
    estimate _ctx _o _d BindMetrics{pCritical, pAlternative, pAllowDataFlow, pRestless, pNumberOfBindedFunctions, pWave, pPercentOfBindedInputs, pOutputNumber} =
        3000
            + pCritical <?> 1000
            + (pAlternative == 1) <?> 500
            + pAllowDataFlow * 10
            + pPercentOfBindedInputs * 50
            - fromMaybe (-1) pWave * 50
            - pNumberOfBindedFunctions * 10
            - pRestless * 4
            + pOutputNumber * 2

waitingTimeOfVariables net =
    [ (variable, inf $ tcAvailable tc)
    | DataflowSt{dfSource = (_, tc@TimeConstrain{}), dfTargets} <- dataflowOptions net
    , (variable, Nothing) <- M.assocs dfTargets
    ]

optionsAfterBind f tag TargetSystem{mUnit = BusNetwork{bnPus}} =
    case tryBind f (bnPus M.! tag) of
        Right pu' -> filter (\(EndpointSt act _) -> act `optionOf` f) $ endpointOptions pu'
        _ -> []
    where
        act `optionOf` f' = not $ S.null (variables act `S.intersection` variables f')

isBind SynthesisDecision{metrics}
    | isJust (cast metrics :: Maybe BindMetrics) = True
isBind _ = False
