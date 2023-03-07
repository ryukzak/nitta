{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -fno-warn-orphans #-}

{- |
Module      : NITTA.Synthesis.Steps.Bind
Description :
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Synthesis.Steps.Bind (
    BindMetrics (..),
    isBind,
) where

import Data.Aeson (ToJSON)
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Set qualified as S
import Data.Typeable
import GHC.Generics
import NITTA.Intermediate.Types
import NITTA.Model.Networks.Bus
import NITTA.Model.Problems.Bind
import NITTA.Model.Problems.Dataflow
import NITTA.Model.Problems.Endpoint
import NITTA.Model.ProcessorUnits
import NITTA.Model.TargetSystem
import NITTA.Synthesis.Types
import NITTA.Utils
import Numeric.Interval.NonEmpty (inf)

data BindMetrics = BindMetrics
    { pCritical :: Bool
    -- ^Can this binding block another one (for example, one 'Loop' can
    -- take the last free buffer)?
    , pAlternative :: Float
    -- ^How many alternative binding we have?
    , pRestless :: Float
    -- ^How many ticks requires for executing the function?
    , pOutputNumber :: Float
    , pAllowDataFlow :: Float
    -- ^How many transactions can be executed with this function?
    , pPossibleDeadlock :: Bool
    -- ^May this binding cause deadlock?
    , pNumberOfBindedFunctions :: Float
    , pPercentOfBindedInputs :: Float
    -- ^number of binded input variables / number of all input variables
    , pWave :: Maybe Float
    , isGroupBinding :: Bool
    -- ^number of unique PU when group binding
    , uniquePUs :: Maybe Float
    -- ^correction if we can not bind function to PU next tryBind has less points for this function
    , correction :: Float
    }
    deriving (Generic)

instance ToJSON BindMetrics

instance
    (UnitTag tag, VarValTime v x t) =>
    SynthesisDecisionCls
        (SynthesisState (TargetSystem (BusNetwork tag v x t) tag v x t) tag v x t)
        (TargetSystem (BusNetwork tag v x t) tag v x t)
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
        b@(Bind f tag)
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
                , isGroupBinding = False
                , uniquePUs = Nothing
                , correction = case (correctionTable mUnit) M.!? b of
                    Just v -> fromIntegral v
                    _ -> 0
                }
    parameters
        SynthesisState 
            { bindingAlternative
            }
        (GroupBinding _ binds)
        _ =
            BindMetrics
                { pCritical = any (\(Bind f _) -> isInternalLockPossible f) binds
                , pAlternative = bAlternatives
                , pAllowDataFlow = toEnum $ length binds
                , pRestless = 0
                , pOutputNumber = 0
                , pPossibleDeadlock = any (\(_, v) -> checkDeadlock v) (M.toList pu_f_dict)
                , pNumberOfBindedFunctions = toEnum $ length binds
                , pPercentOfBindedInputs = 10
                , pWave = Nothing
                , isGroupBinding = True
                , uniquePUs = Just $ fromIntegral $ length $ L.nub tags
                , correction = 0
                }
            where
                fs = map (\(Bind f _) -> f) binds
                tags = map (\(Bind _ tag) -> tag) binds
                pu_f_list = map (\(Bind f tag) -> (tag, f)) binds
                pu_f_dict = foldl (\m (tag, f) -> M.insertWith (++) tag [f] m ) M.empty pu_f_list

                checkDeadlock fs_ = not $ null deadlocks 
                    where 
                        deadlocks =
                            [ f 
                            | f <- fs_
                            , Lock{lockBy} <- locks f
                            , lockBy `S.member` unionsMap variables (filter (\f_ -> f_ /= f) fs_)
                            ]
                
                bAlternatives = sum $ map (\f -> fromIntegral $ length (bindingAlternative M.! f)) fs

    estimate _ctx _o _d BindMetrics{pPossibleDeadlock = True} = 500
    estimate _ctx _o _d BindMetrics{isGroupBinding = True, pNumberOfBindedFunctions = 1} = 0
    estimate _ctx _o _d BindMetrics{isGroupBinding = True, pCritical = True} = 600
    estimate _ctx _o _d BindMetrics{pCritical, pAlternative, pAllowDataFlow, pRestless, pNumberOfBindedFunctions, pWave, pPercentOfBindedInputs, pOutputNumber, correction, uniquePUs} =
        3000
            + pCritical
            <?> 1000
            + (pAlternative == 1)
            <?> 500
            + pAllowDataFlow
            * 10
            + pPercentOfBindedInputs
            * 50
            - fromMaybe (-1) pWave
            * 50
            - pNumberOfBindedFunctions
            * 10
            - pRestless
            * 4
            + pOutputNumber
            * 2
            - correction
            + fromMaybe (0) uniquePUs
            * 50

waitingTimeOfVariables net =
    [ (variable, inf $ tcAvailable constrain)
    | DataflowSt{dfSource = (_, srcEp), dfTargets} <- dataflowOptions net
    , let constrain = epAt srcEp
    , variable <- S.elems (variables srcEp S.\\ unionsMap (variables . snd) dfTargets)
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
