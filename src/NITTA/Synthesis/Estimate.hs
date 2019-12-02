{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Synthesis.Estimate
Description : Estimates for a synthesis step
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Synthesis.Estimate
    ( ParametersCntx(..), prepareParametersCntx'
    , Parameters(..), estimateParameters
    , ObjectiveFunctionConf(..), objectiveFunction
    ) where

import           Data.Default
import           Data.List                       (find)
import qualified Data.Map                        as M
import           Data.Maybe
import qualified Data.Set                        as S
import           GHC.Generics
import           NITTA.Intermediate.Types
import           NITTA.Model.Networks.Bus
import           NITTA.Model.Problems.Dataflow
import           NITTA.Model.Problems.Endpoint
import           NITTA.Model.Problems.Refactor
import           NITTA.Model.Problems.Whole
import           NITTA.Model.ProcessorUnits.Time
import           NITTA.Model.TargetSystem        (ModelState (..))
import           NITTA.Model.Types
import           NITTA.Utils
import           Numeric.Interval                (inf, sup)


-- |Synthesis process setup, which determines next synthesis step selection.
newtype ObjectiveFunctionConf
    = ObjectiveFunctionConf
        { threshold :: Int
            -- ^Порог колличества вариантов, после которого пересылка данных
            -- станет приоритетнее, чем привязка функциональных блоков.
        }
    deriving ( Generic, Show, Eq, Ord )

instance Default ObjectiveFunctionConf where
    def = ObjectiveFunctionConf
        { threshold=20
        }


data ParametersCntx m tag v x t
    = ParametersCntx
        { unitModel                 :: m
        , repeatedDecisionNStepBack :: Maybe Int
        , possibleDeadlockBinds     :: S.Set (F v x)
        , transferableVars          :: S.Set v
        , bindingAlternative        :: M.Map (F v x) [tag]
        , numberOfBindOptions       :: Int
        , numberOfDFOptions         :: Int
        , alreadyBindedVariables    :: S.Set v
        , outputOfBindableFunctions :: S.Set v
        , waves                     :: M.Map v Int
        }

prepareParametersCntx' unitModel repeatedDecisionNStepBack = let
        opts = synthesisOptions unitModel
        bindableFunctions = [ f | (Binding f _) <- opts ]

        mkWaves n pool lockedVars
            | pool == S.empty = []
            | pool `S.intersection` lockedVars == S.empty = zip (S.elems lockedVars) (repeat n)
        mkWaves n pool lockedVars
            = let currentWave = pool `S.intersection` lockedVars
            in zip (S.elems currentWave) (repeat n)
                ++ mkWaves (n+1) (pool S.\\ currentWave) (currentWave `S.union` lockedVars)
    in ParametersCntx
        { unitModel=unitModel
        , repeatedDecisionNStepBack
        , possibleDeadlockBinds = S.fromList
            [ f
            | (Binding f tag) <- opts
            , Lock{ lockBy } <- locks f
            , lockBy `S.member` unionsMap variables (bindedFunctions tag $ mUnit unitModel)
            ]
        , transferableVars = S.fromList
            [ v
            | (Dataflow _ targets) <- opts
            , (v, Just _) <- M.assocs targets
            ]
        , alreadyBindedVariables = variables $ mUnit unitModel
        , waves = M.fromList $ mkWaves 0
            (unionsMap variables bindableFunctions)
            (S.fromList (map locked $ concatMap locks bindableFunctions))
        , bindingAlternative=foldl
            ( \st (Binding f tag) -> M.alter (return . maybe [tag] (tag:)) f st )
            M.empty
            $ filter isBinding opts
        , numberOfBindOptions=length $ filter isBinding opts
        , numberOfDFOptions=length $ filter isDataFlow opts
        , outputOfBindableFunctions=unionsMap outputs bindableFunctions
        }


-----------------------------------------------------------
-- *Parameters

data Parameters
    = BindEdgeParameter
        { pCritical                :: Bool
            -- ^Устанавливается для таких функциональных блоков, привязка
            -- которых может быть заблокирована другими. Пример - занятие
            -- Loop-ом адреса, используемого LoopOut.
        , pAlternative             :: Float
            -- ^Number of binding alternatives
        , pRestless                :: Float
            -- ^Привязка данного функционального блока может быть активировано
            -- только спустя указанное колличество тактов.
        , pOutputNumber            :: Float
        , pAllowDataFlow           :: Float
            -- ^Данная операция может быть привязана прямо сейчас и это приведёт
            -- к разрешению указанного количества пересылок.
        , pPossibleDeadlock        :: Bool
            -- ^Если была выполнена привязка функции из серидины алгоритма, то
            -- это может привести к deadlock-у. В такой ситуации может быть
            -- активирована пересылка в вычислительный блок, в то время как
            -- часть из входных данных не доступна, так как требуемая функция
            -- ещё не привязана, а после привязки не сможет быть вычисленна, так
            -- как ресурс уже занят.
        , pNumberOfBindedFunctions :: Float
        , pPercentOfBindedInputs   :: Float
            -- ^number of binded input variables / number of all input variables
        , pWave                    :: Float
        }
    | DataFlowEdgeParameter
        { pWaitTime              :: Float
        , pRestrictedTime        :: Bool
        , pNotTransferableInputs :: [Float]
            -- ^Number of variables, which is not transferable for affected
            -- functions.
        }
    | RefactorEdgeParameter
        { pRefactor          :: Refactor () ()
        , pVarsCount         :: Float
        , pBufferCount       :: Float
        , pNStepBackRepeated :: Maybe Int
        }
    deriving ( Show, Generic )


estimateParameters
        ObjectiveFunctionConf{}
        ParametersCntx{ possibleDeadlockBinds, bindingAlternative, unitModel, alreadyBindedVariables, waves }
        (Binding f tag)
    = BindEdgeParameter
        { pCritical=isInternalLockPossible f
        , pAlternative=fromIntegral $ length (bindingAlternative M.! f)
        , pAllowDataFlow=fromIntegral $ length $ unionsMap variables $ filter isTarget $ optionsAfterBind f tag unitModel
        , pRestless=fromMaybe 0 $ do
            (_var, tcFrom) <- find (\(v, _) -> v `elem` variables f) $ waitingTimeOfVariables unitModel
            return $ fromIntegral tcFrom
        , pOutputNumber=fromIntegral $ length $ S.elems $ outputs f
        , pPossibleDeadlock=f `S.member` possibleDeadlockBinds
        , pNumberOfBindedFunctions=fromIntegral $ length $ bindedFunctions tag $ mUnit unitModel
        , pPercentOfBindedInputs = let
                is = inputs f
                n = fromIntegral $ length $ S.intersection is alreadyBindedVariables
                nAll = fromIntegral $ length is
            in if nAll == 0 then 1 else n / nAll
        , pWave=let
                    allInputs = S.elems $ inputs f
                    ns = map (\v -> fromMaybe 0 (waves M.!? v)) allInputs
                in fromIntegral $ maximum (0 : ns)
        }

estimateParameters
        ObjectiveFunctionConf{}
        ParametersCntx{ transferableVars, unitModel }
        (Dataflow (_, TimeConstrain{ tcAvailable, tcDuration }) target )
    = DataFlowEdgeParameter
        { pWaitTime=fromIntegral (inf tcAvailable)
        , pRestrictedTime=fromEnum (sup tcDuration) /= maxBound
        , pNotTransferableInputs
            = let
                fs = functions unitModel
                vs = S.fromList [ v | (v, Just _) <- M.assocs target ]
                affectedFunctions = filter (\f -> not $ null (inputs f `S.intersection` vs)) fs
                notTransferableVars = map (\f -> inputs f S.\\ transferableVars) affectedFunctions
            in map (fromIntegral . length) notTransferableVars
        }

estimateParameters ObjectiveFunctionConf{} ParametersCntx{ repeatedDecisionNStepBack } (Refactor BreakLoop{})
    = RefactorEdgeParameter
        { pRefactor=BreakLoop def def def
        , pVarsCount=0
        , pBufferCount=0
        , pNStepBackRepeated=repeatedDecisionNStepBack
        }

estimateParameters ObjectiveFunctionConf{} ParametersCntx{ repeatedDecisionNStepBack } (Refactor (ResolveDeadlock vs))
    = RefactorEdgeParameter
        { pRefactor=ResolveDeadlock def
        , pVarsCount=fromIntegral $ S.size vs
        , pBufferCount=fromIntegral $ sum $ map countSuffix $ S.elems vs
        , pNStepBackRepeated=repeatedDecisionNStepBack
        }


-- |Function, which map 'Parameters' to 'Float'.
objectiveFunction
        ObjectiveFunctionConf{ threshold }
        ParametersCntx{ numberOfDFOptions }
        params
    = case params of
        BindEdgeParameter{ pPossibleDeadlock=True } -> 500
        BindEdgeParameter{ pCritical, pAlternative, pAllowDataFlow, pRestless, pNumberOfBindedFunctions, pWave, pPercentOfBindedInputs, pOutputNumber }
            -> 3000
                + pCritical <?> 1000
                + (pAlternative == 1) <?> 500
                + pAllowDataFlow * 10
                + pPercentOfBindedInputs * 50
                - pWave * 50
                - pNumberOfBindedFunctions * 10
                - pRestless * 4
                + pOutputNumber * 2
        DataFlowEdgeParameter{ pWaitTime, pNotTransferableInputs, pRestrictedTime }
            -> 2000
                + (numberOfDFOptions >= threshold) <?> 1000
                + pRestrictedTime <?> 200
                - sum pNotTransferableInputs * 5
                - pWaitTime
        RefactorEdgeParameter{ pNStepBackRepeated=Just _ }
            -> -1
        RefactorEdgeParameter{ pRefactor=BreakLoop{} }
            -> 5000
        RefactorEdgeParameter{ pRefactor=ResolveDeadlock{}, pVarsCount, pBufferCount }
            -> 1000 + pVarsCount - pBufferCount * 1000
        -- _ -> -1


------------------------------------------------------------
-- *Internal

optionsAfterBind f tag ModelState{ mUnit=BusNetwork{ bnPus } }
    = case tryBind f (bnPus M.! tag) of
        Right pu' -> filter (\(EndpointSt act _) -> act `optionOf` f) $ endpointOptions pu'
        _         -> []
    where
        act `optionOf` f' = not $ S.null (variables act `S.intersection` variables f')


waitingTimeOfVariables net =
    [ (variable, inf $ tcAvailable tc)
    | DataflowSt{ dfSource=(_, tc@TimeConstrain{}), dfTargets } <- dataflowOptions net
    , (variable, Nothing) <- M.assocs dfTargets
    ]


True <?> v = v
False <?> _ = 0
