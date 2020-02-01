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
    ( prepareEstimationCntx
    , Parameters(..), estimateParameters
    , ObjectiveFunctionConf(..), objectiveFunction
    ) where

import           Data.Default
import qualified Data.List                       as L
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


-- |EstimationCntx contains some data about the node, for which we need to
-- estimate all edges
data EstimationCntx m tag v x t
    = EstimationCntx
        { unitModel                  :: m
            -- ^unit under the synthesis process
        , numberOfBindOptions        :: Int
            -- ^number of binding options
        , numberOfDataflowOptions    :: Int
            -- ^number of dataflow options
        , alreadyBindedVariables     :: S.Set v
            -- ^a variable set of all already binded variables
        , bindWaves                  :: M.Map v Int
            -- ^if algorithm will be represented as a graph, where nodes -
            -- variables of not binded functions, edges - casuality, wave is a
            -- minimal number of a step from an initial node to selected

        , transferableVars           :: S.Set v
            -- ^a variable set, which can be transferred on the current
            -- synthesis step
        , decisionDuplicateNStepBack :: Maybe Int
            -- ^ how many synthesis step back require for finding a decision
            -- duplicate
        , possibleDeadlockBinds      :: S.Set (F v x)
            -- ^a function set, which binding may cause dead lock
        , bindingAlternative         :: M.Map (F v x) [tag]
            -- ^a map from functions to possible processor unit tags
        }

-- |The synthesis option estimation require many data about a synthesis tree,
-- current model state and available options. That function allow to create a
-- single context for all edges of the current node.
--
-- Require:
--
-- - a unit model;
-- - how many synthesis step back require for decision duplicate.
prepareEstimationCntx unitModel@ModelState{ mUnit, mDataFlowGraph } decisionDuplicateNStepBack
    = let
        opts = synthesisOptions unitModel
        bindableFunctions = [ f | (Binding f _) <- opts ]

    in EstimationCntx
        { unitModel
        , decisionDuplicateNStepBack
        , possibleDeadlockBinds = S.fromList
            [ f
            | (Binding f tag) <- opts
            , Lock{ lockBy } <- locks f
            , lockBy `S.member` unionsMap variables (bindedFunctions tag mUnit)
            ]
        , transferableVars = S.fromList
            [ v
            | (Dataflow _ targets) <- opts
            , (v, Just _) <- M.assocs targets
            ]
        , alreadyBindedVariables = variables mUnit
        , bindWaves=estimateWaves
               (functions mDataFlowGraph)
               (S.elems (variables mUnit S.\\ unionsMap variables bindableFunctions))
        , bindingAlternative=foldl
            ( \st (Binding f tag) -> M.alter (return . maybe [tag] (tag:)) f st )
            M.empty
            $ filter isBinding opts
        , numberOfBindOptions=length $ filter isBinding opts
        , numberOfDataflowOptions=length $ filter isDataFlow opts
        }


-- |see usage for 'bindWaves' above
estimateWaves fs alreadyVars = let
        io = [ ( is, os )
            | f <- fs
            , let is = (S.elems $ inputs f) L.\\ alreadyVars
                  os = (S.elems $ outputs f)
            ]
    in estimateWaves' io 0 def

estimateWaves' [] _n acc = acc
estimateWaves' io n acc = let
        (first, another) = L.partition (null . fst) io
        firstVs = concatMap snd first
        io' = map (\(is, os) -> (is L.\\ firstVs, os)) another
        acc' = M.union acc $ M.fromList $ map (\v -> (v, n)) firstVs
    in if null first
          then acc -- in case of cycle (maybe some loops are not broken)
          else estimateWaves' io' (n+1) acc'


-----------------------------------------------------------
-- *Parameters

-- |Parameters for estimation of synthesis steps.
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
        , pWave                    :: Maybe Float
        }
    | DataFlowEdgeParameter
        { pWaitTime              :: Float
        , pRestrictedTime        :: Bool
        , pNotTransferableInputs :: [Float]
            -- ^number of variables, which is not transferable for affected
            -- functions.
        }
    -- TODO: split to separated constructor or separated refactor type?
    | RefactorEdgeParameter
        { pRefactorType                  :: Refactor () ()
        , pNumberOfLockedVariables       :: Float
        , pBufferCount                   :: Float
        , pNStepBackRepeated             :: Maybe Int
        , pNumberOfTransferableVariables :: Float
        }
    deriving ( Show, Generic )


estimateParameters
        ObjectiveFunctionConf{}
        EstimationCntx{ possibleDeadlockBinds, bindingAlternative, unitModel, alreadyBindedVariables, bindWaves }
        (Binding f tag)
    = BindEdgeParameter
        { pCritical=isInternalLockPossible f
        , pAlternative=fromIntegral $ length (bindingAlternative M.! f)
        , pAllowDataFlow=fromIntegral $ length $ unionsMap variables $ filter isTarget $ optionsAfterBind f tag unitModel
        , pRestless=fromMaybe 0 $ do
            (_var, tcFrom) <- L.find (\(v, _) -> v `elem` variables f) $ waitingTimeOfVariables unitModel
            return $ fromIntegral tcFrom
        , pOutputNumber=fromIntegral $ length $ S.elems $ outputs f
        , pPossibleDeadlock=f `S.member` possibleDeadlockBinds
        , pNumberOfBindedFunctions=fromIntegral $ length $ bindedFunctions tag $ mUnit unitModel
        , pPercentOfBindedInputs = let
                is = inputs f
                n = fromIntegral $ length $ S.intersection is alreadyBindedVariables
                nAll = fromIntegral $ length is
            in if nAll == 0 then 1 else n / nAll
        , pWave=fmap fromIntegral $ case map (bindWaves M.!?) $ S.elems $ inputs f of
                    []                       -> Just 0
                    waves | all isJust waves -> Just $ maximum $ catMaybes waves
                    _                        -> Nothing
        }

estimateParameters
        ObjectiveFunctionConf{}
        EstimationCntx{ transferableVars, unitModel }
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

estimateParameters ObjectiveFunctionConf{} EstimationCntx{ decisionDuplicateNStepBack } (Refactor BreakLoop{})
    = RefactorEdgeParameter
        { pRefactorType=BreakLoop def def def
        , pNumberOfLockedVariables=0
        , pBufferCount=0
        , pNStepBackRepeated=decisionDuplicateNStepBack
        , pNumberOfTransferableVariables=0
        }

estimateParameters ObjectiveFunctionConf{} EstimationCntx{ decisionDuplicateNStepBack, transferableVars } (Refactor (ResolveDeadlock vs))
    = RefactorEdgeParameter
        { pRefactorType=ResolveDeadlock def
        , pNumberOfLockedVariables=fromIntegral $ S.size vs
        , pBufferCount=fromIntegral $ sum $ map countSuffix $ S.elems vs
        , pNStepBackRepeated=decisionDuplicateNStepBack
        , pNumberOfTransferableVariables=fromIntegral (S.size $ vs `S.intersection` transferableVars)
        }


-- |Synthesis process setup, which determines next synthesis step selection.
newtype ObjectiveFunctionConf
    = ObjectiveFunctionConf
        { threshold :: Int
            -- ^the threshold of a dataflow options number after which dataflow
            -- options have bonus
        }
    deriving ( Generic, Show, Eq, Ord )

instance Default ObjectiveFunctionConf where
    def = ObjectiveFunctionConf
        { threshold=20
        }


-- |Function, which map 'Parameters' to 'Float'.
objectiveFunction
        ObjectiveFunctionConf{ threshold }
        EstimationCntx{ numberOfDataflowOptions }
        params
    = case params of
        BindEdgeParameter{ pPossibleDeadlock=True } -> 500
        BindEdgeParameter{ pCritical, pAlternative, pAllowDataFlow, pRestless, pNumberOfBindedFunctions, pWave, pPercentOfBindedInputs, pOutputNumber }
            -> 3000
                + pCritical <?> 1000
                + (pAlternative == 1) <?> 500
                + pAllowDataFlow * 10
                + pPercentOfBindedInputs * 50
                - (fromMaybe (-1) pWave) * 50
                - pNumberOfBindedFunctions * 10
                - pRestless * 4
                + pOutputNumber * 2
        DataFlowEdgeParameter{ pWaitTime, pNotTransferableInputs, pRestrictedTime }
            -> 2000
                + (numberOfDataflowOptions >= threshold) <?> 1000
                + pRestrictedTime <?> 200
                - sum pNotTransferableInputs * 5
                - pWaitTime
        RefactorEdgeParameter{ pNStepBackRepeated=Just _ }
            -> -1
        RefactorEdgeParameter{ pRefactorType=BreakLoop{} }
            -> 5000
        RefactorEdgeParameter{ pRefactorType=ResolveDeadlock{}, pNumberOfLockedVariables, pBufferCount, pNumberOfTransferableVariables }
            -> 1000 + pNumberOfLockedVariables - pBufferCount * 1000
                - 20 * pNumberOfTransferableVariables
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
