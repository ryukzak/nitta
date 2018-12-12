{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-orphans #-}

{-|
Module      : NITTA.Types.Synthesis
Description : Types to describe synthesis process
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}

module NITTA.Types.Synthesis
    ( SynthesisNode(..)
    , synthesisNode, synthesisNodeIO
    , Nid(..)
    , nidsTree
    , SynthesisSetup(..)
    , simple
      -- *Processing SynthesisTree
    , getSynthesisNode
    , getSynthesis
    , getSynthesisSubNodesIO, getSynthesisSubNodes
    , SpecialMetrics(..)
    , optionsWithMetrics
    , CompilerDT, compiler
    , WithMetric(..)
    , targetProcessDuration
      -- *Synhesis context
    , SynthCntxCls(..)
    , SynthCntx(..)
    , comment
    , setCntx
    , isSchedulingComplete
    , isSchedulingCompletable
    , findCntx
    , option2decision
    , SynthesisSubNode(..)
    , endpointOption2action
    ) where

import           Control.Arrow          (second)
import           Control.Concurrent.STM
import           Data.List              (find)
import           Data.List.Split
import qualified Data.Map               as M
import           Data.Maybe
import           Data.Proxy
import           Data.Set               (Set, fromList, intersection, member)
import qualified Data.Set               as S
import           Data.Tree
import           Data.Typeable          (Typeable, cast)
import           GHC.Generics
import           NITTA.BusNetwork
import           NITTA.DataFlow         (ModelState (..))
import           NITTA.Types
import           NITTA.Utils
import           NITTA.Utils.Lens
import           Numeric.Interval       (Interval, (...))



data SynthesisNode title v x t
    = SynthesisNode
        { sModel     :: ModelState title v x t
        , sCntx      :: [SynthCntx]
        , isComplete :: Bool
        , sSubNodes  :: TVar (Maybe [SynthesisSubNode title v x t])
        }
    deriving ( Generic )


data SynthesisSubNode title v x t
    = SubNode
        { subNode         :: SynthesisNode title v x t
        , characteristic  :: Float
        , characteristics :: SpecialMetrics
        }
    deriving ( Generic )



-- |Create initial synthesis.
synthesisNodeIO m = atomically $ synthesisNode m

synthesisNode m = do
    sSubNodes <- newTVar Nothing
    return SynthesisNode
        { sModel=m
        , isComplete=isSchedulingComplete m
        , sCntx=[]
        , sSubNodes
        }



getSynthesisSubNodesIO node = atomically $ getSynthesisSubNodes node

getSynthesisSubNodes SynthesisNode{ sModel, sSubNodes } = do
    optsM <- readTVar sSubNodes
    case optsM of
        Just opts -> return opts
        Nothing -> do
            let opts = optionsWithMetrics simple sModel
            subNodes <- mapM (\WithMetric{ mDecision, mIntegral, mSpecial } -> do
                node <- synthesisNode $ decision compiler sModel mDecision
                return SubNode
                    { subNode=node
                    , characteristic=fromIntegral mIntegral
                    , characteristics=mSpecial
                    }
                ) opts
            writeTVar sSubNodes $ Just subNodes
            return subNodes



-- |Synthesis identical.
newtype Nid = Nid [Int]
nidSep = ':'

instance Show Nid where
    show (Nid []) = [nidSep]
    show (Nid is) = show' is
        where
            show' []     = ""
            show' (x:xs) = nidSep : show x ++ show' xs

instance Read Nid where
    readsPrec _ [x] | x == nidSep    = [(Nid [], "")]
    readsPrec d (x:xs)
        | x == nidSep
        , let is = map (readsPrec d) $ splitOn [nidSep] xs
        , all (not . null) is
        = [(Nid $ map fst $ concat is, "")]
    readsPrec _ _ = []


nidsTree = inner []
    where
        inner is Node{ subForest } = Node
            { rootLabel=Nid $ reverse is
            , subForest=zipWith (\i subN -> inner (i:is) subN) [0..] subForest
            }





targetProcessDuration Frame{ processor } = nextTick $ process processor


-- *Synthesis context

class SynthCntxCls a where
    data SynthCntx' a :: *

data SynthCntx = forall a. ( Show (SynthCntx' a), Typeable (SynthCntx' a) ) => SynthCntx (SynthCntx' a)

instance Show SynthCntx where
    show (SynthCntx e) = show e

findCntx [] = Nothing
findCntx (SynthCntx c : cs)
    | Just cntx <- cast c = Just cntx
    | otherwise = findCntx cs

setCntx newCntx [] = [SynthCntx newCntx]
setCntx newCntx (SynthCntx c : cs)
    | Just c' <- cast c
    , let _ = c' `asTypeOf` newCntx
    = SynthCntx newCntx : cs
    | otherwise
    = SynthCntx c : setCntx newCntx cs


--------------------

data Comment

instance SynthCntxCls Comment where
    data SynthCntx' Comment = Comment String
        deriving ( Show )

comment = SynthCntx . Comment



-- *Processing

-- |Get specific by @nid@ node from a synthesis tree.
getSynthesisNode (Nid []) n = n
getSynthesisNode nid@(Nid (i:is)) Node{ subForest }
    | length subForest <= i = error $ "getSynthesisNode: wrong nid: " ++ show nid
    | otherwise = getSynthesisNode (Nid is) (subForest !! i)

getSynthesis nid n = rootLabel $ getSynthesisNode nid n



---------------------------------------------------------------------
-- *Compiler Decision Type


data CompilerDT title v x t
compiler = Proxy :: Proxy CompilerDT



instance DecisionType (CompilerDT title v x t) where
    data Option (CompilerDT title v x t)
        = BindingOption (F v x) title
        | DataFlowOption (Source title (TimeConstrain t)) (Target title v (TimeConstrain t))
        deriving ( Generic, Show )

    data Decision (CompilerDT title v x t)
        = BindingDecision (F v x) title
        | DataFlowDecision (Source title (Interval t)) (Target title v (Interval t))
        deriving ( Generic, Show )

isBinding = \case BindingOption{} -> True; _ -> False
isDataFlow = \case DataFlowOption{} -> True; _ -> False

specializeDataFlowOption (DataFlowOption s t) = DataFlowO s t
specializeDataFlowOption _ = error "Can't specialize non DataFlow option!"

generalizeDataFlowOption (DataFlowO s t) = DataFlowOption s t
generalizeBindingOption (BindingO s t) = BindingOption s t



instance ( Var v, Typeable x, Time t
         ) => DecisionProblem (CompilerDT String v x t)
                   CompilerDT (ModelState String v x t)
        where
    options _ f@Frame{ processor }
        =  map generalizeBindingOption (options binding f)
        ++ map generalizeDataFlowOption (options dataFlowDT processor)

    decision _ fr (BindingDecision f title) = decision binding fr $ BindingD f title
    decision _ fr@Frame{ processor } (DataFlowDecision src trg) = fr{ processor=decision dataFlowDT processor $ DataFlowD src trg }

option2decision (BindingOption fb title) = BindingDecision fb title
option2decision (DataFlowOption src trg)
    = let
        pushTimeConstrains = map snd $ catMaybes $ M.elems trg
        pullStart    = maximum $ (snd src^.avail.infimum) : map (\o -> o^.avail.infimum) pushTimeConstrains
        pullDuration = maximum $ map (\o -> o^.dur.infimum) $ snd src : pushTimeConstrains
        pullEnd = pullStart + pullDuration - 1
        pushStart = pullStart
        mkEvent (from_, tc) = Just (from_, pushStart ... (pushStart + tc^.dur.infimum - 1))
        pushs = map (second $ maybe Nothing mkEvent) $ M.assocs trg
    in DataFlowDecision ( fst src, pullStart ... pullEnd ) $ M.fromList pushs



-----------------------------------------------------------
-- *Metrics


-- |Synthesis process setup, which determines next synthesis step selection.
data SynthesisSetup
    = Simple
        { -- |Порог колличества вариантов, после которого пересылка данных станет приоритетнее, чем
          -- привязка функциональных блоков.
          threshhold :: Int
        }
    | UnambiguousBind
    deriving ( Generic, Show, Eq, Ord )

simple = Simple
    { threshhold=2
    }


data WithMetric dt -- FIXME: rename to SubNode
    = WithMetric
        { mOption   :: Option dt
        , mDecision :: Decision dt
        , mSpecial  :: SpecialMetrics
        , mIntegral :: Int
        }


-- | Метрики для принятия решения компилятором.
data SpecialMetrics
    = -- | Решения о привязке функциональных блоков к ВУ.
        BindingMetrics
        { -- |Устанавливается для таких функциональных блоков, привязка которых может быть заблокирована
          -- другими. Пример - занятие Loop-ом адреса, используемого FramInput.
          critical         :: Bool
          -- |Колличество альтернативных привязок для функционального блока.
        , alternative      :: Int
          -- |Привязка данного функционального блока может быть активировано только спустя указанное
          -- колличество тактов.
        , restless         :: Int
          -- |Данная операция может быть привязана прямо сейчас и это приведёт к разрешению указанного
          -- количества пересылок.
        , allowDataFlow    :: Int
          -- |Если была выполнена привязка функции из серидины алгоритма, то это может
          -- привести к deadlock-у. В такой ситуации может быть активирована пересылка
          -- в вычислительный блок, в то время как часть из входных данных не доступна,
          -- так как требуемая функция ещё не привязана, а после привязки не сможет быть
          -- вычисленна, так как ресурс уже занят.
        , possibleDeadlock :: Bool
        }
    | DataFlowMetrics { waitTime :: Int, restrictedTime :: Bool }
    deriving ( Show, Generic )



data MeasureCntx f o m
    = MeasureCntx
        { possibleDeadlockBinds :: Set f
        , numberOfBindOptions   :: Int
        , numberOfDFOptions     :: Int
        , dataflowThreshhold    :: Int
        , allOptions            :: o
        , model                 :: m
        }


optionsWithMetrics Simple{ threshhold } model@Frame{ processor=BusNetwork{ bnBinded } }
    = let
        allOptions = options compiler model

        notBindedFunctions = map (\(BindingOption f _) -> f) $ filter isBinding allOptions
        bindedVar = unionsMap variables $ concat $ M.elems bnBinded
        possibleDeadlockBinds = fromList
            [ f
            | f <- notBindedFunctions
            , Lock{ locked } <- locks f
            , locked `member` bindedVar
            ]

        cntx = MeasureCntx
            { numberOfBindOptions=length $ filter isBinding allOptions
            , numberOfDFOptions=length $ filter isDataFlow allOptions
            , dataflowThreshhold=threshhold
            , possibleDeadlockBinds
            , allOptions
            , model
            }

        measure' mOption
            | let mSpecial = measure cntx mOption
            = WithMetric
                { mOption
                , mDecision=option2decision mOption
                , mSpecial
                , mIntegral=integral cntx mSpecial
                }
    in map measure' allOptions

optionsWithMetrics _ _ = error "Wrong setup for compiler!"



measure
        MeasureCntx{ possibleDeadlockBinds, allOptions, model=Frame{ processor=net@BusNetwork{ bnPus } } }
        (BindingOption f title)
    = BindingMetrics
        { critical=isCritical f
        , alternative=length (howManyOptionAllow (filter isBinding allOptions) M.! f)
        , allowDataFlow=sum $ map (length . variables) $ filter isTarget $ optionsAfterBind f (bnPus M.! title)
        , restless=fromMaybe 0 $ do
            (_var, tcFrom) <- find (\(v, _) -> v `elem` variables f) $ waitingTimeOfVariables net
            return $ fromEnum tcFrom
        , possibleDeadlock=f `member` possibleDeadlockBinds
        }
measure MeasureCntx{} opt@DataFlowOption{} = DataFlowMetrics
    { waitTime=fromEnum (specializeDataFlowOption opt^.at.avail.infimum)
    , restrictedTime=fromEnum (specializeDataFlowOption opt^.at.dur.supremum) /= maxBound
    }

integral MeasureCntx{} BindingMetrics{ possibleDeadlock=True }            = 2000000
integral MeasureCntx{ numberOfDFOptions, dataflowThreshhold } DataFlowMetrics{ waitTime }
    | numberOfDFOptions >= dataflowThreshhold                             = 10000 + 200 - waitTime
integral MeasureCntx{} BindingMetrics{ critical=True }                    = 2000
integral MeasureCntx{} BindingMetrics{ alternative=1 }                    = 500
integral MeasureCntx{} BindingMetrics{ allowDataFlow, restless }          = 200 + allowDataFlow * 10 - restless * 2
integral MeasureCntx{} DataFlowMetrics{ restrictedTime } | restrictedTime = 200 + 100
integral MeasureCntx{} DataFlowMetrics{ waitTime }                        = 200 - waitTime



-- | Подсчитать, сколько вариантов для привязки функционального блока определено.
-- Если вариант всего один, может быть стоит его использовать сразу?
howManyOptionAllow bOptions
    = foldl ( \st (BindingOption fb title) -> M.alter (countOption title) fb st ) (M.fromList []) bOptions
    where
        countOption title (Just titles) = Just $ title : titles
        countOption title Nothing       = Just [ title ]


-- | Время ожидания переменных.
waitingTimeOfVariables net =
    [ (variable, tc^.avail.infimum)
    | DataFlowO{ dfoSource=(_, tc@TimeConstrain{}), dfoTargets } <- options dataFlowDT net
    , (variable, Nothing) <- M.assocs dfoTargets
    ]


-- | Оценить, сколько новых вариантов развития вычислительного процесса даёт привязка
-- функциоанльного блока.
optionsAfterBind fb pu = case tryBind fb pu of
    Right pu' -> filter (\(EndpointO act _) -> act `optionOf` fb) $ options endpointDT pu'
    _         -> []
    where
        act `optionOf` fb' = not $ S.null (variables act `intersection` variables fb')


-- * Утилиты

endpointOption2action o@EndpointO{ epoRole }
    = let
        a = o^.at.avail.infimum
        -- "-1" - необходимо, что бы не затягивать процесс на лишний такт, так как интервал включает
        -- граничные значения.
        b = o^.at.avail.infimum + o^.at.dur.infimum - 1
    in EndpointD epoRole (a ... b)


isSchedulingComplete Frame{ processor, dfg }
    | let inWork = S.fromList $ transfered processor
    , let inAlg = variables dfg
    = inWork == inAlg



-- | Проверка является процесс планирования вычислительного процесса полностью завершимым (все
-- функционаные блоки могут быть выполнены). Данная функция используется для проверки возможности
-- привязки функционального блока.
isSchedulingCompletable pu
    = case options endpointDT pu of
        (o:_os) -> let
                d = endpointOption2action o
                pu' = decision endpointDT pu d
                in isSchedulingCompletable pu'
        _ -> let
                algVars = unionsMap variables $ functions pu
                processVars = unionsMap variables $ getEndpoints $ process  pu
            in algVars == processVars
