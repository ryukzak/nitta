{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module NITTA.Compiler
  ( bindAll
  , bindAllAndNaiveSchedule
  , compiler
  , CompilerDT
  , CompilerStep(..)
  , isSchedulingComplete
  , naive
  , naive'
  , NaiveOpt(..)
  , option2decision
  , optionsWithMetrics
  , passiveOption2action
  , GlobalMetrics(..)
  , SpecialMetrics(..)
  ) where

import           Control.Arrow    (second)
import           Data.Default
import           Data.List        (find, sortOn)
import qualified Data.Map         as M
import           Data.Maybe
import           Data.Proxy
import           Data.Set         (intersection, member, singleton)
import qualified Data.Set         as S
import           GHC.Generics
import           NITTA.BusNetwork
import           NITTA.DataFlow
import           NITTA.Types
import           NITTA.Utils
import           NITTA.Utils.Lens
import           Numeric.Interval (Interval, (...))


-- | Выполнить привязку списка функциональных блоков к указанному вычислительному блоку.
bindAll fbs pu = foldl (flip bind) pu fbs


-- | Выполнить привязку списка функциональных блоков к указанному вычислительному блоку и наивным
-- образом спланировать вычислительный процесса пасивного блока обработки данных (PUClass Passive).
bindAllAndNaiveSchedule alg pu0 = naiveSchedule $ bindAll alg pu0
  where
    naiveSchedule pu
      | opt : _ <- options endpointDT pu = naiveSchedule $ decision endpointDT pu $ passiveOption2action opt
      | otherwise = pu


-- | Проверка является процесс планирования вычислительного процесса полностью завершимым (все
-- функционаные блоки могут быть выполнены). Данная функция используется для проверки возможности
-- привязки функционального блока.
isSchedulingComplete pu
  = let os = options endpointDT pu
        d = passiveOption2action $ head os
        algVars = unionsMap variables $ functionalBlocks pu
        processVars = unionsMap variables $ getEndpoints $ process pu
    in if null os
        then algVars == processVars
        else isSchedulingComplete $ decision endpointDT pu d
        -- then trace ("end on: " ++ show processVars ++ " " ++ show algVars) $ algVars == processVars
        -- else trace ("continue: " ++ show d ++ " " ++ show os) $ isSchedulingComplete $ decision endpointDT pu d


-- | Настройки процесса компиляции.
newtype NaiveOpt = NaiveOpt
  { -- | Порог колличества вариантов, после которого пересылка данных станет приоритетнее, чем
    -- привязка функциональных блоков.
    threshhold :: Int
  } deriving ( Generic )

instance Default NaiveOpt where
  def = NaiveOpt{ threshhold=1000
                }



---------------------------------------------------------------------
-- * Представление решения компилятора.


data CompilerDT title tag v t
compiler = Proxy :: Proxy CompilerDT


instance DecisionType (CompilerDT title tag v t) where
  data Option (CompilerDT title tag v t)
    = ControlFlowOption (DataFlowGraph v)
    | BindingOption (FB (Parcel v Int)) title
    | DataFlowOption (Source title (TimeConstrain t)) (Target title v (TimeConstrain t))
    deriving ( Generic, Show )

  data Decision (CompilerDT title tag v t)
    = ControlFlowDecision (DataFlowGraph v)
    | BindingDecision (FB (Parcel v Int)) title
    | DataFlowDecision (Source title (Interval t)) (Target title v (Interval t))
    deriving ( Generic, Show )

filterBindingOption opts = [ x | x@BindingOption{} <- opts ]
filterControlFlowOption opts = [ x | x@ControlFlowOption{} <- opts ]
filterDataFlowOption opts = [ x | x@DataFlowOption{} <- opts ]

specializeDataFlowOption (DataFlowOption s t) = DataFlowO s t
specializeDataFlowOption _ = error "Can't specialize non DataFlow option!"

generalizeDataFlowOption (DataFlowO s t) = DataFlowOption s t
generalizeControlFlowOption (ControlFlowO x) = ControlFlowOption x
generalizeBindingOption (BindingO s t) = BindingOption s t



instance ( Time t, Var v
         ) => DecisionProblem (CompilerDT String String v (TaggedTime String t))
                   CompilerDT (SystemState String String v Int (TaggedTime String t))
         where
  options _ Level{ currentFrame } = options compiler currentFrame
  options _ f@Frame{ nitta, dfg } = concat
    [ map generalizeDataFlowOption dataFlowOptions
    , map generalizeControlFlowOption $ options controlDT f
    , map generalizeBindingOption $ options binding f
    ]
    where
      dataFlowOptions = sensibleOptions $ filterByDFG $ options dataFlowDT nitta
      allowByDFG = allowByDFG' dfg
      allowByDFG' (DFGNode fb)        = variables fb
      allowByDFG' (DFG g)             = unionsMap allowByDFG' g
      allowByDFG' DFGSwitch{ dfgKey } = singleton dfgKey
      filterByDFG
        = map (\t@DataFlowO{ dfoTargets } -> t
                  { dfoTargets=M.fromList $ map (\(v, desc) -> (v, if v `member` allowByDFG
                                                                      then desc
                                                                      else Nothing)
                                                ) $ M.assocs dfoTargets
                  })
      sensibleOptions = filter $ \DataFlowO{ dfoTargets } -> any isJust $ M.elems dfoTargets

  decision _ l@Level{ currentFrame } d = tryMakeLevelDone l{ currentFrame=decision compiler currentFrame d }
  decision _ f (BindingDecision fb title) = decision binding f $ BindingD fb title
  decision _ f (ControlFlowDecision d) = decision controlDT f $ ControlFlowD d
  decision _ f@Frame{ nitta } (DataFlowDecision src trg) = f{ nitta=decision dataFlowDT nitta $ DataFlowD src trg }


option2decision (ControlFlowOption cf)   = ControlFlowDecision cf
option2decision (BindingOption fb title) = BindingDecision fb title
option2decision (DataFlowOption src trg)
  = let pushTimeConstrains = map snd $ catMaybes $ M.elems trg
        pullStart    = maximum $ (snd src^.avail.infimum) : map (\o -> o^.avail.infimum) pushTimeConstrains
        pullDuration = maximum $ map (\o -> o^.dur.infimum) $ snd src : pushTimeConstrains
        pullEnd = pullStart + pullDuration - 1
        pushStart = pullStart
        mkEvent (from_, tc) = Just (from_, pushStart ... (pushStart + tc^.dur.infimum - 1))
        pushs = map (second $ maybe Nothing mkEvent) $ M.assocs trg
    in DataFlowDecision ( fst src, pullStart ... pullEnd ) $ M.fromList pushs



---------------------------------------------------------------------
-- * Наивный, но полноценный компилятор.

data CompilerStep title tag v x t
  = CompilerStep
    { state        :: SystemState title tag v x t
    , config       :: NaiveOpt
    , lastDecision :: Maybe (Decision (CompilerDT title tag v t))
    }
  deriving ( Generic )

instance Default (CompilerStep title tag v x t) where
  def = CompilerStep{ state=undefined
                    , config=def
                    , lastDecision=Nothing
                    }


instance ( Time t, Var v
         ) => DecisionProblem (CompilerDT String String v (TaggedTime String t))
                   CompilerDT (CompilerStep String String v Int (TaggedTime String t))
         where
  options proxy CompilerStep{ state } = options proxy state
  decision proxy st@CompilerStep{ state } d = st{ state=decision proxy state d
                                                , lastDecision=Just d }


optionsWithMetrics CompilerStep{ state }
  = reverse $ sortOn (\(x, _, _, _, _) -> x) $ map measure' opts
  where
    opts = options compiler state
    -- gm = measureG (trace (show opts) opts) state
    gm = measureG opts state
    measure' o
      = let m = measure opts state o
        in ( integral gm m, gm, m, o, option2decision o )

naive' st@CompilerStep{ state }
  = if null opts -- ( trace (show opts) opts )
    then Nothing
    -- else Just st{ state=decision compiler state $ trace (show d) d
    else Just st{ state=decision compiler state d
                , lastDecision=Just d
                }
  where
    opts = optionsWithMetrics st
    (_, _, _, _, d) = head opts


naive opt f
  = let st = CompilerStep f opt Nothing
        CompilerStep{ state=st' } = fromMaybe st $ naive' st
    in st'



data GlobalMetrics
  = GlobalMetrics
    { bindingOptions, dataFlowOptions, controlFlowOptions :: Int
    } deriving ( Show, Generic )

measureG opts _
  = GlobalMetrics{ bindingOptions=length $ filterBindingOption opts
                 , dataFlowOptions=length $ filterDataFlowOption opts
                 , controlFlowOptions=length $ filterControlFlowOption opts
                 }

-- | Метрики для принятия решения компилятором.
data SpecialMetrics
  = -- | Решения о привязке функциональных блоков к ВУ.
    BindingMetrics
    { -- | Устанавливается для таких функциональных блоков, привязка которых может быть заблокирована
      -- другими. Пример - занятие Loop-ом адреса, используемого FramInput.
      critical      :: Bool
      -- | Колличество альтернативных привязок для функционального блока.
    , alternative   :: Int
      -- | Привязка данного функционального блока может быть активировано только спустя указанное
      -- колличество тактов.
    , restless      :: Int
      -- | Данная операция может быть привязана прямо сейчас и это приведёт к разрешению указанного
      -- количества пересылок.
    , allowDataFlow :: Int
    }
  | DataFlowMetrics { waitTime :: Int, restrictedTime :: Bool }
  | ControlFlowMetrics
  deriving ( Show, Generic )


measure opts Level{ currentFrame } opt = measure opts currentFrame opt
measure opts Frame{ nitta=net@BusNetwork{ bnPus } } (BindingOption fb title) = BindingMetrics
  { critical=isCritical fb
  , alternative=length (howManyOptionAllow (filterBindingOption opts) M.! fb)
  , allowDataFlow=sum $ map (length . variables) $ filter isTarget $ optionsAfterBind fb (bnPus M.! title)
  , restless=fromMaybe 0 $ do
      (_var, tcFrom) <- find (\(v, _) -> v `elem` variables fb) $ waitingTimeOfVariables net
      return $ fromEnum tcFrom
  }
measure _ _ ControlFlowOption{} = ControlFlowMetrics
measure _ _ opt@DataFlowOption{} = DataFlowMetrics
  { waitTime=fromEnum (specializeDataFlowOption opt^.at.avail.infimum)
  , restrictedTime=fromEnum (specializeDataFlowOption opt^.at.dur.supremum) /= maxBound
  }


integral GlobalMetrics{..} DataFlowMetrics{..}
  | dataFlowOptions >= 2                                   = 10000 + 200 - waitTime
integral GlobalMetrics{..} BindingMetrics{ critical=True } = 2000
integral GlobalMetrics{..} BindingMetrics{ alternative=1 } = 500
integral GlobalMetrics{..} BindingMetrics{..}              = 200 + allowDataFlow * 10 - restless * 2
integral GlobalMetrics{..} DataFlowMetrics{..} | restrictedTime = 200 + 100
integral GlobalMetrics{..} DataFlowMetrics{..}             = 200 - waitTime
integral GlobalMetrics{..} _                               = 0



-- * Работа с потоком управления.


tryMakeLevelDone l@Level{ currentFrame=f@Frame{} }
  | opts <- options compiler f
  , null $ filterBindingOption opts ++ filterDataFlowOption opts
  = makeLevelDone l
tryMakeLevelDone l = l

-- | Функция завершения текущего фрейма. Есть два сценария: 1) поменять фрейм не меняя уровень, 2)
-- свернуть уровень и перейти в нажележащему фрейму.
makeLevelDone l@Level{ remainFrames=f:fs, currentFrame, completedFrames }
  = l
    { currentFrame=f
    , remainFrames=fs
    , completedFrames=currentFrame : completedFrames
    }
makeLevelDone Level{ initialFrame, currentFrame, completedFrames }
  = let fs = currentFrame : completedFrames
        mergeTime = (maximum $ map (nextTick . process . nitta) fs){ tag=timeTag initialFrame }
        Frame{ nitta=net@BusNetwork{ bnProcess } } = currentFrame
    in initialFrame
      { nitta=setTime mergeTime net
          { bnProcess=snd $ modifyProcess bnProcess $
              mapM_ (\Step{ sTime, sDesc } -> addStep sTime sDesc) $ concatMap stepsFromFrame fs
          }
      }
  where
    stepsFromFrame Frame{ timeTag, nitta } = whatsHappenWith timeTag nitta
    stepsFromFrame Level{}   = error "stepsFromFrame: wrong args"

makeLevelDone _ = error "makeFrameDone: argument must be Level."



-- | Подсчитать, сколько вариантов для привязки функционального блока определено.
-- Если вариант всего один, может быть стоит его использовать сразу?
howManyOptionAllow bOptions
  = foldl ( \st (BindingOption fb title) -> M.alter (countOption title) fb st ) (M.fromList []) bOptions
  where
    countOption title (Just titles) = Just $ title : titles
    countOption title Nothing       = Just [ title ]


-- | Время ожидания переменных.
waitingTimeOfVariables net
  = [ (variable, tc^.avail.infimum)
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

passiveOption2action d@EndpointO{ epoRole }
  = let a = d^.at.avail.infimum
        -- "-1" - необходимо, что бы не затягивать процесс на лишний такт, так как интервал включает
        -- граничные значения.
        b = d^.at.avail.infimum + d^.at.dur.infimum - 1
    in EndpointD epoRole (a ... b)



whatsHappenWith tag pu =
  [ st | st@Step{ sTime } <- steps $ process pu
       , tag == placeInTimeTag sTime
       ]
