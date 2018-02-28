{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module NITTA.Compiler
  ( bindAll
  , bindAllAndNaiveSchedule
  , compiler
  , CompilerDT
  , isSchedulingComplete
  , naive
  , NaiveOpt(..)
  , passiveOption2action
  , options2decision
  , option2decision
  ) where

import           Control.Arrow    (second)
import           Data.Default
import           Data.List        (find, intersect, sort, sortBy, sortOn)
import qualified Data.Map         as M
import           Data.Maybe       (catMaybes, isJust, mapMaybe)
import           Data.Proxy
import           GHC.Generics
import           NITTA.BusNetwork
import           NITTA.Flows
import           NITTA.Types
import           NITTA.Utils
import           NITTA.Utils.Lens
import           Numeric.Interval (Interval, (...))


-- | Выполнить привязку списка функциональных блоков к указанному вычислительному блоку.
bindAll fbs pu = either (\l -> error $ "Can't bind FB to PU: " ++ show l) id $ foldl nextBind (Right pu) fbs
  where
    nextBind (Right pu') fb = bind fb pu'
    nextBind (Left r) _     = error r



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
        algVars = sort $ concatMap variables $ functionalBlocks pu
        processVars = sort $ concatMap variables $ getEndpoints $ process pu
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
  def = NaiveOpt{ threshhold=2
                }



---------------------------------------------------------------------
-- * Представление решения компилятора.


data CompilerDT title tag v t
compiler = Proxy :: Proxy CompilerDT


instance DecisionType (CompilerDT title tag v t) where
  data Option (CompilerDT title tag v t)
    = ControlFlowOption (ControlFlow tag v)
    | BindingOption (FB (Parcel v) v) title
    | DataFlowOption (Source title (TimeConstrain t)) (Target title v (TimeConstrain t))
    deriving ( Generic )

  data Decision (CompilerDT title tag v t)
    = ControlFlowDecision (ControlFlow tag v)
    | BindingDecision (FB (Parcel v) v) title
    | DataFlowDecision (Source title (Interval t)) (Target title v (Interval t))
    deriving ( Generic )


option2decision (ControlFlowOption cf)   = ControlFlowDecision cf
option2decision (BindingOption fb title) = BindingDecision fb title
option2decision (DataFlowOption src trg) = networkOption2action $ DataFlowO src trg



getCFOption (ControlFlowOption x) = Just x
getCFOption _                     = Nothing
getBOption (BindingOption fb title) = Just $ BindingO fb title
getBOption _                        = Nothing
getDFOption (DataFlowOption s t) = Just $ DataFlowO s t
getDFOption _                    = Nothing

instance ( Tag tag, Time t, Var v
         ) => DecisionProblem (CompilerDT String tag v (TaggedTime tag t))
                   CompilerDT (BranchedProcess String tag v (TaggedTime tag t))
         where
  options _ Bush{..} = options compiler currentBranch
  options _ branch = concat
    [ map (\(DataFlowO s t) -> DataFlowOption s t) $ dataFlowOptions branch
    , map (\(ControlFlowO x) -> ControlFlowOption x) $ options controlFlowDecision branch
    , map (\(BindingO fb title) -> BindingOption fb title) $ options binding branch
    ]

  decision _ bush@Bush{..} act
    = bush{ currentBranch=decision compiler currentBranch act }
  decision _ branch (BindingDecision fb title)
    = decision binding branch $ BindingD fb title
  decision _ branch (ControlFlowDecision d)
    = decision controlFlowDecision branch $ ControlFlowD d
  decision _ branch@Branch{ topPU=pu, .. } (DataFlowDecision src trg)
    = branch{ topPU=decision dataFlowDT pu $ DataFlowD src trg }




---------------------------------------------------------------------
-- * Наивный, но полноценный компилятор.


naive NaiveOpt{..} branch@Branch{}
  = let d = options2decision branch $ options compiler branch
    in case d of
      Just d' -> decision compiler branch d'
      Nothing -> branch

naive no bush@Bush{..}
  = let bush' = bush{ currentBranch=naive no currentBranch }
    in if isCurrentBranchOver bush'
      then finalizeBranch bush'
      else bush'



-- | Мегафункция принятия решения в процессе компиляции. На вход получает ветку процесса и доступные
-- опции. Выполняет их приоритезацию и выдаёт победителя в качестве результата. В потенциале может
-- реализовываться с использованием IO, то есть - решение принемает пользователь.
options2decision branch opts
  = let bOptions = mapMaybe getBOption opts
        cfOptions = mapMaybe getCFOption opts
        dfOptions = mapMaybe getDFOption opts
    in case () of
      _ | length dfOptions >= 2 -> Just $ dfDecision dfOptions
      _ | not $ null bOptions   -> Just $ mkBindDecision branch bOptions
      _ | not $ null dfOptions  -> Just $ dfDecision dfOptions
      _ | not $ null cfOptions  -> Just $ ControlFlowDecision $ head cfOptions
      _ -> Nothing
  where
    dfDecision dfOptions = case sortOn start dfOptions of
      v : _ -> networkOption2action v
      _     -> error "No variants!"
    start opt = opt^.at.avail.infimum



-- * Работа с потоком данных.


dataFlowOptions Branch{..} = sensibleOptions $ filterByControlModel controlFlow $ options dataFlowDT topPU
  where
    filterByControlModel controlModel opts
      = let cfOpts = allowByControlFlow controlModel
        in map (\t@DataFlowO{..} -> t
                { dfoTargets=M.fromList $ map (\(v, desc) -> (v, if v `elem` cfOpts
                                                                     then desc
                                                                     else Nothing)
                                              ) $ M.assocs dfoTargets
                }) opts
    sensibleOptions = filter $ \DataFlowO{..} -> any isJust $ M.elems dfoTargets

dataFlowOptions _ = error "Can't generate dataflow options for BranchingInProgress."



-- * Работа с потоком управления.



-- | Функция применяется к кусту и позволяет определить, осталась ли работа в текущей ветке или нет.
isCurrentBranchOver Bush{ currentBranch=branch@Branch{..} }
  = let bOptions = options binding branch
        dfOptions = dataFlowOptions branch
    in null bOptions && null dfOptions
isCurrentBranchOver _ = False


-- | Функция позволяет выполнить работы по завершению текущей ветки. Есть два варианта:
--
-- 1) Сменить ветку на следующую.
-- 2) Вернуться в выполнение корневой ветки, для чего слить вычислительный процесс всех вариантов
--    ветвления алгоритма.
finalizeBranch bush@Bush{ remainingBranches=b:bs, ..}
  = bush
    { currentBranch=b
    , remainingBranches=bs
    , completedBranches=currentBranch : completedBranches
    }
finalizeBranch Bush{..}
  = let branchs = currentBranch : completedBranches
        mergeTime = (maximum $ map (nextTick . process . topPU) branchs){ tag=branchTag rootBranch }
        Branch{ topPU=pu@BusNetwork{..} } = currentBranch
    in rootBranch
      { topPU=setTime mergeTime pu
          { bnProcess=snd $ modifyProcess bnProcess $
              mapM_ (\Step{..} -> add sTime sDesc) $ concatMap inBranchSteps branchs
          }
      }
finalizeBranch Branch{} = error "finalizeBranch: wrong args."



-- * Работа с привязкой функциональных блоков к вычислительным блокам.

mkBindDecision Branch{ topPU=net@BusNetwork{..} } bOptions
  = case sort' $ map prioritize' bOptions of
      BindOption fb puTitle _ : _ -> BindingDecision fb puTitle
      _                           -> error "Bind variants is over!"
  where
    prioritize' (BindingO fb title)
      = BindOption{ boFB=fb
                  , boTitle=title
                  , boPriority=prioritize net (howManyOptionAllow bOptions) fb title
                  }
    sort' = sortBy (flip $ \a b -> boPriority a `compare` boPriority b)
mkBindDecision _ _ = undefined



data BindOption title v = BindOption
  { boFB       :: FB (Parcel v) v
  , boTitle    :: title
  , boPriority :: Maybe BindPriority
  } deriving (Show)



-- | Приоритеты операции присваивания.
data BindPriority
  -- | Устанавливается для таких функциональных блоков, привязка которых может быть заблокирована
  -- другими. Пример - занятие Loop-ом адреса, используемого FramInput.
  = Critical
  -- | Для данной привязки есть только один вариант.
  | Exclusive
  -- | Привязка данного функционального блока может быть активировано только спустя указанное
  -- колличество тактов.
  | Restless Int
  -- | Данная операция может быть привязана прямо сейчас и это приведёт к разрешению указанного
  -- количества пересылок.
  | Input Int
  deriving (Show, Eq)

instance Ord BindPriority where
  Critical `compare` _ = GT
  _ `compare` Critical = LT
  (Input    a) `compare` (Input    b) = a `compare` b -- чем больше, тем лучше
  (Input    _) `compare`  _           = GT
  (Restless _) `compare` (Input    _) = LT
  (Restless a) `compare` (Restless b) = b `compare` a -- чем меньше, тем лучше
  (Restless _) `compare`  _           = GT
  Exclusive `compare` Exclusive = EQ
  Exclusive `compare` _ = LT



-- | Определить приоретет варианта привязки функционального блока.
prioritize net@BusNetwork{..} optionCount fb title
  -- В настоящий момент данная операци приводит к тому, что часть FB перестают быть вычислимыми.
  | isCritical fb                    = Just Critical
  | length (optionCount M.! fb) == 1 = Just Exclusive

  | pulls <- filter isTarget $ optionsAfterBind fb pu
  , not (null pulls)
  = Just $ Input $ sum $ map (length . variables) pulls

  | Just (_var, tcFrom) <- find (\(v, _) -> v `elem` variables fb) $ waitingTimeOfVariables net
  = Just $ Restless $ fromEnum tcFrom

  | otherwise = Nothing
  where
    pu = bnPus M.! title



-- | Подсчитать, сколько вариантов для привязки функционального блока определено.
-- Если вариант всего один, может быть стоит его использовать сразу?
howManyOptionAllow bOptions
  = foldl ( \st (BindingO fb title) -> M.alter (countOption title) fb st ) (M.fromList []) bOptions
  where
    countOption title (Just titles) = Just $ title : titles
    countOption title Nothing       = Just [ title ]


-- | Время ожидания переменных.
waitingTimeOfVariables net@BusNetwork{..}
  = [ (variable, tc^.avail.infimum)
    | DataFlowO{ dfoSource=(_, tc@TimeConstrain{..}), ..} <- options dataFlowDT net
    , (variable, Nothing) <- M.assocs dfoTargets
    ]


-- | Оценить, сколько новых вариантов развития вычислительного процесса даёт привязка
-- функциоанльного блока.
optionsAfterBind fb pu = case bind fb pu of
  Right pu' -> filter (\(EndpointO act _) -> act `optionOf` fb) $ options endpointDT pu'
  _         -> []
  where
    act `optionOf` fb' = not $ null (variables act `intersect` variables fb')




-- * Утилиты

passiveOption2action d@EndpointO{..}
  = let a = d^.at.avail.infimum
        -- "-1" - необходимо, что бы не затягивать процесс на лишний такт, так как интервал включает
        -- граничные значения.
        b = d^.at.avail.infimum + d^.at.dur.infimum - 1
    in EndpointD epoType (a ... b)

networkOption2action (DataFlowO src trg)
  = let pushTimeConstrains = map snd $ catMaybes $ M.elems trg
        predictPullStartFromPush o = o^.avail.infimum - 1 -- сдвиг на 1 за счёт особенностей используемой сети.
        pullStart    = maximum $ (snd src^.avail.infimum) : map predictPullStartFromPush pushTimeConstrains
        pullDuration = maximum $ map (\o -> o^.dur.infimum) $ snd src : pushTimeConstrains
        pullEnd = pullStart + pullDuration - 1
        pushStart = pullStart + 1

        mkEvent (from_, tc@TimeConstrain{..})
          = Just (from_, pushStart ... (pushStart + tc^.dur.infimum - 1))
        pushs = map (second $ maybe Nothing mkEvent) $ M.assocs trg
    in DataFlowDecision ( fst src, pullStart ... pullEnd ) $ M.fromList pushs


inBranchSteps Branch{..} = whatsHappenWith branchTag topPU
inBranchSteps Bush{}     = error "inBranchSteps: wrong args"


whatsHappenWith tag pu =
  [ st | st@Step{..} <- steps $ process pu
       , tag == placeInTimeTag sTime
       ]
