{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module NITTA.Compiler
  ( bindAll
  , bindAllAndNaiveSchedule
  , passiveOption2action
  , naive
  , NaiveOpt(..)
  )
where

import           Data.Default
import           Data.List        (find, intersect, nub, sortBy)
import qualified Data.Map         as M
import           Data.Maybe       (catMaybes, isJust, mapMaybe)
import           Data.Proxy
import           NITTA.BusNetwork
import           NITTA.Flows
import           NITTA.Lens
import           NITTA.Types
import           NITTA.Utils
import           Numeric.Interval ((...))



-- | Выполнить привязку списка функциональных блоков к указанному вычислительному блоку.
bindAll pu fbs = fromRight (error "Can't bind FB to PU!") $ foldl nextBind (Right pu) fbs
  where
    nextBind (Right pu') fb = bind fb pu'
    nextBind (Left r) _     = error r



-- | Выполнить привязку списка функциональных блоков к указанному вычислительному блоку и наивным
-- образом спланировать вычислительный процесса пасивного блока обработки данных (PUClass Passive).
bindAllAndNaiveSchedule pu0 alg = naiveSchedule $ bindAll pu0 alg
  where
    naiveSchedule pu
      | opt : _ <- options_ endpointDT pu = naiveSchedule $ decision_ endpointDT pu $ passiveOption2action opt
      | otherwise = pu



-- | Настройки процесса компиляции.
data NaiveOpt = NaiveOpt
  { -- | Порог колличества вариантов, после которого пересылка данных станет приоритетнее, чем
    -- привязка функциональных блоков.
    threshhold :: Int
  }

instance Default NaiveOpt where
  def = NaiveOpt{ threshhold=2
                }



---------------------------------------------------------------------
-- * Представление решения компилятора.


data CompilerDT title tag v t
compiler = Proxy :: Proxy CompilerDT


instance DecisionType (CompilerDT title tag v t) where
  data Option_ (CompilerDT title tag v t)
    = CFOption (Option_ (ControlFlowDT tag v))
    | BOption  (Option_ (BindingDT title v))
    | DFOption (Option_ (DataFlowDT String v t))

  data Decision_ (CompilerDT title tag v t)
    = CFDecision (Decision_ (ControlFlowDT tag v))
    | BDecision (Decision_ (BindingDT title v))
    | DFDecision (Decision_ (DataFlowDT String v t))

getCFOption (CFOption x) = Just x
getCFOption _            = Nothing
getBOption (BOption x) = Just x
getBOption _           = Nothing
getDFOption (DFOption x) = Just x
getDFOption _            = Nothing

instance ( Tag tag, Time t, Var v
         ) => Decision CompilerDT (CompilerDT String tag v (TaggedTime tag t))
                      (BranchedProcess String tag v (TaggedTime tag t))
         where
  options_ _ Bush{..} = options_ compiler currentBranch
  options_ _ branch
    = map DFOption (dataFlowOptions branch)
    ++ map CFOption (options_ controlFlowDecision branch)
    ++ map BOption (options_ binding branch)

  decision_ _ bush@Bush{..} act            = bush{ currentBranch=decision_ compiler currentBranch act }
  decision_ _ branch (BDecision d)  = decision_ binding branch d
  decision_ _ branch (CFDecision d) = decision_ controlFlowDecision branch d
  decision_ _ branch@Branch{ topPU=pu, .. } (DFDecision act) = branch{ topPU=decision_ dataFlowDT pu act }




---------------------------------------------------------------------
-- * Наивный, но полноценный компилятор.


naive NaiveOpt{..} branch@Branch{}
  = let d = options2decision_ branch $ options_ compiler branch
    in case d of
      Just d' -> decision_ compiler branch d'
      Nothing -> branch

naive no bush@Bush{..}
  = let bush' = bush{ currentBranch=naive no currentBranch }
    in if isCurrentBranchOver bush'
      then finalizeBranch bush'
      else bush'



-- | Мегафункция принятия решения в процессе компиляции. На вход получает ветку процесса и доступные
-- опции. Выполняет их приоритезацию и выдаёт победителя в качестве результата. В потенциале может
-- реализовываться с использованием IO, то есть - решение принемает пользователь.
options2decision_ branch opts
  = let bOptions = mapMaybe getBOption opts
        cfOptions = mapMaybe getCFOption opts
        dfOptions = mapMaybe getDFOption opts
    in case () of
      _ | length dfOptions >= 2 -> Just $ dfDecision dfOptions
      _ | not $ null bOptions   -> Just $ mkBindDecision branch bOptions
      _ | not $ null dfOptions  -> Just $ dfDecision dfOptions
      _ | not $ null cfOptions  -> Just $ CFDecision $ (\(ControlFlowO cf : _) -> ControlFlowD cf) cfOptions
      _ -> Nothing
  where
    dfDecision dfOptions = case sortBy (\a b -> start a `compare` start b) dfOptions of
      v : _ -> DFDecision $ networkOption2action v
      _     -> error "No variants!"
    start opt = opt^.at.avail.infimum



---------------------------------------------------------------------
-- * Ветвление алгоритма.


data ControlFlowDT tag v
controlFlowDecision = Proxy :: Proxy ControlFlowDT


instance DecisionType (ControlFlowDT tag v) where
  data Option_ (ControlFlowDT tag v) = ControlFlowO (ControlFlow tag v)
  data Decision_ (ControlFlowDT tag v) = ControlFlowD (ControlFlow tag v)


instance ( Tag tag, Var v, Time t
         ) => Decision ControlFlowDT (ControlFlowDT tag v)
                      (BranchedProcess String tag v (TaggedTime tag t))
         where
  options_ _ Branch{ topPU=pu, ..} = branchingOptions controlFlow availableVars
    where
      availableVars = nub $ concatMap (M.keys . dfoTargets) $ options_ dataFlowDT pu
  options_ _ _ = undefined

  -- | Выполнить ветвление вычислительного процесса. Это действие заключается в замене текущей ветки
  -- вычислительного процесса на кустарник (Bush), в рамках работы с которым необъходимо перебрать
  -- все веточки и в конце собрать обратно в одну ветку.
  decision_ _ Branch{..} (ControlFlowD Choice{..})
    = let now = nextTick $ process topPU
          branch : branchs = map (\OptionCF{..} -> Branch
                                    { topPU=setTime now{ tag=ocfTag } topPU
                                    , controlFlow=oControlFlow
                                    , branchTag=ocfTag
                                    , branchInputs=ocfInputs
                                    }
                                  ) cfOptions
      in Bush{ currentBranch=branch
            , remainingBranches=branchs
            , completedBranches=[]
            , rootBranch=branch
            }
  decision_ _ _ _                   = undefined



-- * Работа с потоком данных.


dataFlowOptions Branch{..} = sensibleOptions $ filterByControlModel controlFlow $ options_ dataFlowDT topPU
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

-- | Получить список вариантов ветвления вычислительного процесса.
--
-- Ветвление вычислительного процесса возможно в том случае, если доступнен ключ ветвления
-- алгоритма и все входные переменные для всех вариантов развития вычислительного процесса.
branchingOptions (Block cfs) availableVars
  = [ ControlFlowO x
    | x@Choice{..} <- cfs
    , all (`elem` availableVars) $ cfCond : cfInputs
    ]
branchingOptions _ _ = error "branchingOptions: internal error."



-- | Функция применяется к кусту и позволяет определить, осталась ли работа в текущей ветке или нет.
isCurrentBranchOver Bush{ currentBranch=branch@Branch{..} }
  = let bOptions = options_ binding branch
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
      BindOption fb puTitle _ : _ -> BDecision $ BindingD fb puTitle
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
  { boFB       :: FB Parcel v
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
    | DataFlowO{ dfoSource=(_, tc@TimeConstrain{..}), ..} <- options_ dataFlowDT net
    , (variable, Nothing) <- M.assocs dfoTargets
    ]


-- | Оценить, сколько новых вариантов развития вычислительного процесса даёт привязка
-- функциоанльного блока.
optionsAfterBind fb pu = case bind fb pu of
  Right pu' -> filter (\(EndpointO act _) -> act `optionOf` fb) $ options_ endpointDT pu'
  _         -> []
  where
    act `optionOf` fb' = not $ null (variables act `intersect` variables fb')




-- * Утилиты

passiveOption2action d@EndpointO{..}
  = let a = d^.at.avail.infimum
        b = d^.at.avail.infimum + d^.at.dur.infimum
    in EndpointD epoType (a ... b)

networkOption2action act@DataFlowO{..}
  = let pushTimeConstrains = map snd $ catMaybes $ M.elems dfoTargets
        predictPullStartFromPush o = o^.avail.infimum - 1 -- сдвиг на 1 за счёт особенностей используемой сети.
        pullStart    = maximum $ ((snd dfoSource)^.avail.infimum) : map predictPullStartFromPush pushTimeConstrains
        pullDuration = maximum $ map (\o -> o^.dur.infimum) $ (snd dfoSource) : pushTimeConstrains
        pullEnd = pullStart + pullDuration - 1
        pushStart = pullStart + 1

        mkEvent (from, tc@TimeConstrain{..})
          = Just (from, pushStart ... (pushStart + tc^.dur.infimum - 1))
        pushs = map (\(var, timeConstrain) -> (var, maybe Nothing mkEvent timeConstrain) ) $ M.assocs dfoTargets
    in DataFlowD{ dfdSource=( fst dfoSource, pullStart ... pullEnd )
                , dfdTargets=M.fromList pushs
                }


inBranchSteps Branch{..} = whatsHappenWith branchTag topPU
inBranchSteps Bush{}     = error "inBranchSteps: wrong args"


whatsHappenWith tag pu =
  [ st | st@Step{..} <- steps $ process pu
       , tag == placeInTimeTag sTime
       ]
