{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module NITTA.Compiler
  ( naive
  , bindAll
  , bindAllAndNaiveSchedule
  , passiveOption2action
  , NaiveOpt(..)
  )
where

import           Data.Default
import           Data.List        (find, intersect, nub, sortBy)
import qualified Data.Map         as M
import           Data.Maybe       (catMaybes, isJust)
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
      | opt : _ <- options pu = naiveSchedule $ select pu $ passiveOption2action opt
      | otherwise = pu


-- | Настройки процесса компиляции.
data NaiveOpt = NaiveOpt
  { -- | Порог колличества вариантов, после которого пересылка данных станет приоритетнее, чем
    -- привязка функциональных блоков.
    threshhold :: Int
  }

instance Default NaiveOpt where
  def = NaiveOpt
    { threshhold=2
    }


-- | Наивный, но полноценный компилятор.
naive NaiveOpt{..} branch@Branch{ topPU=pu, ..}
  = let bOptions = bindingOptions pu
        cfOptions = branchingOptions controlFlow availableVars
        dfOptions = dataFlowOptions branch
    in case () of
      _ | length dfOptions >= threshhold -> doSchedule dfOptions
      _ | not $ null bOptions            -> doBind bOptions branch
      _ | not $ null dfOptions           -> doSchedule dfOptions
      _ | not $ null cfOptions           -> doBranch branch (head cfOptions)
      _ -> branch
  where
    availableVars = nub $ concatMap (M.keys . toPush) $ options pu
    start opt = opt^.at.avail.infimum
    doSchedule dfOptions
      = case sortBy (\a b -> start a `compare` start b) dfOptions of
        v : _ -> branch{ topPU=select pu $ networkOption2action v }
        _     -> error "No variants!"

naive no bush@Bush{..}
  = let bush' = bush{ currentBranch=naive no currentBranch }
    in if isCurrentBranchOver bush'
       then finalizeBranch bush'
       else bush'



dataFlowOptions Branch{..} = sensibleOptions $ filterByControlModel controlFlow $ options topPU
  where
    filterByControlModel controlModel opts
      = let cfOpts = allowByControlFlow controlModel
        in map (\t@TransportOpt{..} -> t
                { toPush=M.fromList $ map (\(v, desc) -> (v, if v `elem` cfOpts
                                                              then desc
                                                              else Nothing)
                                          ) $ M.assocs toPush
                }) opts
    sensibleOptions = filter $ \TransportOpt{..} -> any isJust $ M.elems toPush

dataFlowOptions _ = error "Can't generate dataflow options for BranchingInProgress."



-- * Работа с потоком управления.

-- | Получить список вариантов ветвления вычислительного процесса.
--
-- Ветвление вычислительного процесса возможно в том случае, если доступнен ключ ветвления
-- алгоритма и все входные переменные для всех вариантов развития вычислительного процесса.
branchingOptions (Block cfs) availableVars
  = [ x
    | x@Choice{..} <- cfs
    , all (`elem` availableVars) $ cfCond : cfInputs
    ]
branchingOptions _ _ = error "branchingOptions: internal error."

-- | Выполнить ветвление вычислительного процесса. Это действие заключается в замене текущей ветки
-- вычислительного процесса на кустарник (Bush), в рамках работы с которым необъходимо перебрать
-- все веточки и в конце собрать обратно в одну ветку.
doBranch Branch{..} Choice{..}
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
doBranch _ _ = error "Can't split process."


-- | Функция применяется к кусту и позволяет определить, осталась ли работа в текущей ветке или нет.
isCurrentBranchOver Bush{ currentBranch=branch@Branch{..} }
  = let bOptions = bindingOptions topPU
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

data BindOption title v = BindOption
  { boFB       :: FB Parcel v
  , boTitle    :: title
  , boPriority :: Maybe BindPriority
  } deriving (Show)

data BindPriority
  = Exclusive
  | Restless Int
  | Input Int
  -- Must be binded before a other, because otherwise can cause runtime error.
  | Critical
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



-- TODO: А как эта часть синхронизируется с ControlFlow? Как гарантируется сходимость биндингов?
doBind bOptions branch@Branch{ topPU=net@BusNetwork{..} }
  = case sort' $ map prioritize bOptions of
      BindOption fb puTitle _ : _ -> branch{ topPU=subBind fb puTitle net }
      _                           -> error "Bind variants is over!"
  where
    prioritize (fb, title) = BindOption{ boFB=fb
                                       , boTitle=title
                                       , boPriority=prioritize' net fb title
                                       }
    sort' = sortBy (flip $ \a b -> boPriority a `compare` boPriority b)


    prioritize' net@BusNetwork{..} fb title
      -- В настоящий момент данная операци приводит к тому, что часть FB перестают быть вычислимыми.
      --  | isCritical fb = bv{ priority=Just Critical }

      | null (dependency fb)
      , pulls <- filter isPull $ optionsAfterBind fb pu
      , not (null pulls)
      = Just $ Input $ sum $ map (length . variables) pulls

      | Just (_variable, tcFrom) <- find (\(v, _) -> v `elem` variables fb) $ restlessVariables net
      = Just $ Restless $ fromEnum tcFrom

      | length (howManyOptionAllow bOptions M.! fb) == 1
      = Just Exclusive

      | otherwise = Nothing
      where
        pu = bnPus M.! title

doBind _ _ = error "doBind: internal error."


-- | Подсчитать, сколько вариантов для привязки функционального блока определено.
-- Если вариант всего один, может быть стоит его использовать сразу?
howManyOptionAllow bOptions
  = foldl ( \st (fb, title) -> M.alter (countOption title) fb st ) (M.fromList []) bOptions
  where
    countOption title (Just titles) = Just $ title : titles
    countOption title Nothing       = Just [ title ]


-- | ?
restlessVariables net@BusNetwork{..}
  = [ (variable, tc^.avail.infimum)
    | TransportOpt{ toPullAt=tc@TimeConstrain{..}, ..} <- options net
    , (variable, Nothing) <- M.assocs toPush
    ]


-- | Оценить, сколько новых вариантов развития вычислительного процесса даёт привязка
-- функциоанльного блока.
--
-- TODO: И какую пользу даёт данная метрика в текущем виде?
optionsAfterBind fb pu = case bind fb pu of
  Right pu' -> filter (\(EffectOpt act _) -> act `optionOf` fb) $ options pu'
  _         -> []
  where
    act `optionOf` fb' = not $ null (variables act `intersect` variables fb')




-- * Утилиты

passiveOption2action EffectOpt{..}
  = let a = eoAt^.avail.infimum
        b = eoAt^.avail.infimum + eoAt^.dur.infimum
    in EffectAct eoEffect (a ... b)

networkOption2action TransportOpt{..}
  = let pushTimeConstrains = map snd $ catMaybes $ M.elems toPush
        predictPullStartFromPush o = o^.avail.infimum - 1 -- сдвиг на 1 за счёт особенностей используемой сети.
        pullStart    = maximum $ (toPullAt^.avail.infimum) : map predictPullStartFromPush pushTimeConstrains
        pullDuration = maximum $ map (\o -> o^.dur.infimum) $ toPullAt : pushTimeConstrains
        pullEnd = pullStart + pullDuration - 1
        pushStart = pullStart + 1

        mkEvent (from, tc@TimeConstrain{..})
          = Just (from, pushStart ... (pushStart + tc^.dur.infimum - 1))
        pushs = map (\(var, timeConstrain) -> (var, maybe Nothing mkEvent timeConstrain) ) $ M.assocs toPush

        act = TransportAct{ taPullFrom=toPullFrom
                          , taPullAt=pullStart ... pullEnd
                          , taPush=M.fromList pushs
                          }
    in -- trace (">opt>" ++ show opt0 ++ "\n>act>" ++ show act ++ "\n>pullStart>" ++ show (map (\o -> o^.avail.infimum) $ toPullAt : pushTimeConstrains) ++ "\n>pullDuration>" ++ show pullDuration)
        act


inBranchSteps Branch{..} = whatsHappenWith branchTag topPU
inBranchSteps Bush{}     = error "inBranchSteps: wrong args"


whatsHappenWith tag pu =
  [ st | st@Step{..} <- steps $ process pu
       , tag == placeInTimeTag sTime
       ]
