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

{-
TODO: Место для статьи о DFG и CFG. Методах работы с ними. Математической модели
для их преобразований.
-}
module NITTA.FlowGraph
  ( allowByControlFlow
  , BranchedProcess(..)
  , ControlFlowGraph(..), dataFlow2controlFlow
  , controlFlowDecision
  , ControlFlowDT
  , DataFlowGraph(..)
  , Decision(..)
  , Option(..)
  , OptionCF(..)
  ) where

import           Data.List        (nub, (\\))
import qualified Data.Map         as M
import           Data.Typeable
import           GHC.Generics
import           NITTA.BusNetwork
import           NITTA.Types
import           NITTA.Utils


-- * Граф потока управления и потока данных.
--
-- Две параллельно существующие модели алгоритма.
--
-- TODO: Разобраться в необходимости CFG как отдельной сущности, так как по
-- видимому можно ограничиться DFG с взаимозаменяемыми подграфами.
--
-- TODO: Сделать визуализацию DFG & CFG.


-- | Граф потока данных.
--
-- Поток данных представляется в виде графа, описывающего взаимосвязи между
-- функциональными блоками (пересылки). При этом в графе могут быть множества
-- взаимозаменяемых подграфов, соответствующих условному оператору (Switch).
--
-- TODO: В случае если функциональные блоки не имеют побочных эффектов, то
-- множество взаимозаменяемых подграфом можно заменить графом с выборкой
-- результата через мультиплексор.
data DataFlowGraph v
  = DFGNode (FB (Parcel v) v) -- ^ вершина графа, соответствует фунциональному блоку.
  | DFG [DataFlowGraph v] -- ^ граф, где информация о вершинах хранится внутри
                          -- функциональных блоков.
  | DFGSwitch -- ^ множество взаимозаменяемых подграфов.
    { dfgKey   :: v -- ^ ключ, по которому осуществляется выбор подграфа.
    , dfgCases :: [(Int, DataFlowGraph v)] -- ^ таблица соответствия значения ключа
                                           -- перехода и требуемого подграфа.
    }
  deriving ( Show )

instance ( Var v ) => Variables (DataFlowGraph v) v where
  variables (DFGNode fb)  = variables fb
  variables (DFG g)       = concatMap variables g
  variables DFGSwitch{..} = dfgKey : concatMap (variables . snd) dfgCases

instance WithFunctionalBlocks (DataFlowGraph v) (FB (Parcel v) v) where
  functionalBlocks (DFGNode fb)  = [ fb ]
  functionalBlocks (DFG g)       = concatMap functionalBlocks g
  functionalBlocks DFGSwitch{..} = concatMap (functionalBlocks . snd) dfgCases



-- | Граф потока управления.
--
-- Поток управление описывается структура  данных представляется в виде графа,
-- описывающего взаимосвязи между функциональными блоками (пересылки). При этом
-- в графе могут быть множества взаимозаменяемых подграфов, соответствующих
-- условному оператору (Switch).
data ControlFlowGraph tag v
  -- | Вершина графа, соответствует пересылаемому значению.
  = CFGNode v
  -- | Блок операций пересылоккоторые должны быть выполнены группой, при этом реальная
  -- последовательность пересылок указанных в блоке данных не принципиальна.
  | CFG [ControlFlowGraph tag v]
  -- | Блок вариантивного развития вычислительного процесса. Рассматривается как атомарный,
  -- так как иначе не получится обеспечить гарантированное время исполнения и целостность
  -- вычислительного процесса.
  | CFGSwitch
    { -- | Ключ выбора варианта развития вычислительного процесса. Принципиальное отличие от
      -- cfInputs заключается в том, что эта пересылка обязательно перед выбором.
      --
      -- При этом не очень понятно, почему она тут, а не в предыдущем блоке?
      cfgKey    :: v
      -- | Набор входных данных для вариативного развития вычислительного процесса.
    , cfgInputs :: [v]
      -- | Варианты ветвления вычилсительного процесса.
    , cfgCases  :: [OptionCF tag v]
    }
  deriving ( Show, Eq )

instance ( Var v ) => Variables (ControlFlowGraph tag v) v where
  variables (CFGNode v) = [v]
  variables (CFG cfs) = concatMap variables cfs
  variables CFGSwitch{..}  = cfgKey : concatMap (variables . oControlFlow) cfgCases



-- | Ветка потока управления.
data OptionCF tag v
  = OptionCF
  { ocfTag       :: Maybe tag -- ^ Тег ветки времени.
  , ocfInputs    :: [v] -- ^ Входные переменные ветки потока управления.
  , oControlFlow :: ControlFlowGraph tag v -- ^ Вложенный поток управления.
  } deriving ( Show, Eq )


dataFlow2controlFlow (DFGNode fb) = CFG $ map CFGNode $ variables fb
dataFlow2controlFlow paths@DFGSwitch{..}
  = let inputs' = inputsOfFBs $ functionalBlocks paths
        dfPaths' = map (\(key, prog) -> OptionCF
                         { ocfTag=Just $ show dfgKey ++ " == " ++ show key
                         , ocfInputs=inputs' \\ variables prog
                         , oControlFlow=dataFlow2controlFlow prog
                         }
                       ) dfgCases
    in CFGSwitch dfgKey inputs' dfPaths'
dataFlow2controlFlow (DFG ss)
  = let cf = map dataFlow2controlFlow ss
        parallel = filter isCFG cf
        parallel' = nub $ concatMap (\(CFG xs) -> xs) parallel
        withInputs = parallel' ++ nub (filter (not . isCFG) cf)
        inputsVariables = nub $ map CFGNode $ concatMap (\CFGSwitch{..} -> cfgInputs)
                          $ filter isCFGSwitch withInputs
    in CFG $ withInputs \\ inputsVariables


isCFG CFG{} = True
isCFG _     = False
isCFGSwitch CFGSwitch{} = True
isCFGSwitch _           = False



-- | При моделировании вычислительного процесса вычислительный процесс развивается не только в
-- рамках его потока данных, но и одновременно в рамках потока управления. При этом возможно
-- ситуация, когда какое-то действие допустимо с точки зрения потока данных (один вычислительный
-- блок готов выслать данный, а другой принять данные), но при этом данная пересылка должна
-- реализовываться только в случае выбора одной из веток вычислительного процесса. В таком случае
-- компилятору необходимо осуществлять проверку, можем ли мы с точки зрения потока управления
-- выполнить ту или иную пересылку данных. Для этого и служит эта фунция.
allowByControlFlow (CFGNode v)   = [ v ]
allowByControlFlow CFGSwitch{..} = [ cfgKey ]
allowByControlFlow block@(CFG g)
  | not $ any isCFG g = concatMap allowByControlFlow g
  | otherwise = error $ "Bad controlFlow: " ++ show block




-- | Описание ветвящегося вычислительного процесса. Из-за того что планирование вычислительного
-- процесса происходит глобально, то и ветвление времени происходит глобально.
-- Возможное от данного тезиса будет полезно абстрагироваться, если Choice целиком попадает на
-- отдельную подсеть, но пока до таких вопросов ещё очень далеко.
data BranchedProcess title tag v t
  -- | Описание ветки вычислительного процесса.
  = Branch
  { -- | Вычислительный блок, в рамках которого реализуется весь вычислительный процесс.
    --
    -- TODO: Убрать hardcode.
    topPU        :: BusNetwork title v t
    -- | Описание текущего потока управления (в случае если мы находимся в одной из веток
    -- вычислительного процесса, то описывается только её поток управления)
  , controlFlow  :: ControlFlowGraph tag v
    -- | Тег времени, идентифицирующий данную ветку вычислиельного процесса.
  , branchTag    :: Maybe tag
    -- | Входные данные рассматриваемой ветки вычислительного процесса.
  , branchInputs :: [v]
  }
  -- | Куст вычислительного процесса. Процесс расщеплён на множество веток имеющих один корень и
  -- сходящихся в одну точку по их завершению.
  | Bush
  { -- | Ветка процесса, планируемая в текущий момент времени.
    currentBranch     :: BranchedProcess title tag v t
    -- | Ветки процесса, требующие планирования.
  , remainingBranches :: [ BranchedProcess title tag v t ]
    -- | Спланированные ветки процесса.
  , completedBranches :: [ BranchedProcess title tag v t ]
    -- | Исходная ветка, которая была расщеплена. Используется как база для слияния куста.
  , rootBranch        :: BranchedProcess title tag v t
  } deriving ( Generic )






instance ( Var v ) => DecisionProblem (BindingDT String v)
                            BindingDT (BranchedProcess String tag v t)
         where
  options _ Branch{..} = options binding topPU
  options _ _          = undefined
  decision _ branch@Branch{..} act = branch{ topPU=decision binding topPU act }
  decision _ _ _                   = undefined

instance ( Typeable title, Ord title, Show title, Var v, Time t
         ) => DecisionProblem (DataFlowDT title v t)
                   DataFlowDT (BranchedProcess title tag v t)
         where
  options _ Branch{..} = options dataFlowDT topPU
  options _ _          = undefined
  decision _ branch@Branch{..} act = branch{ topPU=decision dataFlowDT topPU act }
  decision _ _ _                   = undefined



---------------------------------------------------------------------
-- * Ветвление алгоритма.


data ControlFlowDT tag v
controlFlowDecision = Proxy :: Proxy ControlFlowDT


instance DecisionType (ControlFlowDT tag v) where
  data Option (ControlFlowDT tag v) = ControlFlowO (ControlFlowGraph tag v)
    deriving ( Generic )
  data Decision (ControlFlowDT tag v) = ControlFlowD (ControlFlowGraph tag v)
    deriving ( Generic )

instance ( Tag tag, Var v, Time t
         ) => DecisionProblem (ControlFlowDT tag v)
                ControlFlowDT (BranchedProcess String tag v (TaggedTime tag t))
         where
  options _ Branch{ topPU=pu, ..} = branchingOptions controlFlow availableVars
    where
      availableVars = nub $ concatMap (M.keys . dfoTargets) $ options dataFlowDT pu
  options _ _ = undefined

  -- | Выполнить ветвление вычислительного процесса. Это действие заключается в замене текущей ветки
  -- вычислительного процесса на кустарник (Bush), в рамках работы с которым необъходимо перебрать
  -- все веточки и в конце собрать обратно в одну ветку.
  decision _ Branch{..} (ControlFlowD CFGSwitch{..})
    = let now = nextTick $ process topPU
          branch : branchs = map (\OptionCF{..} -> Branch
                                    { topPU=setTime now{ tag=ocfTag } topPU
                                    , controlFlow=oControlFlow
                                    , branchTag=ocfTag
                                    , branchInputs=ocfInputs
                                    }
                                  ) cfgCases
      in Bush{ currentBranch=branch
            , remainingBranches=branchs
            , completedBranches=[]
            , rootBranch=branch
            }
  decision _ _ _                   = undefined



-- | Получить список вариантов ветвления вычислительного процесса.
--
-- Ветвление вычислительного процесса возможно в том случае, если доступнен ключ ветвления
-- алгоритма и все входные переменные для всех вариантов развития вычислительного процесса.
branchingOptions (CFG cfs) availableVars
  = [ ControlFlowO x
    | x@CFGSwitch{..} <- cfs
    , all (`elem` availableVars) $ cfgKey : cfgInputs
    ]
branchingOptions _ _ = error "branchingOptions: internal error."
