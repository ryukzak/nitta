{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
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
  ( controlFlowDecision
  , ControlFlowDT
  , ControlFlowGraph(..)
  , DataFlowGraph(..)
  , Decision(..)
  , Option(..)
  , OptionCF(..)
  , SystemState(..)
  ) where

import           Data.List        (nub)
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
  deriving ( Show, Generic )

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



-- | Для описания текущего состояния вычислительной системы (с учётом алгоритма,
-- потока управления, "текущего места" исполнения алгоритма, микроархитектуры и
-- расписния) необходимо работать со стеком. При этом, относительно
-- программирования, вызов процедуры подменяется входом в подграф DFG. При этом
-- общая логика развития (синтеза) вычислительного процесса не отличается от
-- логики работы языков высокого уровня, за тем исключением что тут немного
-- другая модель вычислений:
--
-- - сперва необходимо выполнить всю работу на вершине стека, после чего можно
--   перейти к работе с нижележайшем кадром;
-- - новый кадр стека формируется не для вызова подпрограммы, а для выполнения
--   подграфа (соответствует ветвлению алгоритма);
-- - так как стек необходим для реализации ветвления алгоритма и при этом
--   решается задача планирования вычислительного процесса, то необходимо пройти
--   все возможные варианты развития вычислительного процесса в общем случае,
--   следовательно, на каждом уровне стека может присутствовать несколько
--   кадров.
data SystemState title tag v t
  = Frame
    { nitta        :: BusNetwork title v t
    , dfg          :: DataFlowGraph v
    , timeTag      :: Maybe tag
    , branchInputs :: [v]
    }
  | Level
    { currentBranch     :: SystemState title tag v t
    , remainingBranches :: [ SystemState title tag v t ]
    , completedBranches :: [ SystemState title tag v t ]
    , rootBranch        :: SystemState title tag v t
    }
  deriving ( Generic )

instance ( Var v ) => DecisionProblem (BindingDT String v)
                            BindingDT (SystemState String tag v t)
         where
  options _ Frame{..} = options binding nitta
  options _ _         = undefined
  decision _ branch@Frame{..} act = branch{ nitta=decision binding nitta act }
  decision _ _ _                  = undefined

instance ( Typeable title, Ord title, Show title, Var v, Time t
         ) => DecisionProblem (DataFlowDT title v t)
                   DataFlowDT (SystemState title tag v t)
         where
  options _ Frame{..} = options dataFlowDT nitta
  options _ _         = undefined
  decision _ branch@Frame{..} act = branch{ nitta=decision dataFlowDT nitta act }
  decision _ _ _                   = undefined



---------------------------------------------------------------------
-- * Ветвление алгоритма.
--
-- Под ветвлением алгоритма понимается возможность выбора одного из подграфов
-- DFG (DFGSwitch -> dfgCases) в зависимости от данных. При этом выбор подграфа
-- может осуществляться 1) спекулятивно (мультиплексор), если функциональные
-- блоки не содаржат побочных эффектов или 2) реальным выбором вычислительного
-- процесса.
data ControlFlowDT tag v
controlFlowDecision = Proxy :: Proxy ControlFlowDT


instance DecisionType (ControlFlowDT tag v) where
  data Option (ControlFlowDT tag v) = ControlFlowO (ControlFlowGraph tag v)
    deriving ( Generic )
  data Decision (ControlFlowDT tag v) = ControlFlowD (ControlFlowGraph tag v)
    deriving ( Generic )

instance ( Var v, Time t
         ) => DecisionProblem (ControlFlowDT String v)
                ControlFlowDT (SystemState String String v (TaggedTime String t))
         where
  options _ Frame{ dfg=DFG g, nitta }
    = [ ControlFlowO undefined -- g
      | DFGSwitch{ dfgKey } <- g
      , all (`elem` availableVars) $ dfgKey : inputsOfFBs (concatMap functionalBlocks g)
      ]
    where
      availableVars = nub $ concatMap (M.keys . dfoTargets) $ options dataFlowDT nitta
  options _ _ = error "DecisionProblem ControlFlowDT: internal error."

  -- | Выполнить ветвление вычислительного процесса. Это действие заключается в замене текущей ветки
  -- вычислительного процесса на кустарник (Frame), в рамках работы с которым необъходимо перебрать
  -- все веточки и в конце собрать обратно в одну ветку.
  decision _ Frame{..} (ControlFlowD CFGSwitch{..})
    = let now = nextTick $ process nitta
          branch : branchs = map (\OptionCF{..} -> Frame
                                    { nitta=setTime now{ tag=ocfTag } nitta
                                    , dfg=undefined
                                    , timeTag=ocfTag
                                    , branchInputs=ocfInputs
                                    }
                                  ) cfgCases
      in Level{ currentBranch=branch
              , remainingBranches=branchs
              , completedBranches=[]
              , rootBranch=branch
              }
  decision _ _ _                   = undefined
