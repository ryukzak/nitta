{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module NITTA.DataFlow
  ( controlDT
  , ControlDT
  , DataFlowGraph(..)
  , Decision(..)
  , node
  , Option(..)
  , SystemState(..)
  ) where

import           Data.List        (nub)
import qualified Data.Map         as M
import           Data.Set         (singleton, union)
import           Data.Typeable
import           GHC.Generics
import           NITTA.BusNetwork
import           NITTA.Types
import           NITTA.Utils


-- * Модифицированый граф потока данных.

-- |Граф потока данных.
--
-- Поток данных представляется в виде графа, описывающего взаимосвязи между функциональными блоками
-- (пересылки). При этом в графе могут быть множества взаимозаменяемых подграфов, соответствующих
-- разным веткам условного оператору (Switch). В теории возможны и циклы.
--
-- В случае если функциональные блоки не имеют побочных эффектов, то множество взаимозаменяемых
-- подграфом можно заменить графом с выборкой результата через мультиплексор.

-- FIXME: Сделать визуализацию DataFlowGraph через graphviz. В первую очередь DFG.
data DataFlowGraph v
  -- |Вершина графа, соответствует фунциональному блоку.
  = DFGNode (F (Parcel v Int))
  -- |Граф, где информация о вершинах хранится внутри функциональных блоков.
  | DFG [DataFlowGraph v]
  -- |Множество взаимозаменяемых подграфов.
  | DFGSwitch
    { dfgKey   :: v -- ^ключ, по которому осуществляется выбор подграфа.
    , dfgCases :: [(Int, DataFlowGraph v)] -- ^таблица значений ключей и соответствующих подграфов.
    }
  deriving ( Show, Generic )

instance ( Var v ) => Variables (DataFlowGraph v) v where
  variables (DFGNode fb)                  = variables fb
  variables (DFG g)                       = unionsMap variables g
  variables DFGSwitch{ dfgKey, dfgCases } = singleton dfgKey `union` unionsMap (variables . snd) dfgCases

instance WithFunctions (DataFlowGraph v) (F (Parcel v Int)) where
  functions (DFGNode fb)          = [ fb ]
  functions (DFG g)               = concatMap functions g
  functions DFGSwitch{ dfgCases } = concatMap (functions . snd) dfgCases

dfgInputs g = algInputs $ functions g
node (fb :: F (Parcel v Int)) = DFGNode fb

-- |Для описания текущего состояния вычислительной системы (с учётом алгоритма, потока управления,
-- "текущего места" исполнения алгоритма, микроархитектуры и расписния) необходима работа со стеком.
-- При этом, относительно программирования, вызов процедуры подменяется входом в подграф DFG. При
-- этом общая логика развития вычислительного процесса (синтеза) не отличается от логики работы
-- языков высокого уровня, за тем исключением иной модели вычислений:
--
-- - сперва необходимо выполнить всю работу на вершине стека, после чего можно перейти к работе с
--   нижележайшем кадром;
-- - новый кадр стека формируется для выполнения подграфа (соответствует ветвлению алгоритма);
-- - так как стек необходим для реализации ветвления алгоритма и при этом решается задача
--   планирования вычислительного процесса, то необходимо пройти все варианты развития
--   вычислительного процесса, следовательно, на каждом уровне стека может присутствовать несколько
--   кадров.
data SystemState title tag v x t
  = Frame
    { nitta   :: BusNetwork title v x t
    , dfg     :: DataFlowGraph v
    , timeTag :: Maybe tag
    }
  | Level
    { currentFrame    :: SystemState title tag v x t
    , remainFrames    :: [ SystemState title tag v x t ]
    , completedFrames :: [ SystemState title tag v x t ]
    , initialFrame    :: SystemState title tag v x t
    }
  deriving ( Generic )

instance ( Var v
         , Typeable x
         ) => DecisionProblem (BindingDT String (Parcel v x))
                    BindingDT (SystemState String tag v x t)
         where
  options _ Frame{ nitta }        = options binding nitta
  options _ Level{ currentFrame } = options binding currentFrame
  decision _ f@Frame{ nitta } d        = f{ nitta=decision binding nitta d }
  decision _ l@Level{ currentFrame } d = l{ currentFrame=decision binding currentFrame d }

instance ( Typeable title, Ord title, Show title, Var v, Time t
         , Typeable x
         ) => DecisionProblem (DataFlowDT title v t)
                   DataFlowDT (SystemState title tag v x t)
         where
  options _ Frame{ nitta }        = options dataFlowDT nitta
  options _ Level{ currentFrame } = options dataFlowDT currentFrame
  decision _ f@Frame{ nitta } d        = f{ nitta=decision dataFlowDT nitta d }
  decision _ l@Level{ currentFrame } d = l{ currentFrame=decision dataFlowDT currentFrame d }


---------------------------------------------------------------------
-- |Ветвление алгоритма.
--
-- Под ветвлением алгоритма понимается возможность выбора одного из подграфов DFG
-- `DFGSwitch(dfgCases)` в зависимости от данных. При этом выбор подграфа может осуществляться:
--
-- 1. спекулятивно (мультиплексор), если функциональные блоки не содержат побочных эффектов;
-- 2. реальным условным переходом практических как в неймановских процессорах.

data ControlDT v
controlDT = Proxy :: Proxy ControlDT

instance DecisionType (ControlDT v) where
  data Option (ControlDT v) = ControlFlowO (DataFlowGraph v) -- DFGSwitch
    deriving ( Generic )
  data Decision (ControlDT v) = ControlFlowD (DataFlowGraph v)
    deriving ( Generic )

instance ( Var v, Time t
         , Typeable x
         ) => DecisionProblem (ControlDT v)
                ControlDT (SystemState String String v x (TaggedTime String t))
         where
  options _ Frame{ dfg=DFG g, nitta }
    = let availableVars = nub $ concatMap (M.keys . dfoTargets) $ options dataFlowDT nitta
    in [ ControlFlowO sg
       | sg@DFGSwitch{ dfgKey } <- g
       , all (`elem` availableVars) $ singleton dfgKey `union` dfgInputs sg
       ]
  options _ Level{ currentFrame } = options controlDT currentFrame
  options _ _ = error "ControlFlowDT: options: wrong DFG."

  decision _ Frame{ nitta } (ControlFlowD DFGSwitch{ dfgKey, dfgCases })
    = let now = nextTick $ process nitta
          f : fs = map
            (\( caseValue, dfg ) -> Frame
                { nitta=setTime now{ tag=Just $ show dfgKey ++ "." ++ show caseValue } nitta
                , timeTag=Just $ show dfgKey
                , dfg
                }
            ) dfgCases
      in Level{ currentFrame=f
              , remainFrames=fs
              , completedFrames=[]
              , initialFrame=f
              }
  decision _ l@Level{ currentFrame } d = l{ currentFrame=decision controlDT currentFrame d }
  decision _ _ _ = error "ControlFlowDT: decision: wrong decision"
