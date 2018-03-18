{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
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
import           Data.Typeable
import           GHC.Generics
import           NITTA.BusNetwork
import           NITTA.Types
import           NITTA.Utils


-- * Модифицированый граф потока данных.
--
-- TODO: Сделать визуализацию DFG.


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
  variables (DFGNode fb)                  = variables fb
  variables (DFG g)                       = concatMap variables g
  variables DFGSwitch{ dfgKey, dfgCases } = dfgKey : concatMap (variables . snd) dfgCases

instance WithFunctionalBlocks (DataFlowGraph v) (FB (Parcel v) v) where
  functionalBlocks (DFGNode fb)          = [ fb ]
  functionalBlocks (DFG g)               = concatMap functionalBlocks g
  functionalBlocks DFGSwitch{ dfgCases } = concatMap (functionalBlocks . snd) dfgCases

dfgInputs g = inputsOfFBs $ functionalBlocks g
node fb = DFGNode $ FB fb

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
    { nitta   :: BusNetwork title v t
    , dfg     :: DataFlowGraph v
    , timeTag :: Maybe tag
    }
  | Level
    { currentFrame    :: SystemState title tag v t
    , remainFrames    :: [ SystemState title tag v t ]
    , completedFrames :: [ SystemState title tag v t ]
    , initialFrame    :: SystemState title tag v t
    }
  deriving ( Generic )

instance ( Var v ) => DecisionProblem (BindingDT String v)
                            BindingDT (SystemState String tag v t)
         where
  options _ Frame{ nitta }        = options binding nitta
  options _ Level{ currentFrame } = options binding currentFrame
  decision _ f@Frame{ nitta } d        = f{ nitta=decision binding nitta d }
  decision _ l@Level{ currentFrame } d = l{ currentFrame=decision binding currentFrame d }

instance ( Typeable title, Ord title, Show title, Var v, Time t
         ) => DecisionProblem (DataFlowDT title v t)
                   DataFlowDT (SystemState title tag v t)
         where
  options _ Frame{ nitta }        = options dataFlowDT nitta
  options _ Level{ currentFrame } = options dataFlowDT currentFrame
  decision _ f@Frame{ nitta } d        = f{ nitta=decision dataFlowDT nitta d }
  decision _ l@Level{ currentFrame } d = l{ currentFrame=decision dataFlowDT currentFrame d }


---------------------------------------------------------------------
-- * Ветвление алгоритма.
--
-- Под ветвлением алгоритма понимается возможность выбора одного из подграфов
-- DFG (DFGSwitch -> dfgCases) в зависимости от данных. При этом выбор подграфа
-- может осуществляться 1) спекулятивно (мультиплексор), если функциональные
-- блоки не содаржат побочных эффектов или 2) реальным выбором вычислительного
-- процесса.
data ControlDT v
controlDT = Proxy :: Proxy ControlDT

instance DecisionType (ControlDT v) where
  data Option (ControlDT v) = ControlFlowO (DataFlowGraph v) -- DFGSwitch
    deriving ( Generic )
  data Decision (ControlDT v) = ControlFlowD (DataFlowGraph v)
    deriving ( Generic )

instance ( Var v, Time t
         ) => DecisionProblem (ControlDT v)
                ControlDT (SystemState String String v (TaggedTime String t))
         where
  options _ Frame{ dfg=DFG g, nitta }
    = let availableVars = nub $ concatMap (M.keys . dfoTargets) $ options dataFlowDT nitta
    in [ ControlFlowO sg
       | sg@DFGSwitch{ dfgKey } <- g
       , all (`elem` availableVars) $ dfgKey : dfgInputs sg
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
