{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.DataFlow
Description :
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}

module NITTA.DataFlow
    ( DataFlowGraph(..)
    , Decision(..)
    , node
    , Option(..)
    , ModelState(..)
    ) where

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
    = DFGNode (F v Int)
    -- |Граф, где информация о вершинах хранится внутри функциональных блоков.
    | DFG [DataFlowGraph v]
    --  |Множество взаимозаменяемых подграфов.
    --  | DFGSwitch
    --     { dfgKey   :: v -- ^ключ, по которому осуществляется выбор подграфа.
    --     , dfgCases :: [(Int, DataFlowGraph v)] -- ^таблица значений ключей и соответствующих подграфов.
    --     }
    deriving ( Show, Generic )

instance ( Var v ) => Variables (DataFlowGraph v) v where
    variables (DFGNode fb) = variables fb
    variables (DFG g)      = unionsMap variables g
    -- variables DFGSwitch{ dfgKey, dfgCases } = singleton dfgKey `union` unionsMap (variables . snd) dfgCases

instance WithFunctions (DataFlowGraph v) (F v Int) where
    functions (DFGNode f) = [ f ]
    functions (DFG g)     = concatMap functions g
    -- functions DFGSwitch{ dfgCases } = concatMap (functions . snd) dfgCases

-- dfgInputs g = algInputs $ functions g
node (f :: F v Int) = DFGNode f

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
data ModelState title tag v x t
    = Frame
        { processor :: BusNetwork title v x t
        , dfg       :: DataFlowGraph v
        , timeTag   :: Maybe tag
        }
    --  | Level
    --     { currentFrame    :: ModelState title tag v x t
    --     , remainFrames    :: [ ModelState title tag v x t ]
    --     , completedFrames :: [ ModelState title tag v x t ]
    --     , initialFrame    :: ModelState title tag v x t
    --     }
    deriving ( Generic )




instance ( Var v
         , Typeable x
         ) => DecisionProblem (BindingDT String v x)
                    BindingDT (ModelState String tag v x t)
         where
    options _ Frame{ processor }    = options binding processor
    -- options _ Level{ currentFrame } = options binding currentFrame
    decision _ f@Frame{ processor } d = f{ processor=decision binding processor d }
    -- decision _ l@Level{ currentFrame } d = l{ currentFrame=decision binding currentFrame d }

instance ( Typeable title, Ord title, Show title, Var v, Time t
         , Typeable x
         ) => DecisionProblem (DataFlowDT title v t)
                   DataFlowDT (ModelState title tag v x t)
         where
    options _ Frame{ processor }    = options dataFlowDT processor
    -- options _ Level{ currentFrame } = options dataFlowDT currentFrame
    decision _ f@Frame{ processor } d = f{ processor=decision dataFlowDT processor d }
    -- decision _ l@Level{ currentFrame } d = l{ currentFrame=decision dataFlowDT currentFrame d }


---------------------------------------------------------------------
-- |Ветвление алгоритма.
--
-- Под ветвлением алгоритма понимается возможность выбора одного из подграфов DFG
-- `DFGSwitch(dfgCases)` в зависимости от данных. При этом выбор подграфа может осуществляться:
--
-- 1. спекулятивно (мультиплексор), если функциональные блоки не содержат побочных эффектов;
-- 2. реальным условным переходом практических как в неймановских процессорах.

-- data ControlDT v
-- controlDT = Proxy :: Proxy ControlDT

-- instance DecisionType (ControlDT v) where
--     data Option (ControlDT v) = ControlFlowO (DataFlowGraph v) -- DFGSwitch
--         deriving ( Generic )
--     data Decision (ControlDT v) = ControlFlowD (DataFlowGraph v)
--         deriving ( Generic )

-- instance ( Var v, Time t
--          , Typeable x
--          ) => DecisionProblem (ControlDT v)
--                 ControlDT (ModelState String String v x (TaggedTime String t))
--          where
--     options _ Frame{ dfg=DFG g, processor }
--         = let availableVars = nub $ concatMap (M.keys . dfoTargets) $ options dataFlowDT processor
--         in [ ControlFlowO sg
--             | sg@DFGSwitch{ dfgKey } <- g
--             , all (`elem` availableVars) $ singleton dfgKey `union` dfgInputs sg
--             ]
--     options _ Level{ currentFrame } = options controlDT currentFrame
--     options _ _ = error "ControlFlowDT: options: wrong DFG."

--     decision _ Frame{ processor } (ControlFlowD DFGSwitch{ dfgKey, dfgCases })
--         = let
--             now = nextTick $ process processor
--             f : fs = map
--                 (\( caseValue, dfg ) -> Frame
--                     { processor=setTime now{ tag=Just $ show dfgKey ++ "." ++ show caseValue } processor
--                     , timeTag=Just $ show dfgKey
--                     , dfg
--                     }
--                 ) dfgCases
--         in Level
--             { currentFrame=f
--             , remainFrames=fs
--             , completedFrames=[]
--             , initialFrame=f
--             }
--     decision _ l@Level{ currentFrame } d = l{ currentFrame=decision controlDT currentFrame d }
--     decision _ _ _ = error "ControlFlowDT: decision: wrong decision"
