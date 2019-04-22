{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.DataFlow
Description : Processor and algorithm model
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.DataFlow
    ( DataFlowGraph(..)
    , Decision(..)
    , Option(..)
    , ModelState(..)
    , targetProcessDuration
    , endpointOption2action
    , isSchedulingCompletable
    , isSchedulingComplete
    ) where

import           Data.Set         (fromList, member)
import           Data.Typeable
import           GHC.Generics
import           NITTA.BusNetwork
import           NITTA.Functions  (reg)
import           NITTA.Types
import           NITTA.Utils
import           NITTA.Utils.Lens
import           Numeric.Interval ((...))


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
data DataFlowGraph v x
    -- |Вершина графа, соответствует фунциональному блоку.
    = DFGNode (F v x)
    -- |Граф, где информация о вершинах хранится внутри функциональных блоков.
    | DFG [DataFlowGraph v x]
    --  |Множество взаимозаменяемых подграфов.
    --  | DFGSwitch
    --     { dfgKey   :: v -- ^ключ, по которому осуществляется выбор подграфа.
    --     , dfgCases :: [(Int, DataFlowGraph v)] -- ^таблица значений ключей и соответствующих подграфов.
    --     }
    deriving ( Show, Generic )

instance ( Var v, Typeable x ) => Patch (DataFlowGraph v x) (v, v) where
    -- FIXME: on complex DFG don't work correct
    patch diff@(v, v') (DFG dfgs) = DFG $ (DFGNode $ reg v [v']) : map (patch diff) dfgs
    patch diff@(v, _) n@(DFGNode f)
        | v `member` inputs f = DFGNode $ patch diff f
        | otherwise = n

instance ( Var v ) => Variables (DataFlowGraph v x) v where
    variables (DFGNode fb) = variables fb
    variables (DFG g)      = unionsMap variables g
    -- variables DFGSwitch{ dfgKey, dfgCases } = singleton dfgKey `union` unionsMap (variables . snd) dfgCases

instance WithFunctions (DataFlowGraph v x) (F v x) where
    functions (DFGNode f) = [ f ]
    functions (DFG g)     = concatMap functions g
    -- functions DFGSwitch{ dfgCases } = concatMap (functions . snd) dfgCases

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
data ModelState title v x t
    = Frame
        { processor :: BusNetwork title v x t
        , dfg       :: DataFlowGraph v x
        }
    --  | Level
    --     { currentFrame    :: ModelState title tag v x t
    --     , remainFrames    :: [ ModelState title tag v x t ]
    --     , completedFrames :: [ ModelState title tag v x t ]
    --     , initialFrame    :: ModelState title tag v x t
    --     }
    deriving ( Generic )


instance ( Var v
         ) => WithFunctions (ModelState title v x t) (F v x) where
    functions Frame{ processor } = functions processor


instance  ( Ord v ) =>
        DecisionProblem (BindingDT String v x)
              BindingDT (ModelState String v x t)
        where
    options _ Frame{ processor }    = options binding processor
    -- options _ Level{ currentFrame } = options binding currentFrame
    decision _ f@Frame{ processor } d = f{ processor=decision binding processor d }
    -- decision _ l@Level{ currentFrame } d = l{ currentFrame=decision binding currentFrame d }

instance ( Typeable title, Ord title, Show title, Var v, Time t
         , Typeable x
         ) => DecisionProblem (DataFlowDT title v t)
                   DataFlowDT (ModelState title v x t)
         where
    options _ Frame{ processor }    = options dataFlowDT processor
    -- options _ Level{ currentFrame } = options dataFlowDT currentFrame
    decision _ f@Frame{ processor } d = f{ processor=decision dataFlowDT processor d }
    -- decision _ l@Level{ currentFrame } d = l{ currentFrame=decision dataFlowDT currentFrame d }



targetProcessDuration Frame{ processor } = nextTick $ process processor


endpointOption2action o@EndpointO{ epoRole }
    = let
        a = o^.at.avail.infimum
        -- "-1" - необходимо, что бы не затягивать процесс на лишний такт, так как интервал включает
        -- граничные значения.
        b = o^.at.avail.infimum + o^.at.dur.infimum - 1
    in EndpointD epoRole (a ... b)


isSchedulingComplete Frame{ processor, dfg }
    | let inWork = fromList $ transfered processor
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
