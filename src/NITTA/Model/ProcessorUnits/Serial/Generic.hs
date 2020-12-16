{- FOURMOLU_DISABLE -}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : NITTA.Model.ProcessorUnits.Serial.Generic
Description : Generic serial mUnit model
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

В общем случае, вычислительный блок может обладать произвольным поведением, в том числе и выпонять
несоколько функциональных блоков параллельно. Как правило, это не так, и вычислительный блок может
выполнять функциональные блоки строго последовательно, один за другим. Для таких вычислительных
блоков значительная часть реализации модели стало бы идентичной, в связи, с чем с целью повторного
использования, был реализован данный модуль предоставляющий эту логику в виде обёртки вокруг
состояния вычислительного блока.
-}

module NITTA.Model.ProcessorUnits.Serial.Generic
  ( SerialPU (SerialPU, spuState)
  , SerialPUState (..)
  , serialSchedule
  ) where

import           Control.Monad.State
import           Data.Default
import           Data.Either
import           Data.List ( find )
import           Data.Set ( elems, (\\) )
import qualified Data.Set as S
import           Data.Typeable
import           NITTA.Intermediate.Types
import           NITTA.Model.Problems
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Model.Types
import           NITTA.Utils
import           Numeric.Interval ( Interval, inf, sup, width, (...) )


-- | Внешняя обёртка для вычислительных блоков, выполняющих функции последовательно.
data SerialPU st v x t
  = SerialPU
  { -- | Внутрее состояние вычислительного блока. Конкретное состояние зависит от конкретного типа.
    spuState   :: st
  , spuCurrent :: Maybe (CurrentJob v x t)
  -- | Список привязанных к вычислительному блоку функций, но работа над которыми ещё не началась.
  -- Второе значение - ссылка на шаг вычислительного процесса, описывающий привязку функции
  -- к вычислительному блоку.
  , spuRemain  :: [(F v x, ProcessStepID)]
  -- | Описание вычислительного процесса.
  , spuProcess :: Process v x t
  , spuFBs     :: [F v x]
  }

instance ( Show st
         , Show (Process v x t)
         ) => Show (SerialPU st v x t) where
  show SerialPU{ spuState, spuProcess }
    = "SerialPU{spuState=" ++ show spuState
      ++ "spuProcess=" ++ show spuProcess
      ++ "}"

instance ( Time t, Default st ) => Default (SerialPU st v x t) where
  def = SerialPU def def def def def


instance WithFunctions (SerialPU st v x t) (F v x) where
  functions SerialPU{ spuFBs } = spuFBs


-- | Описание текущей работы вычислительного блока.
data CurrentJob v x t
  = CurrentJob
  { cFB    :: F v x -- ^ Текущая функция.
  , cStart :: t -- ^ Момент времни, когда функция начала вычисляться.
  -- | Выполненные для данной функции вычислительные шаги. Необходимо в значительной
  -- степени для того, чтобы корректно задать все вертикальные отношения между уровнями по
  -- завершению работы над функциональным блоком..
  , cSteps :: [ ProcessStepID ]
  }



-- | Основная логика работы последовательного вычислительного блока строится вокруг его состояния,
-- реализующего следующий интерфейс:
class ( VarValTime v x t ) => SerialPUState st v x t | st -> v x t where
  -- | Привязать функцию к текущему состоянию вычислительного блока. В один момент времени только
  -- один функциональный блок.
  bindToState :: F v x -> st -> Either String st
  -- | Получить список вариантов развития вычислительного процесса, на основе предоставленного
  -- состояния последовательного вычислительного блока.
  stateOptions :: st -> t -> [ EndpointSt v (TimeConstrain t) ]
  -- | Получить данные для планирования вычислительного процесса состояния. Результат функции:
  --
  -- - состояние после выполнения вычислительного процесса;
  -- - монада State, которая сформирует необходимое описание многоуровневого вычислительного
  --   процессса.
  simpleSynthesis
      :: st -> EndpointSt v (Interval t)
      -> ( st, State (Process v x t) [ ProcessStepID ] )



instance ( Default st, SerialPUState st v x t
        ) => EndpointProblem (SerialPU st v x t) v t
        where
  endpointOptions SerialPU{ spuCurrent=Nothing, spuRemain, spuState, spuProcess }
    = concatMap ((\f -> f $ nextTick spuProcess) . stateOptions)
      $ rights $ map (\(fb, _) -> bindToState fb spuState) spuRemain
  endpointOptions SerialPU{ spuCurrent=Just _, spuState, spuProcess }
    = stateOptions spuState $ nextTick spuProcess

  endpointDecision pu@SerialPU{ spuCurrent=Nothing, spuRemain, spuState } act@EndpointSt{ epAt }
    | Just (fb, compilerKey) <- find (not . S.null . (variables act `S.intersection`) . variables . fst) spuRemain
    , Right spuState' <- bindToState fb spuState
    = endpointDecision pu
        { spuState=spuState'
        , spuCurrent=Just CurrentJob
                      { cFB=fb
                      , cStart=inf epAt
                      , cSteps=[ compilerKey ]
                      }
        , spuRemain=filter ((/= fb) . fst) spuRemain
        } act
    | otherwise = error "Variable not found in binded functional blocks."
  endpointDecision pu@SerialPU{ spuCurrent=Just cur, spuState, spuProcess } act@EndpointSt{ epAt }
   | nextTick spuProcess > inf epAt
   = error $ "Time wrap! Time: " ++ show (nextTick spuProcess) ++ " Act start at: " ++ show (inf epAt)
   | otherwise
    = let (spuState', work) = simpleSynthesis spuState act
          (steps, spuProcess') = modifyProcess spuProcess work
          cur' = cur{ cSteps=steps ++ cSteps cur }
          pu' = pu{ spuState=spuState'
                  , spuProcess=spuProcess'
                  , spuCurrent=Just cur
                  }
          nextOptions = stateOptions spuState' (nextTick spuProcess')
      in case nextOptions of
           [] -> pu'{ spuCurrent=Nothing
                    , spuProcess=finish spuProcess' cur'
                    }
           _  -> pu'
    where
      finish p CurrentJob{ cFB, cStart } = snd $ modifyProcess p $ do
        _h <- addStep (cStart ... (inf epAt + width epAt)) $ FStep cFB
        return ()
        -- mapM_ (relation . Vertical h) cSteps



instance ( Default st
         , SerialPUState st v x t
         ) => ProcessorUnit (SerialPU st v x t) v x t where

  tryBind fb pu@SerialPU{ spuFBs, spuRemain, spuProcess }
    -- Почему делается попытка привязать функцию к нулевому состоянию последовательного вычислителя,
    -- а не к текущему? Потому что, успешная привязка функции производится к объёртке (помещаем ФБ
    -- в spuRemain), а не к самому состоянию. Ведь к самому состоянию может быть привязана в один
    -- момент времени только один функциональный блок.
    = case fb `bindToState` (def :: st) of
        Right _ -> let (key, spuProcess') = modifyProcess spuProcess $ bindFB fb $ nextTick spuProcess
                   in Right pu{ spuRemain=(fb, key) : spuRemain
                              , spuProcess=spuProcess'
                              , spuFBs=fb : spuFBs
                              }
        Left reason -> Left reason

  process = spuProcess


instance ( Var v, SerialPUState st v x t, Default st ) => Locks (SerialPU st v x t) v where
    locks SerialPU{ spuCurrent=Nothing } = []
    locks pu@SerialPU{ spuCurrent=Just CurrentJob{ cFB }, spuRemain } = let
            workInProgress = variables cFB
            already = transferred pu
            available = unionsMap variables $ endpointOptions pu
        in
            [ Lock{ locked, lockBy }
            | locked <- elems $ S.unions
                [ unionsMap (variables . fst) spuRemain
                , workInProgress \\ already \\ available
                ]
            , lockBy <- elems available
            ]

instance RefactorProblem (SerialPU st v x t) v x


-- * Утилиты --------------------------------------------------------

-- | Простой способ спланировать вычислительный процесс последовательного вычислительного блока.
-- На вход подаётся тип вычислительного блока вместе с обёрткой, действие и инструкция его
-- реализующая (само собой работает только в том случае, если инструкция действительно одна и
-- должна выставляться только на длительность действия). Результат - преобразование над описанием
-- вычислительного процесса State.

serialSchedule
  :: ( Show (Instruction pu), Time t, Typeable pu )
  => Instruction pu -> EndpointSt v (Interval t) -> State (Process v x t) [ ProcessStepID ]
serialSchedule instr act@EndpointSt{ epAt } = do
  e <- addStep epAt $ EndpointRoleStep $ epRole act
  i <- addStep epAt $ InstructionStep instr
  -- mapM_ (relation . Vertical e) [i]
  setProcessTime $ sup epAt + 1
  return [e, i]
