{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

{-|
В общем случае, вычислительный блок может обладать произвольным поведением, в том числе и выпонять
несоколько функциональных блоков параллельно. Как правило, это не так, и вычислительный блок может
выполнять функциональные блоки строго последовательно, один за другим. Для таких вычислительных
блоков значительная часть реализации модели стало бы идентичной, в связи, с чем с целью повторного
использования, был реализован данный модуль предоставляющий эту логику в виде обёртки вокруг
состояния вычислительного блока.
-}

module NITTA.ProcessUnits.SerialPU
  ( SerialPU (SerialPU)
  , SerialPUState (..)
  , serialSchedule
  ) where

import           Control.Lens        hiding (at, from, (...))
import           Control.Monad.State
import           Data.Default
import           Data.Either
import           Data.List           (find, intersect)
import           Data.Maybe
import           Data.Typeable
import           NITTA.Lens
import           NITTA.Types
import           NITTA.Utils
import           Numeric.Interval    ((...))




-- | Внешняя обёртка для вычислительных блоков, выполняющих функции последовательно.
data SerialPU st io v t
  = SerialPU
  { -- | Внутрее состояние вычислительного блока. Конкретное состояние зависит от конкретного типа.
    spuState   :: st
  , spuCurrent :: Maybe (CurrentJob io v t)
  -- | Список привязанных к вычислительному блоку функций, но работа над которыми ещё не началась.
  -- Второе значение - ссылка на шаг вычислительного процесса, описывающий привязку функции
  -- к вычислительному блоку.
  , spuRemain  :: [(FB io v, ProcessUid)]
  -- | Описание вычислительного процесса.
  , spuProcess :: Process v t
  } deriving ( Show )

instance ( Time t, Var v, Default st ) => Default (SerialPU st Parcel v t) where
  def = SerialPU def def def def



-- | Описание текущей работы вычислительного блока.
data CurrentJob io v t
  = CurrentJob
  { cFB    :: FB io v -- ^ Текущая функция.
  , cStart :: t -- ^ Момент времни, когда функция начала вычисляться.
  -- | Выполненные для данной функции вычислительные шаги. Необходимо в значительной
  -- степени для того, чтобы корректно задать все вертикальные отношения между уровнями по
  -- завершению работы над функциональным блоком..
  , cSteps :: [ProcessUid]
  } deriving ( Show )



-- | Основная логика работы последовательного вычислительного блока строится вокруг его состояния,
-- реализующего следующий интерфейс:
class SerialPUState st io v t | st -> io, st -> v, st -> t where
  -- | Привязать функцию к текущему состоянию вычислительного блока. В один момент времени только
  -- один функциональный блок.
  bindToState :: FB io v -> st -> Either String st
  -- | Получить список вариантов развития вычислительного процесса, на основе предоставленного
  -- состояния последовательного вычислительного блока.
  stateOptions :: st -> t -> [Option Passive v t]
  -- | Получить данные для планирования вычислительного процесса состояния. Результат функции:
  --
  -- - состояние после выполнения вычислительного процесса;
  -- - монада State, которая сформирует необходимое описание многоуровневого вычислительного
  --   процессса.
  schedule :: st -> Action Passive v t -> (st, State (Process v t) [ProcessUid])



instance ( Var v, Time t
         , Default st
         , SerialPUState st Parcel v t
         ) => PUClass Passive (SerialPU st Parcel v t) v t where

  bind fb pu@SerialPU{..}
    -- Почему делается попытка привязать функцию к нулевому состоянию последовательного вычислителя,
    -- а не к текущему? Потому что, успешная привязка функции производится к объёртке (помещаем ФБ
    -- в spuRemain), а не к самому состоянию. Ведь к самому состоянию может быть привязана в один
    -- момент времени только один функциональный блок.
    = case fb `bindToState` (def :: st) of
        Right _ -> let (key, spuProcess') = modifyProcess spuProcess $ bindFB fb $ nextTick spuProcess
                   in Right pu{ spuRemain=(fb, key) : spuRemain
                              , spuProcess=spuProcess'
                              }
        Left reason -> Left reason

  options SerialPU{ spuCurrent=Nothing, .. }
    = concatMap ((\f -> f $ nextTick spuProcess) . stateOptions)
      $ rights $ map (\(fb, _) -> bindToState fb spuState) spuRemain
  options SerialPU{ spuCurrent=Just _, .. }
    = stateOptions spuState $ nextTick spuProcess

  select pu@SerialPU{ spuCurrent=Nothing, .. } act
    | Just (fb, compilerKey) <- find (not . null . (variables act `intersect`) . variables . fst) spuRemain
    , Right spuState' <- bindToState fb spuState
    = select pu{ spuState=spuState'
               , spuCurrent=Just CurrentJob
                             { cFB=fb
                             , cStart=act^.at.infimum
                             , cSteps=[ compilerKey ]
                             }
              } act
    | otherwise = error "Variable not found in binded functional blocks."
  select pu@SerialPU{ spuCurrent=Just cur, .. } act
   | nextTick spuProcess > act^.at.infimum
   = error $ "Time wrap! Time: " ++ show (nextTick spuProcess) ++ " Act start at: " ++ show (act^.at.infimum)
   | otherwise
    = let (spuState', work) = schedule spuState act
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
      finish p CurrentJob{..} = snd $ modifyProcess p $ do
        h <- addActivity (cStart ... (act^.at.infimum + act^.at.dur)) $ FBStep cFB
        mapM_ (relation . Vertical h) cSteps

  process = spuProcess

  setTime t pu@SerialPU{..} = pu{ spuProcess=spuProcess{ nextTick=t } }


instance ( Var v, Time t
         , Default (Instruction (SerialPU st Parcel v t))
         , Show (Instruction (SerialPU st Parcel v t))
         , Typeable st, Default st
         , SerialPUState st Parcel v t
         , UnambiguouslyDecode (SerialPU st Parcel v t)
         ) => ByTime (SerialPU st Parcel v t) t where
  signalAt pu@SerialPU{..} t sig
    = let instr = fromMaybe def $ extractInstructionAt pu t
      in decodeInstruction instr sig



-- * Утилиты --------------------------------------------------------

-- | Простой способ спланировать вычислительный процесс последовательного вычислительного блока.
-- На вход подаётся тип вычислительного блока вместе с обёрткой, действие и инструкция его
-- реализующая (само собой работает только в том случае, если инструкция действительно одна и
-- должна выставляться только на длительность действия). Результат - преобразование над описанием
-- вычислительного процесса State.

serialSchedule
  :: ( Show (Instruction pu), Default (Instruction pu), Var v, Time t, Typeable pu )
  => Proxy pu -> Action Passive v t -> Instruction pu -> State (Process v t) [ProcessUid]
serialSchedule proxy act instr = do
  now <- getProcessTime
  e <- addActivity (act^.at) $ EffectStep $ eaEffect act
  i <- addActivity (act^.at) $ InstructionStep instr
  is <- if now < act^.at.infimum
        then do
            ni <- addActivity (now ... act^.at.infimum - 1) $ InstructionStep $ nopFor proxy
            return [i, ni]
        else return [i]
  mapM_ (relation . Vertical e) is
  setProcessTime $ (act^.at.supremum) + 1
  return $ e : is
