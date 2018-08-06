{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
{-# OPTIONS_HADDOCK ignore-exports #-}
{-# OPTIONS_HADDOCK prune #-}

{-|
Module : Multiplier (Умножитель)

В данном модуле реализуется модель вычислительного блока
умножителя для САПР. Цель данной модели - «научить» САПР работать
с вычислительным блоком, а именно:

* какие функциональный блоки могут быть вычислены с
  использованием данного вычислительного блока (класс
  ProcessUnit, функция bind);
* назначить экземпляру вычислительного блока выполнение
  указанного функционального блока (класс ProcessUnit, функция
  bind);

* система команд вычислительного блока и набор сигналов, позволяющий 
  им управлять (класс типов `Controllable`);
* функция отображения инструкций 

* какие есть варианты (options) развития вычислительного процесса (загрузить
  или выгрузить ту или иную переменную);
* какую последовательность сигналов необходимо сформировать, чтобы 
  вычислительный блок выполнил работу (decision);


Вычислительный блок может вычислять следующие функциональные
блоки:

* `Multiply a b c` <=> `c := a*b`.

Рассмотрим пример планирования вычислительного процесса для данного 
вычислительного блока, для чего запустим `stack repl`.
>>>> ...

TODO: Перспективным направлением по развитию данного вычислительного
блока является внедрение в него регистра накопителя, что позволит
перемножать произвольное количество аргументов, что сократит количество
транзакций по шине данных при перемножении более двух переменных.

Данный модуль следует рассматривать как референтный при
реализации других вычислительных блоков.
-}

module NITTA.ProcessUnits.Multiplier
    ( Multiplier(..)
    , PUPorts(..)
    , multiplier
    ) where

import           Data.Default
import           Data.List            (find, (\\))
import           Data.Set             (elems, fromList, member)
import           Data.Typeable
import           NITTA.FunctionBlocks (castFB)
import qualified NITTA.FunctionBlocks as FB
import           NITTA.Types
import           NITTA.Utils
import           NITTA.Utils.Process
import           Numeric.Interval     (inf, sup, (...))


-- | Идентификатор аргумента операции умножения.
--
-- В текущей реализации умножитель рассчитан исключительно на
-- перемножение двух чисел. Причем:
--
-- - с аппаратной точки зрения: порядок загрузки аргументов
--   строго зафиксирован и не может быть изменен;
-- - с точки зрения модели порядок аргументов не имеет значения
--   и его можно изменить никак не уведомляя об этом аппаратную
--   часть.
data ArgumentSelector
    = A
    | B
    deriving ( Show, Eq )

-- | Состояние экземпляра вычислительного блока умножителя, где:
--
-- - v - идентификатор перемменой;
-- - x - тип значений, с которыми работает умножитель;
-- - t - идентификатор момента времени.
data Multiplier v x t
    = Multiplier
        { -- | Список назначенных, но еще необработанных или
          -- необрабатываемых функциональных блоков. Выполнение
          -- функционального блока начинается с:
          --
          -- - удаления функционального блока из данного списка;
          -- - переноса информации из функционального блока в
          --   поля `puTarget` и `puSource`.
          --
          -- Функциональные блоки могут выполняться в
          -- произвольном порядке. Хранение информации о
          -- выполненных функциональных блоках не осуществляется,
          -- так как она есть в описание вычислительного процесса
          -- `puProcess`.
          puRemain  :: [FB (Parcel v x)]
        , -- | Список переменных, которые необходимо загрузить в
          -- вычислительный блок.
          --
          -- TODO: Удалить ArgumentSelector, позволит загружать
          -- аргументы в любом порядке.
          puTarget  :: [(ArgumentSelector, v)]
          -- | Список переменных, которые необходимо выгрузить из
          -- вычислительного блока в любом порядке. Важно
          -- отметить, что все выгружаемые переменные
          -- соответствуют одному значению - результату
          -- умножения.
        , puSource  :: [v]
          -- | Описание вычислительного процесса, спланированного
          -- для данного вычислительного блока.
        , puProcess :: Process (Parcel v x) t
          -- | В реализации данного вычислительного блока
          -- используется IP ядро поставляемое вместе с Altera
          -- Quartus, что не позволяет осуществлять симуляцию при
          -- помощи Icarus Verilog. Чтобы обойти эту проблему
          -- была создана заглушка, подключаемая вместо IP ядра
          -- если установлен данный флаг.
        , puMocked  :: Bool
        }
    deriving ( Show )

instance ( Default t ) => Default (Multiplier v x t) where
    def = Multiplier [] [] [] def False

multiplier :: ( Default t ) => Multiplier v x t
multiplier = def
-- TODO: multiplier mock = def{ puMock=mock }


instance ( Var v, Time t
         ) => ProcessUnit (Multiplier v x t) (Parcel v x) t where
    tryBind fb pu@Multiplier{ puRemain }
        | Just FB.Multiply{} <- castFB fb = Right pu{ puRemain=fb : puRemain }
        | otherwise = Left $ "Unknown functional block: " ++ show fb
    process = puProcess
    setTime t pu@Multiplier{ puProcess } = pu{ puProcess=puProcess{ nextTick=t } }

execute pu@Multiplier{ puTarget=[], puSource=[], puRemain } fb
    | Just (FB.Multiply (I a) (I b) (O c)) <- castFB fb = pu{ puTarget=[(A, a), (B, b)], puSource=elems c, puRemain=puRemain \\ [ fb ] }
execute _ _ = error ""


instance ( Var v, Time t
         , Typeable x
         ) => DecisionProblem (EndpointDT v t)
                   EndpointDT (Multiplier v x t)
        where

    options _proxy Multiplier{ puTarget=(_, v):_, puProcess=Process{ nextTick } }
        = [ EndpointO (Target v) $ TimeConstrain (nextTick ... maxBound) (1 ... maxBound) ]
    options _proxy Multiplier{ puSource, puProcess=Process{ nextTick } } | not $ null puSource
        = [ EndpointO (Source $ fromList puSource) $ TimeConstrain (nextTick + 2 ... maxBound) (1 ... maxBound) ]
    options proxy pu@Multiplier{ puRemain } = concatMap (options proxy . execute pu) puRemain

    decision _proxy pu@Multiplier{ puTarget=(sel, v'):xs, puProcess=Process{ nextTick } } d@EndpointD{ epdRole=Target v, epdAt }
        | v == v'
        = pu
            { puProcess=schedule pu $
                scheduleEndpoint d $ do
                    scheduleNopAndUpdateTick nextTick (inf epdAt - 1)
                    scheduleInstructionAndUpdateTick (inf epdAt) (sup epdAt) $ Load sel
            , puTarget=xs
            }
    decision _proxy pu@Multiplier{ puSource, puProcess=Process{ nextTick } } d@EndpointD{ epdRole=Source v, epdAt }
        | not $ null puSource
        , let puSource' = puSource \\ elems v
        , puSource' /= puSource
        = pu
            { puProcess=schedule pu $
                scheduleEndpoint d $ do
                    scheduleNopAndUpdateTick nextTick (inf epdAt - 1)
                    scheduleInstructionAndUpdateTick (inf epdAt) (sup epdAt) Out
            , puSource=puSource'
            }
    decision proxy pu@Multiplier{ puTarget=[], puSource=[], puRemain } d
        | let v = oneOf $ variables d
        , Just fb <- find (\fb -> v `member` variables fb) puRemain
        = decision proxy (execute pu fb) d
    decision _ pu d = error $ "Multiplier decision error\npu: " ++ show pu ++ ";\n decison:" ++ show d


-- |Низкоуровневые интерфейсы управления вычислительным блоком.
-- Описываются два уровня организации вычислительного процесса: на уровне 
-- инструкций вычислительного блока и на уровне микрокоманд 
-- вычислительного блока.
instance Controllable (Multiplier v x t) where
    -- |Инструкции вычислительного блока с точки зрения поведения для
    -- разработчика. Есть одна инструкция по умолчанию - Nop (воспроизводимая 
    -- для всех вычислительных блоков), означающая безопасное состояние для 
    -- вычислительного блока, в котором он может прибывать сколь угодно долго без 
    -- побочных эффектов.
    data Instruction (Multiplier v x t)
        = Nop
        | Load ArgumentSelector
        | Out
        deriving (Show)
    nop = Nop
    -- |Структура микрокода управляющего поведением вычислительного блока. 
    -- Может быть использован непосредственно для управления аппаратной частью
    -- вычислительного блока, так как содержит непосредственно значения управляющих 
    -- сигналов.
    data Microcode (Multiplier v x t)
        = Microcode
            { wrSignal :: Bool
            , selSignal :: Bool
            , oeSignal :: Bool
            }
        deriving ( Show, Eq, Ord )

-- TODO: Nop -> Nothing.
-- TODO: универсальная функция декодирования, в том числе и для BusNetwork.
instance UnambiguouslyDecode (Multiplier v x t) where
    decodeInstruction Nop       = Microcode
        { wrSignal=False
        , selSignal=False
        , oeSignal=False
        }
    decodeInstruction (Load A) = (decodeInstruction Nop){ wrSignal=True, selSignal=False }
    decodeInstruction (Load B) = (decodeInstruction Nop){ wrSignal=True, selSignal=True }
    decodeInstruction Out      = (decodeInstruction Nop){ oeSignal=True }



instance Connected (Multiplier v x t) where
    data PUPorts (Multiplier v x t)
        = PUPorts{ wr, wrSel, oe :: Signal } deriving ( Show )
    transmitToLink Microcode{..} PUPorts{..}
        =
            [ (wr, Bool wrSignal)
            , (wrSel, Bool selSignal)
            , (oe, Bool oeSignal)
            ]


instance ( Var v
         , Integral x
         ) => Simulatable (Multiplier v x t) v x where
    simulateOn cntx _ fb
        | Just fb'@FB.Multiply{} <- castFB fb = simulate cntx fb'
        | otherwise = error $ "Can't siMultate " ++ show fb ++ " on Shift."


-- | Реализация кодогенерации для встраивания вычислительного блока в
-- процессор.
instance ( Time t, Var v
         ) => TargetSystemComponent (Multiplier v x t) where
    -- | Наименование аппаратного модуля, соответствующего данному
    -- вычислительному блоку.
    moduleName _ _ = "pu_mult"
    -- | Программное обеспечение для данного вычислительного блока отсутствует.
    -- Под ПО вычислительного блока понимается настройка вычислительного
    -- блока, которая может меняться при изменении прикладного алгоритма без
    -- повторного синтеза аппаратуры.
    software _ _ = Empty
    -- | Аппаратное обеспечение вычислительного блока умножителя. Состоит из
    -- нескольких файлов, самого умножителя и окружения для встраивания в
    -- микроархитектуру NITTA. Причем умножитель может быть выбран либо
    -- стандартный (`_inner`), либо заглушка (`_mock`).
    hardware title pu@Multiplier{ puMocked }
        = Aggregate Nothing
            [ if puMocked
                then FromLibrary "mult/mult_mock.v"
                else FromLibrary "mult/mult_inner.v"
            , FromLibrary $ "mult/" ++ moduleName title pu ++ ".v"
            ]
    -- | Генерация кода создания экземпляра вычислительного блока в рамках
    -- вычислительной платформы NITTA.
    hardwareInstance title _ Enviroment{ net=NetEnv{..}, signalClk, signalRst } PUPorts{..} = renderMST
        [ "pu_mult"
        , "  #( .DATA_WIDTH( " ++ show parameterDataWidth ++ " )"
        , "   , .ATTR_WIDTH( " ++ show parameterAttrWidth ++ " )"
        , "   , .INVALID( 0 )" -- FIXME:
        , "   ) $name$"
        , "  ( .clk( " ++ signalClk ++ " )"
        , "  , .rst( " ++ signalRst ++ " )"
        , "  , .signal_wr( " ++ signal wr ++ " )"
        , "  , .signal_sel( " ++ signal wrSel ++ " )"
        , "  , .data_in( " ++ dataIn ++ " )"
        , "  , .attr_in( " ++ attrIn ++ " )"
        , "  , .signal_oe( " ++ signal oe ++ " )"
        , "  , .data_out( " ++ dataOut ++ " )"
        , "  , .attr_out( " ++ attrOut ++ " )"
        , "  );"
        ] [("name", title)]