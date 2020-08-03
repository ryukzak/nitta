{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|
Module      : NITTA.Project.Implementation
Description : Types for target system implementation description.
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Project.Implementation
    ( TargetEnvironment(..)
    , Implementation(..)
    , UnitEnv(..)
    , Parameter(..)
    , TargetSystemComponent(..)
    ) where

import           NITTA.Model.ProcessorUnits.Time


-- |Реализация компонента системы или её фрагмента (HW или SW).

-- TODO: Рассмотреть возможность отказа от Empty в пользу оборачивания в Maybe.
data Implementation
    -- |Непосредственно реализация компонента.
    = Immediate { impFileName, impText :: String }
    -- |Библиотечный элемент, приведённый в указанном файле.
    | FromLibrary { impFileName :: String }
    -- |Релизация описывается совокупностью файлов располагаемых в указанном каталоге относительно
    -- рабочей папки.
    | Aggregate { impPath :: Maybe String, subComponents :: [ Implementation ] }
    -- |Реализация не требуется (к примеру: для многих вычислительных блоков ПО отсутствует).
    | Empty


-- |Type class for target components. Target - a target system project or a testbench.
class TargetSystemComponent pu where
    -- |Наименование аппаратного модуля, соответствующего вычислительному блоку.
    moduleName :: String -> pu -> String

    -- |Программное обеспечение для вычислительного блока. Под ПО вычислительного блока понимается
    -- настройка вычислительного блока, которая может меняться при изменении прикладного алгоритма
    -- без повторного синтеза аппаратуры. Наличие ПО не является обязательным.
    software :: String -> pu -> Implementation

    -- |Аппаратное обеспечение вычислительного блока. Это может быть ссылка на библиотеку,
    -- сгенерированный файл либо их совокупность.
    --
    -- В связи с тем, что в проекте используется несколько целевых платформ (на момент написания это
    -- Icarus Verilog для тестирования и Quartus для синтеза), для отдельных вычислительных блоков
    -- может осуществляться конфигурировние.
    hardware :: String -> pu -> Implementation

    -- |Генерация фрагмента исходного кода для создания экземпляра вычислительного блока в рамках
    -- вычислительной платформы NITTA.
    hardwareInstance :: String -> pu -> TargetEnvironment -> Ports pu -> IOPorts pu -> String


data Parameter
    = InlineParam String
    | IntParam Int
    deriving ( Eq, Ord )

instance Show Parameter where
    show (IntParam i)    = show i
    show (InlineParam s) = s


-- |Target mUnit environment, including IO ports, clk, rst and cycle signals.
data TargetEnvironment
    = TargetEnvironment
        { signalClk        :: String -- ^clock
        , signalRst        :: String -- ^reset
        , signalCycleBegin :: String -- ^posedge on computation cycle begin
        , signalInCycle    :: String -- ^positive on computation cycle
        , signalCycleEnd   :: String -- ^posedge on computation cycle end
        , inputPort        :: InputPortTag -> String
        , outputPort       :: OutputPortTag -> String
        , inoutPort        :: InoutPortTag -> String
        , unitEnv          :: UnitEnv -- unit specific environment
        }


data UnitEnv
    -- |Environment of process unit.
    = ProcessUnitEnv
        { parameterAttrWidth :: Parameter
        , dataIn, attrIn     :: String -- ^bus name
        , dataOut, attrOut   :: String -- ^bus name
        , signal             :: SignalTag -> String -- ^control signal
        }
    -- |Environment of network.
    | NetworkEnv
