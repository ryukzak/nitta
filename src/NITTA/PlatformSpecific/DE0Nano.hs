{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

-- |Модуль отвечающий за генерацию проектов на базе процессора NITTA.
module NITTA.PlatformSpecific.DE0Nano
    ( de0nano
    ) where

import           Control.Monad                 (mapM_, unless)
import           Data.FileEmbed
import           Data.List                     (isSubsequenceOf)
import qualified Data.List                     as L
import qualified Data.String.Utils             as S
import           NITTA.Functions               as F
import           NITTA.Types
import           NITTA.Types.Project
import           NITTA.Utils
import           System.Directory
import           System.Exit
import           System.FilePath.Posix         (joinPath, pathSeparator)
import           System.IO                     (hPutStrLn, stderr)
import           System.Process
import           Text.InterpolatedString.Perl6 (qc)

-- FIXME: Replace embedStringFile by fixIndent + qc

de0nano prj = do
    writeModelsimDo prj
    writeQuartus prj


-- |Сгенерировать служебные файлы для симуляции при помощи ModelSim.

-- FIXME: Исправить интеграцию Modelsim и Quartus (прозрачный запуск симуляции по кнопке из
-- Quartus).
writeModelsimDo prj@Project{ projectPath } = do
    let (tb, files) = projectFiles prj
    writeFile ( joinPath [ projectPath, "wave.do" ] )
        $ renderST
            $(embedStringFile "template/modelsim/wave.do")
            [ ( "top_level", tb ) ]
    writeFile ( joinPath [ projectPath, "sim.do" ] )
        $ renderST
            $(embedStringFile "template/modelsim/sim.do")
            [ ( "top_level", tb )
            , ( "verilog_files", S.join "\n" $ map (\fn -> "vlog -vlog01compat -work work +incdir+$path $path/" ++ fn) files )
            ]


-- |Сгенерировать служебные файлы для Quartus.
writeQuartus prj@Project{ projectName, projectPath, processorModel } = do
    let (tb, files) = projectFiles prj
    writeFile (joinPath [ projectPath, "nitta.qpf" ]) quartusQPF
    writeFile (joinPath [ projectPath, "nitta.qsf" ]) $ quartusQSF tb files
    writeFile (joinPath [ projectPath, "nitta.sdc" ]) quartusSDC
    writeFile ( joinPath [ projectPath, "nitta.v" ] )
        $ renderST
            $(embedStringFile "template/quartus/nitta.v")
            [ ( "top_level_module", moduleName projectName processorModel ) ]
    writeFile ( joinPath [ projectPath, "pll.v" ] )
        $(embedStringFile "template/quartus/pll.v")

quartusQPF = $(embedStringFile "template/quartus/project_file.qpf") :: String

quartusQSF tb files = renderST $(embedStringFile "template/quartus/settings_file.qsf")
    [ ( "verilog_files"
      , S.join "\n" $ map ("set_global_assignment -name VERILOG_FILE " ++) files
      )
    , ( "test_bench_files"
      , S.join "\n" $ map (\fn -> "set_global_assignment -name EDA_TEST_BENCH_FILE " ++ fn ++ " -section_id " ++ tb) files
      )
    , ( "testbench_module", tb )
    ]

quartusSDC = $(embedStringFile "template/quartus/synopsys_design_constraint.sdc") :: String
