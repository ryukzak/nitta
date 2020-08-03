{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : NITTA.Project.Parts.Quartus
Description : Quartus project for DE0Nano platform
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Project.Parts.Quartus
    ( QuartusProject(..)
    ) where

import           Data.FileEmbed
import qualified Data.String.Utils as S
import           NITTA.Model.Networks.Bus
import           NITTA.Model.Types
import           NITTA.Project.Implementation
import           NITTA.Project.Parts.TestBench
import           NITTA.Project.Types
import           NITTA.Utils
import           System.Directory ( createDirectoryIfMissing )
import           System.FilePath.Posix ( joinPath )


data QuartusProject = QuartusProject

instance ( VarValTime v x t
        ) => ProjectPart QuartusProject (Project (BusNetwork String v x t) v x) where
    writePart QuartusProject prj@Project{ pPath } = do
        createDirectoryIfMissing True pPath
        writeModelsimDo prj
        writeQuartus prj


-- |Modelsim configuration. FIXME: Known problem: running modelsim from Quartus.
writeModelsimDo prj@Project{ pPath } = do
    let files = projectFiles prj
        tb = testBenchTopModuleName prj
    writeFile ( joinPath [ pPath, "wave.do" ] )
        $ renderST
            $(embedStringFile "template/modelsim/wave.do")
            [ ( "top_level", tb ) ]
    writeFile ( joinPath [ pPath, "sim.do" ] )
        $ renderST
            $(embedStringFile "template/modelsim/sim.do")
            [ ( "top_level", tb )
            , ( "verilog_files", S.join "\n" $ map (\fn -> "vlog -vlog01compat -work work +incdir+$path $path/" ++ fn) files )
            ]


-- |Сгенерировать служебные файлы для Quartus.
writeQuartus prj@Project{ pName, pPath, pUnit } = do
    let files = projectFiles prj
        tb = testBenchTopModuleName prj
    writeFile (joinPath [ pPath, "nitta.qpf" ]) quartusQPF
    writeFile (joinPath [ pPath, "nitta.qsf" ]) $ quartusQSF tb files
    writeFile (joinPath [ pPath, "nitta.sdc" ]) quartusSDC
    writeFile ( joinPath [ pPath, "nitta.v" ] )
        $ renderST
            $(embedStringFile "template/quartus/nitta.v")
            [ ( "top_level_module"
              , hardwareInstance (moduleName pName pUnit) pUnit (bnEnv pUnit) BusNetworkPorts (bnIOPorts pUnit)
              )
            ]
    writeFile ( joinPath [ pPath, "pll.v" ] )
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
