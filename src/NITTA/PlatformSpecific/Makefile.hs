
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

-- |Модуль отвечающий за генерацию проектов на базе процессора NITTA.
module NITTA.PlatformSpecific.Makefile
    ( makefile 
    ) where

import qualified Data.String.Utils             as S
import           NITTA.Types.Project
import           NITTA.Utils
import           System.FilePath.Posix         (joinPath)
import           Text.InterpolatedString.Perl6 (qc)


makefile prj@Project{ projectPath }
    = writeFile (joinPath [ projectPath, "Makefile" ]) $ space2tab $ fixIndent [qc|
|           icarus:
|               iverilog { S.join " " $ snd $ projectFiles prj }
|               vvp a.out
|           modelsim:
|               modelsim -do sim.do
|           |]

