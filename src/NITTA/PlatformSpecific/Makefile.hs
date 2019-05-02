
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.PlatformSpecific.Makefile
Description : IcarusVerilog and Makefile platform specific files
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
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
|               iverilog { S.join " " $ projectFiles prj }
|               vvp a.out
|           modelsim:
|               modelsim -do sim.do
|           |]

