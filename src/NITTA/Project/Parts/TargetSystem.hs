{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : NITTA.Project.Parts.TargetSystem
Description : Target system implementation (hardware and software).
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Project.Parts.TargetSystem (
    writeTargetSystem,
) where

import NITTA.Project.Implementation
import NITTA.Project.Parts.Utils
import NITTA.Project.Types
import System.Directory (createDirectoryIfMissing)

writeTargetSystem prj@Project{pName, pPath, pUnit} = do
    createDirectoryIfMissing True pPath
    writeImplementation pPath $ hardware pName pUnit
    writeImplementation pPath $ software pName pUnit
    copyLibraryFiles prj
