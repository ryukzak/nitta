{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|
Module      : NITTA.Project.Parts.TargetSystem
Description : Target system implementation (hardware and software).
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Project.Parts.TargetSystem
    ( TargetSystem(..)
    ) where

import           NITTA.Project.Implementation
import           NITTA.Project.Parts.Utils
import           NITTA.Project.Types
import           System.Directory             (createDirectoryIfMissing)


data TargetSystem = TargetSystem

instance ( TargetSystemComponent (m v x t)
        ) => ProjectPart TargetSystem (Project (m v x t) v x) where
    writePart TargetSystem prj@Project{ pName, pPath, pUnit } = do
        createDirectoryIfMissing True pPath
        writeImplementation pPath $ hardware pName pUnit
        writeImplementation pPath $ software pName pUnit
        copyLibraryFiles prj
