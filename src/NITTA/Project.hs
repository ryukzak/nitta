{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}

{-|
Module      : NITTA.Project
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Project
    ( module NITTA.Project.Implementation
    , module NITTA.Project.Parts.Icarus
    , module NITTA.Project.Parts.Quartus
    , module NITTA.Project.Parts.TargetSystem
    , module NITTA.Project.Parts.TestBench
    , module NITTA.Project.Parts.Utils
    , module NITTA.Project.Snippets
    , module NITTA.Project.Types
    , module NITTA.Project.Utils
    ) where

import           NITTA.Project.Implementation
import           NITTA.Project.Parts.Icarus
import           NITTA.Project.Parts.Quartus
import           NITTA.Project.Parts.TargetSystem
import           NITTA.Project.Parts.TestBench
import           NITTA.Project.Parts.Utils
import           NITTA.Project.Snippets
import           NITTA.Project.Types
import           NITTA.Project.Utils
