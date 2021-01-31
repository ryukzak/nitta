{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : NITTA.Project.Types
Description : Types for a target project description and generation
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Project.Types (
    Project (..),
) where

import Data.Default
import NITTA.Intermediate.Types
import NITTA.Intermediate.Value ()

{- |Target project for different purpose (testing, target system, etc). Should
be writable to disk.
-}

-- FIXME: collision between target project name and output directory. Maybe
-- pName or pPath should be maybe? Or both?
data Project m v x = Project
    { -- |target project name
      pName :: String
    , -- |IP-core library directory
      pLibPath :: String
    , -- |output directory
      pPath :: String
    , -- |'mUnit' model (a mUnit unit for testbench or network for complete NITTA mUnit)
      pUnit :: m
    , -- |testbench context with input values
      pTestCntx :: Cntx v x
    }
    deriving (Show)

instance (Default x) => DefaultX (Project m v x) x
