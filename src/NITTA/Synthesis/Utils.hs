{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Synthesis.Utils
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Synthesis.Utils
    ( targetProcessDuration
    , isPUSynthesisFinite
    ) where

import           NITTA.Intermediate.Types
import           NITTA.Model.Problems
import           NITTA.Model.ProcessorUnits.Time
import           NITTA.Model.TargetSystem
import           NITTA.Utils


-- |Processor unit synthesis is finite when all binded function can be
-- processed. Checking is very simple and don't work correct for pipelined
-- units.

-- FIXME: Should be moved.
isPUSynthesisFinite pu
    = case endpointOptions pu of
        [] -> let
                algVars = unionsMap variables $ functions pu
                processVars = unionsMap variables $ getEndpoints $ process pu
            in algVars == processVars
        o:_ -> let
                d = endpointOptionToDecision o
                pu' = endpointDecision pu d
            in isPUSynthesisFinite pu'


-- FIXME: Should be moved.
targetProcessDuration ModelState{ mUnit } = nextTick $ process mUnit
