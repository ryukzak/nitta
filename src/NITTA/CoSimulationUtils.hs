{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS -fno-warn-redundant-constraints #-}

{- |
Module      : NITTA.Model.MultiplierDsl
Description : Provides functions to make decisions in PU
Copyright   : (c) Artyom Kostyuchik, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.CoSimulationUtils (
    puCoSim,
    naiveSynthesis,
) where

import Data.Default
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import NITTA.Intermediate.Functions ()
import NITTA.Intermediate.Simulation
import NITTA.Intermediate.Types
import NITTA.Model.Networks.Types
import NITTA.Model.Problems hiding (Bind, BreakLoop)
import NITTA.Model.ProcessorUnits
import NITTA.Model.TargetSystem ()
import NITTA.Project
import qualified NITTA.Project as P
import NITTA.Utils
import System.Directory
import System.FilePath.Posix (joinPath)

-- |Execute co-simulation test for the specific process unit
puCoSim ::
    ( PUClasses (pu String x Int) String x Int
    , WithFunctions (pu String x Int) (F String x)
    , P.Testable (pu String x Int) String x
    , DefaultX (pu String x Int) x
    ) =>
    String ->
    pu String x Int ->
    [(String, x)] ->
    [F String x] ->
    IO (TestbenchReport String x)
puCoSim name u cntxCycle alg = do
    wd <- getCurrentDirectory
    let mname = toModuleName name
        pTargetProjectPath = joinPath [wd, "gen", mname]
        prj =
            Project
                { pName = T.pack mname
                , pLibPath = "hdl"
                , pTargetProjectPath
                , pNittaPath = "."
                , pUnit = naiveSynthesis alg u
                , pUnitEnv = def
                , pTestCntx = simulateAlg 5 (CycleCntx $ M.fromList cntxCycle) [] alg
                , pTemplates = ["templates/Icarus"]
                }
    writeProject prj
    runTestbench prj

{- |Bind all functions to processor unit and synthesis process with endpoint
decisions.
-}
naiveSynthesis alg u0 = naiveSynthesis' $ foldl (flip bind) u0 alg
    where
        naiveSynthesis' u
            | opt : _ <- endpointOptions u =
                naiveSynthesis' $ endpointDecision u $ endpointOptionToDecision opt
            | otherwise = u