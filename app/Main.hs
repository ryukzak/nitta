{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS -fno-warn-orphans #-}

{- |
Module      : Main
Description : NITTA entry point
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module Main (main) where

import Control.Exception
import Control.Monad (when)
import Data.Default (def)
import Data.Maybe
import Data.Proxy
import qualified Data.String.Utils as S
import qualified Data.Text.IO as T
import Data.Version
import GHC.TypeLits
import NITTA.Intermediate.Simulation
import NITTA.Intermediate.Types
import NITTA.LuaFrontend
import NITTA.Model.Microarchitecture
import NITTA.Model.Networks.Bus
import NITTA.Model.Networks.Types
import NITTA.Model.ProcessorUnits
import NITTA.Project.Parts.TestBench
import NITTA.TargetSynthesis (TargetSynthesis (..), mkModelWithOneNetwork, runTargetSynthesis)
import NITTA.UIBackend
import Paths_nitta
import System.Console.CmdArgs hiding (def)
import System.Exit
import System.FilePath.Posix (joinPath)
import System.IO (stdout)
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Logger
import Text.Read
import Text.Regex

-- |Command line interface.
data Nitta = Nitta
    { filename :: FilePath
    , type_ :: String
    , io_sync :: IOSynchronization
    , port :: Int
    , n :: Int
    , fsim :: Bool
    , lsim :: Bool
    , verbose :: Bool
    , output_path :: String
    }
    deriving (Show, Data, Typeable)

deriving instance Data IOSynchronization

nittaArgs =
    Nitta
        { filename = def &= argPos 0 &= typFile
        , type_ = "fx32.32" &= help "Data type (default: 'fx32.32')"
        , io_sync = Sync &= help "IO synchronization mode: sync, async, onboard"
        , port = 0 &= help "Run nitta server for UI on specific port (by default - not run)"
        , n = 10 &= help "Number of computation cycles for simulation and testbench"
        , fsim = False &= help "Functional simulation with trace"
        , lsim = False &= help "Logical (HDL) simulation with trace"
        , verbose = False &= help "Verbose"
        , output_path = "gen" &= help "Place the output into specified directory"
        }
        &= summary ("nitta v" ++ showVersion version ++ " - CAD for reconfigurable real-time ASIP")

parseFX input =
    let typePattern = mkRegex "fx([0-9]+).([0-9]+)"
        [m, b] = fromMaybe (error "incorrect Bus type input") $ matchRegex typePattern input
        convert = fromJust . someNatVal . read
     in (convert m, convert b)

main = do
    Nitta{port, filename, type_, io_sync, fsim, lsim, n, verbose, output_path} <- cmdArgs nittaArgs
    setupLogger verbose

    src <- readSourceCode filename
    ( \(SomeNat (_ :: Proxy m), SomeNat (_ :: Proxy b)) -> do
            let FrontendResult{frDataFlow, frTrace, frPrettyCntx} = lua2functions src
                -- FIXME: https://nitta.io/nitta-corp/nitta/-/issues/50
                -- data for sin_ident
                received = [("u#0", map (\i -> read $ show $ sin ((2 :: Double) * 3.14 * 50 * 0.001 * i)) [0 .. toEnum n])]
                ma = (microarch io_sync :: BusNetwork String String (Attr (FX m b)) Int)

            infoM "NITTA" $ "will trace: " <> S.join ", " (map (show . tvVar) frTrace)

            when (port > 0) $ do
                buf <- try $ readFile $ joinPath ["web", "src", "gen", "PORT"]
                let expect = case buf of
                        Right p -> readMaybe p
                        Left (_ :: IOError) -> Nothing
                warningIfUnexpectedPort expect port
                backendServer port received output_path $ mkModelWithOneNetwork ma frDataFlow
                exitSuccess

            when fsim $ functionalSimulation n received src

            TestbenchReport
                { tbLogicalSimulationCntx
                } <-
                synthesizeAndTest ma n frDataFlow received output_path

            when lsim $ do
                putCntx $ frPrettyCntx tbLogicalSimulationCntx
        )
        $ parseFX type_

setupLogger verbose = do
    let level = if verbose then DEBUG else NOTICE
    h <-
        streamHandler stdout level >>= \lh ->
            return $
                setFormatter lh (simpleLogFormatter "[$prio : $loggername] $msg")

    removeAllHandlers
    updateGlobalLogger "NITTA" (setLevel level . addHandler h)

readSourceCode filename = do
    infoM "NITTA" $ "read source code from: " <> show filename <> "..."
    when (null filename) $ error "no input files"
    src <- T.readFile filename
    infoM "NITTA" $ "read source code from: " <> show filename <> "...ok"
    return src

functionalSimulation n received src = do
    let FrontendResult{frDataFlow, frPrettyCntx} = lua2functions src
        cntx = simulateDataFlowGraph n def received frDataFlow
    infoM "NITTA" "run functional simulation..."
    putCntx $ frPrettyCntx cntx
    infoM "NITTA" "run functional simulation...ok"

synthesizeAndTest ma n dataflow received outputPath = do
    Right report <-
        runTargetSynthesis
            def
                { tName = "main"
                , tPath = outputPath
                , tMicroArch = ma
                , tDFG = dataflow
                , tReceivedValues = received
                , tSimulationCycleN = n
                }
    return report

putCntx cntx = putStr $ cntx2table cntx

microarch ioSync = evalNetwork ioSync $ do
    addManual "fram1" (PU (framWithSize 16) FramPorts{oe = SignalTag 0, wr = SignalTag 1, addr = map SignalTag [2, 3, 4, 5]} FramIO def)
    addCustom "fram2" (framWithSize 32) FramIO
    add "shift" ShiftIO
    add "mul" MultiplierIO
    add "accum" AccumIO
    add "div" DividerIO
    add "spi" $
        SPISlave
            { slave_mosi = InputPortTag "mosi"
            , slave_miso = OutputPortTag "miso"
            , slave_sclk = InputPortTag "sclk"
            , slave_cs = InputPortTag "cs"
            }

warningIfUnexpectedPort expect port =
    when (expect /= Just port) $
        warningM "NITTA.UI" $
            concat
                [ "WARNING: expected backend port: "
                , show expect
                , " actual: "
                , show port
                , " (maybe you need regenerate API by nitta-api-gen)"
                ]
