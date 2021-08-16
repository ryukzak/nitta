{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import Control.Applicative
import Control.Exception
import Control.Monad (when)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Default (def)
import Data.Functor
import Data.Maybe
import Data.Proxy
import qualified Data.String.Utils as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Version
import GHC.TypeLits
import NITTA.Intermediate.Simulation
import NITTA.Intermediate.Types
import NITTA.LuaFrontend
import NITTA.Model.Microarchitecture.Builder
import NITTA.Model.Microarchitecture.Config
import NITTA.Model.Networks.Bus
import NITTA.Model.Networks.Types
import NITTA.Model.ProcessorUnits
import NITTA.Project (TestbenchReport (..), defProjectTemplates, runTestbench)
import NITTA.Synthesis
import NITTA.UIBackend
import NITTA.Utils
import Paths_nitta
import System.Console.CmdArgs hiding (def)
import System.Console.CmdArgs.Explicit (helpText, HelpFormat(HelpFormatDefault))
import System.Exit
import System.FilePath.Posix
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
    , uarch :: Maybe FilePath
    , type_ :: Maybe String
    , io_sync :: Maybe IOSynchronization
    , port :: Int
    , templates :: String
    , n :: Int
    , fsim :: Bool
    , lsim :: Bool
    , verbose :: Bool
    , extra_verbose :: Bool
    , output_path :: FilePath
    , format :: String
    }
    deriving (Show, Data, Typeable)

deriving instance Data IOSynchronization

nittaArgs =
    Nitta
        { filename = def &= argPos 0 &= typFile
        , output_path =
            "gen" &= typ "PATH" &= help "Target system path"
                &= groupname "Common flags"
        , port =
            0 &= help "Run nitta server for UI on specific port (by default - not run)"
                &= groupname "Common flags"
        , uarch =
            Nothing &= typ "PATH" &= help "Microarchitecture configuration file"
                &= explicit
                &= name "uarch"
                &= groupname "Target system configuration"
        , type_ =
            Nothing &= name "t" &= typ "fxM.B" &= help "Overrides data type specified in config file"
                &= groupname "Target system configuration"
        , io_sync =
            Nothing &= help "Overrides IO synchronization mode specified in config file"
                &= explicit
                &= name "io-sync"
                &= typ "sync|async|onboard"
                &= groupname "Target system configuration"
        , templates =
            defTemplates &= typ "PATH[:PATH]" &= help ("Target platform templates (default: '" <> defTemplates <> "')")
                &= groupname "Target system configuration"
        , fsim =
            False &= name "f" &= help "Functional simulation with trace"
                &= groupname "Simulation"
        , lsim =
            False &= name "l" &= help "Logical (HDL) simulation with trace"
                &= groupname "Simulation"
        , format =
            "md" &= help "Simulation output format (default: 'md')"
                &= typ "md|json|csv"
                &= groupname "Simulation"
        , n =
            10 &= help "Number of simulation cycles"
                &= groupname "Simulation"
        , verbose =
            False &= help "Verbose"
                &= groupname "Other"
        , extra_verbose =
            False &= help "Extra verbose"
                &= groupname "Other"
        }
        &= summary ("nitta v" ++ showVersion version ++ " - tool for hard real-time CGRA processors")
        &= helpArg [groupname "Other"]
        &= versionArg [groupname "Other"]
    where
        defTemplates = S.join ":" defProjectTemplates

getNittaArgs :: IO Nitta
getNittaArgs = do
    let handleError :: ExitCode -> IO Nitta
        handleError exitCode = do
            print $ helpText [] HelpFormatDefault $ cmdArgsMode nittaArgs
            exitWith exitCode
    catch (cmdArgs nittaArgs) handleError

main = do
    Nitta{port, filename, uarch, type_, io_sync, fsim, lsim, n, verbose, extra_verbose, output_path, templates, format} <-
        getNittaArgs
    setupLogger verbose extra_verbose

    toml <- case uarch of
        Nothing -> return Nothing
        Just path -> T.readFile path <&> (Just . getToml)

    let fromConf s = getFromTomlSection s =<< toml

    src <- readSourceCode filename
    ( \(SomeNat (_ :: Proxy m), SomeNat (_ :: Proxy b)) -> do
            let FrontendResult{frDataFlow, frTrace, frPrettyLog} = lua2functions src
                -- FIXME: https://nitta.io/nitta-corp/nitta/-/issues/50
                -- data for sin_ident
                received = [("u#0", map (\i -> read $ show $ sin ((2 :: Double) * 3.14 * 50 * 0.001 * i)) [0 .. toEnum n])]
                ioSync = fromJust $ io_sync <|> fromConf "ioSync" <|> Just Sync
                confMa = toml >>= Just . mkMicroarchitecture ioSync
                ma = fromJust $ confMa <|> Just (defMicroarch ioSync) :: BusNetwork T.Text T.Text (Attr (FX m b)) Int

            infoM "NITTA" $ "will trace: " <> S.join ", " (map (show . tvVar) frTrace)

            when (port > 0) $ do
                bufE <- try $ readFile (apiPath </> "PORT")
                let expect = case bufE of
                        Right buf -> case readEither buf of
                            Right p -> p
                            Left e -> error $ "can't get nitta-api info: " <> show e <> "; you should use nitta-api-gen to fix it"
                        Left (e :: IOError) -> error $ "can't get nitta-api info: " <> show e
                warningIfUnexpectedPort expect port
                backendServer port received output_path $ mkModelWithOneNetwork ma frDataFlow
                exitSuccess

            when fsim $ functionalSimulation n received src format

            prj <-
                synthesizeTargetSystem
                    def
                        { tName = "main"
                        , tPath = output_path
                        , tMicroArch = ma
                        , tDFG = frDataFlow
                        , tReceivedValues = received
                        , tTemplates = S.split ":" templates
                        , tSimulationCycleN = n
                        }
                    >>= \case
                        Left msg -> error msg
                        Right p -> return p

            when lsim $ logicalSimulation format frPrettyLog prj
        )
        $ parseFX . fromJust $ type_ <|> fromConf "type" <|> Just "fx32.32"

parseFX input =
    let typePattern = mkRegex "fx([0-9]+).([0-9]+)"
        [m, b] = fromMaybe (error "incorrect Bus type input") $ matchRegex typePattern input
        convert = fromJust . someNatVal . read
     in (convert m, convert b)

setupLogger verbose extra = do
    let level = case (verbose, extra) of
            (_, True) -> DEBUG
            (True, _) -> NOTICE
            _ -> WARNING
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

-- |Simulation on intermediate level (data-flow graph)
functionalSimulation n received src format = do
    let FrontendResult{frDataFlow, frPrettyLog} = lua2functions src
        cntx = simulateDataFlowGraph n def received frDataFlow
    infoM "NITTA" "run functional simulation..."
    putLog format $ frPrettyLog $ map cycleCntx $ cntxProcess cntx
    infoM "NITTA" "run functional simulation...ok"

-- |Simulation on RTL level by a Verilog simulator.
logicalSimulation format prettyLog prj = do
    TestbenchReport{tbLogicalSimulationLog} <- runTestbench prj
    putLog format $ prettyLog tbLogicalSimulationLog

putLog "md" records = putStr $ log2md records
putLog "json" records = BS.putStrLn $ log2json records
putLog "csv" records = BS.putStr $ log2csv records
putLog t _ = error $ "not supported output format option: " <> t

warningIfUnexpectedPort expect port =
    when (expect /= port) $
        warningM "NITTA.UI" $
            concat
                [ "WARNING: expected backend port: "
                , show expect
                , " actual: "
                , show port
                , " (maybe you need regenerate API by nitta-api-gen)"
                ]

defMicroarch ioSync = defineNetwork "net1" ioSync $ do
    addCustom "fram1" (framWithSize 16) FramIO
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
