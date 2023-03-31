{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Control.Applicative
import Control.Exception
import Control.Monad (when)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Default (def)
import Data.Maybe
import Data.Proxy
import Data.String.Utils qualified as S
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Version
import GHC.TypeLits
import NITTA.Frontends
import NITTA.Intermediate.Simulation
import NITTA.Intermediate.Types
import NITTA.Model.Microarchitecture.Config
import NITTA.Model.Networks.Bus
import NITTA.Synthesis.Method
import NITTA.Model.Networks.Types
import NITTA.Model.ProcessorUnits
import NITTA.Project (TestbenchReport (..), defProjectTemplates, runTestbench)
import NITTA.Synthesis
import NITTA.UIBackend
import NITTA.Utils
import Paths_nitta
import System.Console.CmdArgs hiding (def)
import System.Console.CmdArgs.Explicit (HelpFormat (HelpFormatDefault), helpText)
import System.Exit
import System.FilePath.Posix
import System.IO (stdout)
import System.Log.Formatter
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple
import System.Log.Logger
import Text.Read
import Text.Regex

-- | Command line interface.
data Nitta = Nitta
    { filename :: FilePath
    , uarch :: Maybe FilePath
    , auto_uarch :: Bool
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
    , frontend_language :: Maybe FrontendType
    }
    deriving (Show, Data, Typeable)

deriving instance Data IOSynchronization

nittaArgs =
    Nitta
        { filename = def &= argPos 0 &= typFile
        , output_path =
            "gen"
                &= typ "PATH"
                &= help "Target system path"
                &= groupname "Common flags"
        , port =
            0
                &= help "Run nitta server for UI on specific port (by default - not run)"
                &= groupname "Common flags"
        , uarch =
            Nothing
                &= typ "PATH"
                &= help "Microarchitecture configuration file"
                &= explicit
                &= name "uarch"
                &= groupname "Target system configuration"
        , auto_uarch =
            False
                &= help "Use empty microarchitecture and allocate PUs during synthesis process."
                &= name "auto-uarch"
                &= groupname "Target system configuration"
        , type_ =
            Nothing
                &= name "t"
                &= typ "fxM.B"
                &= help "Overrides data type specified in config file"
                &= groupname "Target system configuration"
        , io_sync =
            Nothing
                &= help "Overrides IO synchronization mode specified in config file"
                &= explicit
                &= name "io-sync"
                &= typ "sync|async|onboard"
                &= groupname "Target system configuration"
        , templates =
            defTemplates
                &= typ "PATH[:PATH]"
                &= help ("Target platform templates (default: '" <> defTemplates <> "')")
                &= groupname "Target system configuration"
        , fsim =
            False
                &= name "f"
                &= help "Functional simulation with trace"
                &= groupname "Simulation"
        , lsim =
            False
                &= name "l"
                &= help "Logical (HDL) simulation with trace"
                &= groupname "Simulation"
        , format =
            "md"
                &= help "Simulation output format (default: 'md')"
                &= typ "md|json|csv"
                &= groupname "Simulation"
        , n =
            10
                &= help "Number of simulation cycles"
                &= groupname "Simulation"
        , verbose =
            False
                &= help "Verbose"
                &= groupname "Other"
        , extra_verbose =
            False
                &= help "Extra verbose"
                &= groupname "Other"
        , frontend_language =
            Nothing
                &= help "Language used to source algorithm description. (default: decision by file extension)"
                &= typ "Lua|XMILE"
                &= groupname "Target system configuration"
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
    ( Nitta
            filename
            uarch
            auto_uarch
            type_
            io_sync
            port
            templates
            n
            fsim
            lsim
            verbose
            extra_verbose
            output_path
            format
            frontend_language
        ) <-
        getNittaArgs
    setupLogger verbose extra_verbose

    toml <- case uarch of
        Nothing -> return Nothing
        Just path -> Just . getToml <$> T.readFile path

    let fromConf s = getFromTomlSection s =<< toml
    let exactFrontendType = identifyFrontendType filename frontend_language

    src <- readSourceCode filename
    ( \(SomeNat (_ :: Proxy m), SomeNat (_ :: Proxy b)) -> do
            let frontendResult@FrontendResult{frDataFlow, frTrace, frPrettyLog} =
                    translate exactFrontendType src
                -- FIXME: https://nitta.io/nitta-corp/nitta/-/issues/50
                -- data for sin_ident
                received = [("u#0", map (\i -> read $ show $ sin ((2 :: Double) * 3.14 * 50 * 0.001 * i)) [0 .. toEnum n])]
                ioSync = fromJust $ io_sync <|> fromConf "ioSync" <|> Just Sync
                confMa = toml >>= Just . mkMicroarchitecture ioSync
                ma :: BusNetwork T.Text T.Text (Attr (FX m b)) Int
                ma
                    | auto_uarch && isJust confMa =
                        error $
                            "auto_uarch flag means that an empty uarch with default prototypes will be used. "
                                <> "Remove uarch flag or specify prototypes list in config file and remove auto_uarch."
                    | auto_uarch = microarchWithProtos ioSync
                    | isJust confMa = fromJust confMa
                    | otherwise = defMicroarch ioSync

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

            when fsim $ functionalSimulation n received format frontendResult

            prj <-
                synthesizeTargetSystem
                    (def :: TargetSynthesis T.Text T.Text (Attr (FX m b)) Int)
                        { tName = "main"
                        , tPath = output_path
                        , tMicroArch = ma
                        , tDFG = frDataFlow
                        , tReceivedValues = received
                        , tTemplates = S.split ":" templates
                        , tSynthesisMethod = stateOfTheArtSynthesisIO ()
                        , tSimulationCycleN = n
                        , tSourceCodeType = exactFrontendType
                        }
                    >>= \case
                        Left msg -> error msg
                        Right p -> return p

            when lsim $ logicalSimulation format frPrettyLog prj
        )
        $ parseFX . fromJust
        $ type_ <|> fromConf "type" <|> Just "fx32.32"

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

-- | Simulation on intermediate level (data-flow graph)
functionalSimulation n received format FrontendResult{frDataFlow, frPrettyLog} = do
    let cntx = simulateDataFlowGraph n def received frDataFlow
    infoM "NITTA" "run functional simulation..."
    putLog format $ frPrettyLog $ map cycleCntx $ cntxProcess cntx
    infoM "NITTA" "run functional simulation...ok"

-- | Simulation on RTL level by a Verilog simulator.
logicalSimulation format prettyLog_ prj = do
    TestbenchReport{tbLogicalSimulationLog} <- runTestbench prj
    putLog format $ prettyLog_ tbLogicalSimulationLog

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

microarchWithProtos ioSync = defineNetwork "net1" ioSync $ do
    addCustomPrototype "fram{x}" (framWithSize 32) FramIO
    addPrototype "shift{x}" ShiftIO
    addPrototype "mul{x}" MultiplierIO
    addPrototype "accum{x}" AccumIO
    addPrototype "div{x}" DividerIO
    add "spi" $ -- FIXME: use addPrototype when https://github.com/ryukzak/nitta/issues/194 will be fixed
        SPISlave
            { slave_mosi = InputPortTag "mosi"
            , slave_miso = OutputPortTag "miso"
            , slave_sclk = InputPortTag "sclk"
            , slave_cs = InputPortTag "cs"
            }
