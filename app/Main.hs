{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-# OPTIONS -fno-warn-overlapping-patterns #-} -- for master/slave selection
{-# OPTIONS_GHC -fno-cse #-}

{-|
Module      : Main
Description : NITTA CAD executable
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module Main ( main ) where

import           Control.Monad                    (void, when)
import           Data.Default                     (def)
import qualified Data.Text.IO                     as T
import           NITTA.Frontend
import           NITTA.Intermediate.Types
import           NITTA.Model.Networks.Bus
import           NITTA.Model.Networks.Types
import           NITTA.Model.ProcessorUnits
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Project                    (TargetSynthesis (..),
                                                   mkModelWithOneNetwork,
                                                   runTargetSynthesis)
import           NITTA.UIBackend
import           System.Console.CmdArgs           hiding (def)
import           System.Exit
import           Text.InterpolatedString.Perl6    (qc)


-- |Command line interface.
data Nitta
    = Nitta
        { web          :: Bool
        , port         :: Int
        , npm_build    :: Bool
        , generate_api :: Bool
        , type_        :: String
        , file         :: Maybe FilePath
        }
    deriving ( Show, Data, Typeable )

nittaArgs = Nitta
    { web=False &= help "Run web server"
    , port=8080 &= help "WebUI port"
    , npm_build=False &= help "No regenerate WebUI static files"
    , generate_api=False &= help "generate rest_api.js library and exit"
    , type_="fx32.32" &= help "Bus type, default value: \"fx32.32\""
    , file=def &= args &= typ "LUA_FILE"
    }

main = do
    Nitta{ web, port, npm_build, generate_api, file, type_ } <- cmdArgs nittaArgs
    when generate_api $ do
        prepareJSAPI port
        exitSuccess
    when npm_build prepareStaticFiles
    case file of
        Just fn -> do
            putStrLn [qc|> readFile: { fn }|]
            src <- T.readFile fn
            let runner = if web
                then runWebUI port $ lua2functions src
                else runTestbench $ lua2functions src
            case type_ of
                "fx24.32" -> runner (microarch :: BusNetwork String String (FX 24 32) Int)
                "fx32.32" -> runner (microarch :: BusNetwork String String (FX 32 32) Int)
                _ -> error "Wrong bus type"
        Nothing -> runHardcoded

runWebUI port alg ma = backendServer port $ mkModelWithOneNetwork ma alg
runTestbench tDFG tMicroArch
    = void $ runTargetSynthesis (def :: TargetSynthesis _ _ _ Int)
        { tName="main"
        , tMicroArch
        , tDFG
        , tVerbose=True
        , tReceivedValues=
            [ ( "a:0", [1, 2, 3, 4, 5, 6] )
            , ( "b:0", [1, 1, 1, 1, 1, 1] )
            ]
        }


runHardcoded = do
    putStrLn "-- hardcoded begin --"
    -- funSim 5 def{ cntxInputs=fromList [("b_0", [1..5])] } $ lua2functions
    --     [qc|function fib(a)
    --             local b = receive()
    --             local c = a + b
    --             fib(c)
    --         end
    --         fib(1)|]
    putStrLn "--------------------------------"
    let microarchHC = busNetwork 31 (Just True)
            [ ("fram1", PU def def FramPorts{ oe=SignalTag 11, wr=SignalTag 10, addr=map SignalTag [9, 8, 7, 6] } )
            , ("fram2", PU def def FramPorts{ oe=SignalTag 5, wr=SignalTag 4, addr=map SignalTag [3, 2, 1, 0] } )
            , ("accum", PU def def AccumPorts{ init=SignalTag 18, load=SignalTag 19, neg=SignalTag 20, oe=SignalTag 21 } )
            , ("spi", PU def
                (anySPI 0)
                SPIPorts
                    { wr=SignalTag 22, oe=SignalTag 23
                    , stop="stop"
                    , externalPorts=Slave
                        { slave_mosi=InputPortTag "mosi"
                        , slave_miso=OutputPortTag "miso"
                        , slave_sclk=InputPortTag "sclk"
                        , slave_cs=InputPortTag "cs"
                        }
                    })
            , ("mul", PU def (multiplier True) MultiplierPorts{ wr=SignalTag 24, wrSel=SignalTag 25, oe=SignalTag 26 } )
            , ("div", PU def (divider 4 True) DividerPorts{ wr=SignalTag 27, wrSel=SignalTag 28, oe=SignalTag 29, oeSel=SignalTag 30 } )
            ] :: BusNetwork String String (FX 30 32) Int
    let algHC = lua2functions
            -- FIXME: Why not work with one fram?
            [qc|function fib(x)
                    y = x + x + x
                    fib(y)
                end
                fib(1)|]

    backendServer 8080 $ mkModelWithOneNetwork microarchHC algHC
    void $ runTargetSynthesis (def :: TargetSynthesis _ _ _ Int)
        { tName="hardcode"
        , tMicroArch=microarchHC
        , tDFG=algHC
        , tVerbose=True
        }
    putStrLn "-- hardcoded end --"


-- FIXME: В настоящее время при испытании на стенде сигнал rst не приводит к сбросу вычислителя в начальное состояние.

-- TODO: Необходимо иметь возможность указать, какая именно частота будет у целевого вычислителя. Данная задача связана
-- с задачей о целевой платформе.
microarch = busNetwork 31 (Just False)
    [ ("fram1", PU def def FramPorts{ oe=SignalTag 11, wr=SignalTag 10, addr=map SignalTag [9, 8, 7, 6] } )
    , ("fram2", PU def def FramPorts{ oe=SignalTag 5, wr=SignalTag 4, addr=map SignalTag [3, 2, 1, 0] } )
    -- , ("shift", PU def S.Ports{ S.work=SignalTag 12, S.direction=SignalTag 13, S.mode=SignalTag 14, S.step=SignalTag 15, S.init=SignalTag 16, S.oe=SignalTag 17 })
    , ("accum", PU def def AccumPorts{ init=SignalTag 18, load=SignalTag 19, neg=SignalTag 20, oe=SignalTag 21 } )
    , ("spi", PU def
        (anySPI 0)
        SPIPorts
            { wr=SignalTag 22, oe=SignalTag 23
            , stop="stop"
            , externalPorts=case "slave" of
                "slave" -> Slave
                    { slave_mosi=InputPortTag "mosi"
                    , slave_miso=OutputPortTag "miso"
                    , slave_sclk=InputPortTag "sclk"
                    , slave_cs=InputPortTag "cs"
                    }
                "master" -> Master
                    { master_mosi=OutputPortTag "mosi"
                    , master_miso=InputPortTag "miso"
                    , master_sclk=OutputPortTag "sclk"
                    , master_cs=OutputPortTag "cs"
                    }
            })
    , ("mul", PU def (multiplier True) MultiplierPorts{ wr=SignalTag 24, wrSel=SignalTag 25, oe=SignalTag 26 } )
    , ("div", PU def (divider 4 True) DividerPorts{ wr=SignalTag 27, wrSel=SignalTag 28, oe=SignalTag 29, oeSel=SignalTag 30 } )
    ]
