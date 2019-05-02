{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes               #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-cse #-}

{-|
Module      : Main
Description : NITTA CAD executable
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module Main ( main ) where

import           Control.Monad                 (void, when)
import           Data.Default                  as D
import qualified Data.Map                      as M
import           Data.Maybe
import qualified Data.Text.IO                  as T
import           NITTA.API                     (backendServer,
                                                prepareStaticFiles)
import           NITTA.BusNetwork
import           NITTA.DataFlow
import           NITTA.Frontend
import qualified NITTA.Functions               as F
import qualified NITTA.ProcessUnits.Accum      as A
import qualified NITTA.ProcessUnits.Divider    as D
import qualified NITTA.ProcessUnits.Fram       as FR
import qualified NITTA.ProcessUnits.Multiplier as M
import qualified NITTA.ProcessUnits.Shift      as S
import qualified NITTA.ProcessUnits.SPI        as SPI
import           NITTA.Project
import           NITTA.SynthesisMethod
import           NITTA.Types
import           NITTA.Types.Synthesis
import           NITTA.Utils.Test              (Test (..),
                                                mkModelWithOneNetwork, runTest)
import           System.Console.CmdArgs
import           System.FilePath               (joinPath)
import           Text.InterpolatedString.Perl6 (qc)



-- |Command line interface.
data Nitta
    = Nitta
        { web        :: Bool
        , npm_build  :: Bool
        , no_api_gen :: Bool
        , type_      :: String
        , file       :: Maybe FilePath
        }
    deriving ( Show, Data, Typeable )


nittaArgs = Nitta
    { web=False &= help "Run web server"
    , npm_build=False &= help "No regenerate WebUI static files"
    , no_api_gen=False &= help "No regenerate rest_api.js library"
    , type_="fx32.32" &= help "Bus type, default value: \"fx32.32\""
    , file=D.def &= args &= typ "LUA_FILE"
    }


main = do
    Nitta{ web, npm_build, no_api_gen, file, type_ } <- cmdArgs nittaArgs
    when npm_build prepareStaticFiles
    case file of
        Just fn -> do
            putStrLn [qc|> readFile: { fn }|]
            src <- T.readFile fn
            let runner = if web
                then runWebUI no_api_gen $ lua2functions src
                else runTestbench $ lua2functions src
            case type_ of
                "fx24.32" -> runner (microarch :: BusNetwork String String (FX 24 32) Int)
                "fx32.32" -> runner (microarch :: BusNetwork String String (FX 32 32) Int)
                _ -> error "Wrong bus type"
        Nothing -> runHardcoded

runWebUI no_api_gen alg ma = backendServer no_api_gen $ mkModelWithOneNetwork ma alg
runTestbench alg microarchitecture
    = void $ runTest D.def
        { testProjectName="main"
        , microarchitecture
        , alg
        , verbose=True
        }


runHardcoded = do
    putStrLn "-- hardcoded begin --"
    -- funSim 5 D.def{ cntxInputs=M.fromList [("b_0", [1..5])] } $ lua2functions
    --     [qc|function fib(a)
    --             local b = receive()
    --             local c = a + b
    --             fib(c)
    --         end
    --         fib(1)|]
    putStrLn "--------------------------------"
    let microarchHC = busNetwork 31 (Just True)
            [ ("fram1", PU  D.def D.def FR.Ports{ FR.oe=SignalTag 11, FR.wr=SignalTag 10, FR.addr=map SignalTag [9, 8, 7, 6] } )
            , ("fram2", PU  D.def D.def FR.Ports{ FR.oe=SignalTag 5, FR.wr=SignalTag 4, FR.addr=map SignalTag [3, 2, 1, 0] } )
            , ("accum", PU  D.def D.def A.Ports{ A.init=SignalTag 18, A.load=SignalTag 19, A.neg=SignalTag 20, A.oe=SignalTag 21 } )
            , ("spi", PU  D.def
                (SPI.slaveSPI 0)
                SPI.Ports
                    { SPI.wr=SignalTag 22, SPI.oe=SignalTag 23
                    , SPI.stop="stop"
                    , SPI.externalPorts=SPI.Slave
                        { SPI.slave_mosi=InputPortTag "mosi"
                        , SPI.slave_miso=OutputPortTag "miso"
                        , SPI.slave_sclk=InputPortTag "sclk"
                        , SPI.slave_cs=InputPortTag "cs"
                        }
                    })
            , ("mul", PU  D.def (M.multiplier True) M.Ports{ M.wr=SignalTag 24, M.wrSel=SignalTag 25, M.oe=SignalTag 26 } )
            , ("div", PU  D.def (D.divider 4 True) D.Ports{ D.wr=SignalTag 27, D.wrSel=SignalTag 28, D.oe=SignalTag 29, D.oeSel=SignalTag 30 } )
            ] :: BusNetwork String String (FX 30 32) Int
    let algHC = lua2functions
            -- FIXME: Why not work with one fram?
            [qc|function fib(x)
                    y = x + x + x
                    fib(y)
                end
                fib(1)|]

    backendServer True $ mkModelWithOneNetwork microarchHC algHC
    void $ runTest D.def
        { testProjectName="hardcode"
        , microarchitecture=microarchHC
        , alg=algHC
        , verbose=True
        }
    putStrLn "-- hardcoded end --"


-- FIXME: В настоящее время при испытании на стенде сигнал rst не приводит к сбросу вычислителя в начальное состояние.

-- TODO: Необходимо иметь возможность указать, какая именно частота будет у целевого вычислителя. Данная задача связана
-- с задачей о целевой платформе.
microarch = busNetwork 31 (Just False)
    [ ("fram1", PU D.def D.def FR.Ports{ FR.oe=SignalTag 11, FR.wr=SignalTag 10, FR.addr=map SignalTag [9, 8, 7, 6] } )
    , ("fram2", PU D.def D.def FR.Ports{ FR.oe=SignalTag 5, FR.wr=SignalTag 4, FR.addr=map SignalTag [3, 2, 1, 0] } )
    -- , ("shift", PU D.def S.Ports{ S.work=SignalTag 12, S.direction=SignalTag 13, S.mode=SignalTag 14, S.step=SignalTag 15, S.init=SignalTag 16, S.oe=SignalTag 17 })
    , ("accum", PU D.def D.def A.Ports{ A.init=SignalTag 18, A.load=SignalTag 19, A.neg=SignalTag 20, A.oe=SignalTag 21 } )
    , ("spi", PU D.def
        (SPI.slaveSPI 0)
        SPI.Ports
            { SPI.wr=SignalTag 22, SPI.oe=SignalTag 23
            , SPI.stop="stop"
            , SPI.externalPorts=SPI.Slave
                { SPI.slave_mosi=InputPortTag "mosi"
                , SPI.slave_miso=OutputPortTag "miso"
                , SPI.slave_sclk=InputPortTag "sclk"
                , SPI.slave_cs=InputPortTag "cs"
                }
            })
    , ("mul", PU D.def (M.multiplier True) M.Ports{ M.wr=SignalTag 24, M.wrSel=SignalTag 25, M.oe=SignalTag 26 } )
    , ("div", PU D.def (D.divider 4 True) D.Ports{ D.wr=SignalTag 27, D.wrSel=SignalTag 28, D.oe=SignalTag 29, D.oeSel=SignalTag 30 } )
    ]
