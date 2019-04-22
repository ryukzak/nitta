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

import           Control.Monad                 (when)
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
import           NITTA.Utils.Test
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
    putStrLn "-- the end --"

runWebUI no_api_gen alg ma = backendServer no_api_gen $ mkModelWithOneNetwork ma alg
runTestbench alg ma = test "main" ma alg >>= print


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
            [ ("fram1", PU  D.def D.def FR.PUPorts{ FR.oe=Signal 11, FR.wr=Signal 10, FR.addr=map Signal [9, 8, 7, 6] } )
            , ("fram2", PU  D.def D.def FR.PUPorts{ FR.oe=Signal 5, FR.wr=Signal 4, FR.addr=map Signal [3, 2, 1, 0] } )
            , ("accum", PU  D.def D.def A.PUPorts{ A.init=Signal 18, A.load=Signal 19, A.neg=Signal 20, A.oe=Signal 21 } )
            , ("spi", PU  D.def
                (SPI.slaveSPI 0)
                SPI.PUPorts
                    { SPI.wr=Signal 22, SPI.oe=Signal 23
                    , SPI.stop="stop"
                    , SPI.externalPorts=SPI.Slave
                        { SPI.slave_mosi=InputPort "mosi"
                        , SPI.slave_miso=OutputPort "miso"
                        , SPI.slave_sclk=InputPort "sclk"
                        , SPI.slave_cs=InputPort "cs"
                        }
                    })
            , ("mul", PU  D.def (M.multiplier True) M.PUPorts{ M.wr=Signal 24, M.wrSel=Signal 25, M.oe=Signal 26 } )
            , ("div", PU  D.def (D.divider 4 True) D.PUPorts{ D.wr=Signal 27, D.wrSel=Signal 28, D.oe=Signal 29, D.oeSel=Signal 30 } )
            ] :: BusNetwork String String (FX 30 32) Int
    let algHC = lua2functions
            -- FIXME: Why not work with one fram?
            [qc|function fib(x)
                    y = x + x + x
                    fib(y)
                end
                fib(1)|]

    backendServer True $ mkModelWithOneNetwork microarchHC algHC
    print =<< testWithInput "hardcode" [  ] microarchHC algHC
    putStrLn "-- hardcoded end --"


-- FIXME: В настоящее время при испытании на стенде сигнал rst не приводит к сбросу вычислителя в начальное состояние.

-- TODO: Необходимо иметь возможность указать, какая именно частота будет у целевого вычислителя. Данная задача связана
-- с задачей о целевой платформе.
microarch = busNetwork 31 (Just False)
    [ ("fram1", PU D.def D.def FR.PUPorts{ FR.oe=Signal 11, FR.wr=Signal 10, FR.addr=map Signal [9, 8, 7, 6] } )
    , ("fram2", PU D.def D.def FR.PUPorts{ FR.oe=Signal 5, FR.wr=Signal 4, FR.addr=map Signal [3, 2, 1, 0] } )
    -- , ("shift", PU D.def S.PUPorts{ S.work=Signal 12, S.direction=Signal 13, S.mode=Signal 14, S.step=Signal 15, S.init=Signal 16, S.oe=Signal 17 })
    , ("accum", PU D.def D.def A.PUPorts{ A.init=Signal 18, A.load=Signal 19, A.neg=Signal 20, A.oe=Signal 21 } )
    , ("spi", PU D.def
        (SPI.slaveSPI 0)
        SPI.PUPorts
            { SPI.wr=Signal 22, SPI.oe=Signal 23
            , SPI.stop="stop"
            , SPI.externalPorts=SPI.Slave
                { SPI.slave_mosi=InputPort "mosi"
                , SPI.slave_miso=OutputPort "miso"
                , SPI.slave_sclk=InputPort "sclk"
                , SPI.slave_cs=InputPort "cs"
                }
            })
    , ("mul", PU D.def (M.multiplier True) M.PUPorts{ M.wr=Signal 24, M.wrSel=Signal 25, M.oe=Signal 26 } )
    , ("div", PU D.def (D.divider 4 True) D.PUPorts{ D.wr=Signal 27, D.wrSel=Signal 28, D.oe=Signal 29, D.oeSel=Signal 30 } )
    ]
