{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-cse #-}

{-|
Module      : Main
Description : NITTA CAD executable
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}

module Main where

import           Control.Monad                 (when)
import           Data.Default                  as D
import qualified Data.Map                      as M
import           Data.Maybe
import           Demo
import           NITTA.API                     (backendServer)
import           NITTA.BusNetwork
import           NITTA.Compiler
import           NITTA.DataFlow
import qualified NITTA.Functions               as F
import qualified NITTA.ProcessUnits.Accum      as A
import qualified NITTA.ProcessUnits.Divisor    as D
import qualified NITTA.ProcessUnits.Fram       as FR
import qualified NITTA.ProcessUnits.Multiplier as M
import qualified NITTA.ProcessUnits.Shift      as S
import qualified NITTA.ProcessUnits.SPI        as SPI
import           NITTA.Project
import           NITTA.Types
import           NITTA.Types.Synthesis
import           System.Console.CmdArgs
import           System.FilePath               (joinPath)


microarch = busNetwork 31 (Just True)
    [ InputPort "mosi", InputPort "sclk", InputPort "cs" ]
    [ OutputPort "miso" ]
    [ ("fram1", PU D.def FR.PUPorts{ FR.oe=Signal 11, FR.wr=Signal 10, FR.addr=map Signal [9, 8, 7, 6] } )
    , ("fram2", PU D.def FR.PUPorts{ FR.oe=Signal 5, FR.wr=Signal 4, FR.addr=map Signal [3, 2, 1, 0] } )
    , ("shift", PU D.def S.PUPorts{ S.work=Signal 12, S.direction=Signal 13, S.mode=Signal 14, S.step=Signal 15, S.init=Signal 16, S.oe=Signal 17 })
    , ("accum", PU D.def A.PUPorts{ A.init=Signal 18, A.load=Signal 19, A.neg=Signal 20, A.oe=Signal 21 } )
    , ("spi", PU
        (SPI.slaveSPI 0)
        SPI.PUPorts
            { SPI.wr=Signal 22, SPI.oe=Signal 23
            , SPI.stop="stop"
            , SPI.mosi=InputPort "mosi", SPI.miso=OutputPort "miso", SPI.sclk=InputPort "sclk", SPI.cs=InputPort "cs"
            })
    , ("mul", PU (M.multiplier True) M.PUPorts{ M.wr=Signal 24, M.wrSel=Signal 25, M.oe=Signal 26 } )
    , ("div", PU (D.divisor 4 True) D.PUPorts{ D.wr=Signal 27, D.wrSel=Signal 28, D.oe=Signal 29, D.oeSel=Signal 30 } )
    ]


divAndMulAlg =
    [ F.constant 100 ["a"] :: F (Parcel String Int)
    , F.loop 2 "e" ["b"]
    , F.division "a" "b" ["c"] ["d"]
    -- , F.add "c" "d" ["e"]
    , F.add "c" "d" ["e", "e'"]

    , F.constant 200 ["a1"]
    , F.loop 2 "e1" ["b1"]
    , F.division "a1" "b1" ["c1"] ["d1"]
    , F.add "c1" "d1" ["e1"]

    , F.loop 1 "y" ["x"]
    , F.multiply "x" "e'" ["y"]
    ]


---------------------------------------------------------------------------------

-- |Command line interface.
data Nitta
    = Nitta
        { web        :: Bool
        , no_web_gen :: Bool
        }
    deriving (Show, Data, Typeable)
nittaArgs = Nitta
    { web=False &= help "Run web server"
    , no_web_gen=False &= help "No regenerate WebUI static files"
    }



main = do
    teacupDemo
    fibonacciDemo
    
    -- test "fibonacci" $ schedule $ mkModelWithOneNetwork microarch fibonacciAlg

    -- putStrLn "funSim teacup:"
    -- test "teacup" $ schedule $ mkModelWithOneNetwork microarch teacupAlg
    -- funSim 5 D.def teacupAlg

    -- putStrLn "funSim fibonacci:"
    -- funSim 5 D.def divAndMulAlg

    Nitta{ web, no_web_gen } <- cmdArgs nittaArgs
    when web $ backendServer (not no_web_gen) $ mkModelWithOneNetwork microarch teacupAlg
    putStrLn "-- the end --"


test n pu = do
    let prj = Project
            { projectName=n
            , libraryPath="../.."
            , projectPath=joinPath ["hdl", "gen", n]
            , model=pu
            , testCntx=Nothing
            }
    r <- writeAndRunTestBench prj
    if r then putStrLn "Success"
    else putStrLn "Fail"


-----------------------------------------------------------


funSim n cntx alg = putStrLn $ (!! (n - 1)) $ map (filter (/= '"') . show) $ F.simulateAlgByCycle cntx alg
