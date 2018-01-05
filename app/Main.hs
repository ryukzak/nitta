{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main where

import           Control.Monad
import           Data.Array               (array, elems)
import qualified Data.Array               as A
import           Data.Default
import           Data.Map                 (Map, assocs, fromList, (!))
import qualified Data.Map                 as M
import           Data.Maybe
import           Data.Proxy
import           Data.String.Utils
import qualified Data.String.Utils        as S
import           Data.Typeable
import           NITTA.BusNetwork
import           NITTA.Compiler
import           NITTA.Flows
import           NITTA.FunctionBlocks
import qualified NITTA.FunctionBlocks     as FB
import qualified NITTA.ProcessUnits.Accum as A
import qualified NITTA.ProcessUnits.SPI   as S
import           NITTA.ProcessUnits.Fram
import           NITTA.TestBench
import           NITTA.Timeline
import           NITTA.Types
import           NITTA.Utils
import           System.FilePath          (joinPath)
import           Text.StringTemplate

type T = TaggedTime String Int



-- | Пример работы с ветвящимся временем.
--
-- - TODO: планирование вычислительного процесса (пропуск переменных, колизии потоков управления и
--   данных, привязок функциональных блоков).
-- - TODO: генерация машинного кода.
-- - TODO: генерация аппаратуры.
-- - TODO: testbench.
bushExample
  = let fram = PU (def :: Fram String T)
        accum = PU (def :: A.Accum String T)
        net = busNetwork
          [ ("fram1", fram)
          , ("fram2", fram)
          , ("accum", accum)
          ]
          $ array (0, 19) [ (19, [("accum", S (A.NEG  :: Signal (A.Accum String T)))])
                          , (18, [("accum", S (A.LOAD :: Signal (A.Accum String T)))])
                          , (17, [("accum", S (A.INIT :: Signal (A.Accum String T)))])
                          , (16, [("accum", S (A.OE   :: Signal (A.Accum String T)))])

                          , (15, [("fram1", S (OE :: Signal (Fram String T)))])
                          , (14, [("fram1", S (WR :: Signal (Fram String T)))])
                          , (13, [])
                          , (12, [])

                          , (11, [("fram1", S (ADDR 3 :: Signal (Fram String T)))])
                          , (10, [("fram1", S (ADDR 2 :: Signal (Fram String T)))])
                          , ( 9, [("fram1", S (ADDR 1 :: Signal (Fram String T)))])
                          , ( 8, [("fram1", S (ADDR 0 :: Signal (Fram String T)))])

                          , ( 7, [("fram2", S (OE :: Signal (Fram String T)))])
                          , ( 6, [("fram2", S (WR :: Signal (Fram String T)))])
                          , ( 5, [])
                          , ( 4, [])

                          , ( 3, [("fram2", S (ADDR 3 :: Signal (Fram String T)))])
                          , ( 2, [("fram2", S (ADDR 2 :: Signal (Fram String T)))])
                          , ( 1, [("fram2", S (ADDR 1 :: Signal (Fram String T)))])
                          , ( 0, [("fram2", S (ADDR 0 :: Signal (Fram String T)))])
                          ]
        dataFlow = Stage
          [ Actor $ FB.framInput 0 $ O [ "cond", "cond'" ]
          , Actor $ FB.framInput 1 $ O [ "x1", "x2" ]
          , Actor $ FB.framOutput 2 $ I "cond'"
          , Paths "cond"
            [ (0, Stage [ Actor $ FB.reg (I "x1") $ O ["y1"], Actor $ FB.framOutput 10 $ I "y1" ])
            , (1, Stage [ Actor $ FB.reg (I "x2") $ O ["y2"], Actor $ FB.framOutput 11 $ I "y2" ])
            ]
          ]
        net' = bindAll (functionalBlocks dataFlow) (net :: BusNetwork String String T)
        initialBranch = Branch net' (dataFlow2controlFlow dataFlow) Nothing []
        Branch{ topPU=pu } = foldl (\b _ -> naive def b) initialBranch $ replicate 50 ()
    in pu


-- | Пример работы с единым временем.
branchExample
  = let fram = PU (def :: Fram String T)
        accum = PU (def :: A.Accum String T)
        net = busNetwork
          [ ("fram1", fram)
          , ("fram2", fram)
          , ("accum", accum)
          ]
          $ array (0, 19) [ (19, [("accum", S (A.NEG  :: Signal (A.Accum String T)))])
                          , (18, [("accum", S (A.LOAD :: Signal (A.Accum String T)))])
                          , (17, [("accum", S (A.INIT :: Signal (A.Accum String T)))])
                          , (16, [("accum", S (A.OE   :: Signal (A.Accum String T)))])

                          , (15, [("fram1", S (OE :: Signal (Fram String T)))])
                          , (14, [("fram1", S (WR :: Signal (Fram String T)))])
                          , (13, [])
                          , (12, [])

                          , (11, [("fram1", S (ADDR 3 :: Signal (Fram String T)))])
                          , (10, [("fram1", S (ADDR 2 :: Signal (Fram String T)))])
                          , ( 9, [("fram1", S (ADDR 1 :: Signal (Fram String T)))])
                          , ( 8, [("fram1", S (ADDR 0 :: Signal (Fram String T)))])

                          , ( 7, [("fram2", S (OE :: Signal (Fram String T)))])
                          , ( 6, [("fram2", S (WR :: Signal (Fram String T)))])
                          , ( 5, [])
                          , ( 4, [])

                          , ( 3, [("fram2", S (ADDR 3 :: Signal (Fram String T)))])
                          , ( 2, [("fram2", S (ADDR 2 :: Signal (Fram String T)))])
                          , ( 1, [("fram2", S (ADDR 1 :: Signal (Fram String T)))])
                          , ( 0, [("fram2", S (ADDR 0 :: Signal (Fram String T)))])
                          ]
        alg = [ FB.framInput 3 $ O [ "a"
                                   , "d"
                                   ]
              , FB.framInput 4 $ O [ "b"
                                   , "c"
                                   , "e"
                                   ]
              , FB.reg (I "a") $ O ["x"]
              , FB.reg (I "b") $ O ["y"]
              , FB.reg (I "c") $ O ["z"]
              , FB.framOutput 5 $ I "x"
              , FB.framOutput 6 $ I "y"
              , FB.framOutput 7 $ I "z"
              , FB.framOutput 0 $ I "sum"
              , FB $ FB.Constant 42 $ O ["const"]
              , FB.framOutput 9 $ I "const"
              , FB.loop (O ["f"]) $ I "g"
              , FB.reg (I "f") $ O ["g"]
              , FB $ FB.Add (I "d") (I "e") (O ["sum"])
              ]
        dataFlow = Stage $ map Actor alg
        net' = bindAll (functionalBlocks dataFlow) (net :: BusNetwork String String T)
        initialBranch = Branch net' (dataFlow2controlFlow dataFlow) Nothing []
        Branch{ topPU=pu } = foldl (\b _ -> naive def b) initialBranch $ replicate 50 ()
    in pu


---------------------------------------------------------------------------------


main = do
  timeline "resource/data.json" branchExample
  r <- testBench ".." (joinPath ["hdl", "gen"]) branchExample ([] :: [(String, Int)])
  if r then putStrLn "Success"
  else putStrLn "Fail"
  print "ok"

getPU puTitle net
  = case bnPus net ! puTitle of
      PU pu | Just pu' <- cast pu -> pu'
