{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : NITTA.Model.ProcessorUnits.Accum
Description : Accumulator processor unit implementation
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Accum (
    Accum,
    Ports (..),
    IOPorts (..),
) where

import Control.Monad (when)
import Data.Default
import Data.List (find, partition, (\\))
import Data.Maybe (fromMaybe)
import Data.Set (elems, fromList, member)
import Data.String.Interpolate
import Data.String.ToString
import qualified Data.Text as T
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits.Types
import NITTA.Model.Types
import NITTA.Project
import NITTA.Utils
import NITTA.Utils.ProcessDescription
import Numeric.Interval.NonEmpty (inf, singleton, sup, (...))

{- |Type that contains expression:

@a + b = c@ is exression and it equals:
    @[[(False, "a"), (False, "b")], [(False, "c")]]@

@a + b = c; d - e = f@ is one expression too and it equals:
    @[[(False, "a"), (False, "b")], [(False, "c")], [(False, "d"), (True, "d")], [(False, "f")]]@
-}
data Job v x = Job
    { -- |Contains future parts expression to eval (c + d = e)
      tasks :: [[(Bool, v)]]
    , -- |Contain current parts expression (a + b = c)
      current :: [[(Bool, v)]]
    , -- |Func of this expression
      func :: F v x
    , -- |Flag indicates when evaluation ended
      calcEnd :: Bool
    }
    deriving (Eq, Show)

data Accum v x t = Accum
    { -- |List of jobs (expressions)
      work :: [Job v x]
    , -- |Current job
      currentWork :: Maybe (Job v x)
    , -- |Process
      process_ :: Process t (StepInfo v x t)
    , -- |Flag is indicated when new job starts
      isInit :: Bool
    }

instance (VarValTime v x t) => Show (Accum v x t) where
    show a =
        [__i|
            Accum:
                work                 = #{ work a }
                currentWork          = #{ currentWork a }
                process_             = #{ process_ a }
                isInit               = #{ isInit a }
        |]

instance (VarValTime v x t) => Default (Accum v x t) where
    def =
        Accum
            { work = []
            , currentWork = Nothing
            , process_ = def
            , isInit = True
            }

instance Default x => DefaultX (Accum v x t) x

tryBindJob f@Acc{actions} =
    Job
        { tasks = concat $ actionGroups actions
        , current = []
        , func = packF f
        , calcEnd = False
        }

actionGroups [] = []
actionGroups as =
    let (pushs, as') = span isPush as
        (pulls, as'') = span isPull as'
     in [ map (\(Push sign (I v)) -> (sign == Minus, v)) pushs
        , concatMap (\(Pull (O vs)) -> map (True,) $ elems vs) pulls
        ] :
        actionGroups as''

endpointOptionsJob Job{tasks = []} = []
endpointOptionsJob Job{tasks = (t : _), current = []} = map snd t
endpointOptionsJob Job{tasks = (t : ts), current = (c : _)}
    | null (t \\ c) && null ts = []
    | null $ t \\ c = map snd $ head ts
    | otherwise = map snd $ t \\ c

endpointDecisionJob j@Job{tasks = []} _ = j
endpointDecisionJob j@Job{tasks = tasks@(t : _), current = []} v = j{tasks = updateTasks current' tasks, current = current'}
    where
        ([(neg, _)], _) = partition ((== v) . snd) t
        current' = [[(neg, v)]]
endpointDecisionJob j@Job{tasks = tasks@(t : ts), current = (c : cs)} v
    | null $ t \\ c = endpointDecisionJob j{tasks = ts} v
    | t \\ c /= t && length t > length c = j{tasks = updateTasks currentInsert tasks, current = currentInsert}
    | otherwise = j{tasks = updateTasks currentAdd tasks, current = currentAdd}
    where
        ([val], _) = partition ((== v) . snd) t
        currentInsert = (val : c) : cs
        currentAdd = [val] : c : cs

updateTasks (c : _) tasks@(t : ts)
    | null $ t \\ c = ts
    | otherwise = tasks
updateTasks [] _ = error "Current is null"
updateTasks _ _ = error "Matching error updateTasks"

toTarget = even . length
toSource = odd . length

instance (VarValTime v x t, Num x) => ProcessorUnit (Accum v x t) v x t where
    tryBind f pu@Accum{work}
        | Just (Add a b c) <- castF f = Right pu{work = tryBindJob (Acc [Push Plus a, Push Plus b, Pull c]) : work}
        | Just (Sub a b c) <- castF f = Right pu{work = tryBindJob (Acc [Push Plus a, Push Minus b, Pull c]) : work}
        | Just f'@Acc{} <- castF f = Right pu{work = tryBindJob f' : work}
        | otherwise = Left $ "The function is unsupported by Accum: " ++ show f

    process = process_

instance (VarValTime v x t, Num x) => EndpointProblem (Accum v x t) v t where
    endpointOptions Accum{currentWork = Just a@Job{tasks, calcEnd}, process_ = Process{nextTick = tick}}
        | toTarget tasks = targets
        | toSource tasks = sources
        where
            targets = map (\v -> EndpointSt (Target v) $ TimeConstrain (tick + 1 ... maxBound) (singleton 1)) (endpointOptionsJob a)
            sources = [EndpointSt (Source $ fromList (endpointOptionsJob a)) $ TimeConstrain (max tick (tickSource calcEnd) ... maxBound) (1 ... maxBound)]
            tickSource True = tick + 1
            tickSource _ = tick + 3
    endpointOptions p@Accum{work, currentWork = Nothing} =
        concatMap (\a -> endpointOptions p{currentWork = Just a}) work
    endpointOptions _ = error "Error in matching in endpointOptions function"

    endpointDecision
        pu@Accum{currentWork = Just j@Job{tasks}, isInit}
        d@EndpointSt{epRole = Target v, epAt}
            | not (null tasks) && toTarget tasks =
                let job@Job{tasks = tasks', current = (((neg, _) : _) : _)} = endpointDecisionJob j v
                    sel = if isInit then ResetAndLoad neg else Load neg
                    (_, process_') = runSchedule pu $ do
                        updateTick (sup epAt)
                        scheduleEndpoint d $ scheduleInstruction epAt sel
                 in pu
                        { process_ = process_'
                        , currentWork = Just job{calcEnd = False}
                        , isInit = null tasks'
                        }
    endpointDecision
        pu@Accum{currentWork = Just j@Job{tasks, current, func}, process_}
        d@EndpointSt{epRole = Source v, epAt}
            | not (null current) && toSource tasks =
                let job@Job{tasks = tasks'} = foldl endpointDecisionJob j (elems v)
                    a = inf $ stepsInterval $ relatedEndpoints process_ $ variables func
                    (_, process_') = runSchedule pu $ do
                        endpoints <- scheduleEndpoint d $ scheduleInstruction (epAt -1) Out
                        when (null tasks') $ do
                            high <- scheduleFunction (a ... sup epAt) func
                            let low = endpoints ++ map pID (relatedEndpoints process_ $ variables func)
                            establishVerticalRelations high low

                        updateTick (sup epAt)
                        return endpoints
                 in pu
                        { process_ = process_'
                        , currentWork = if null tasks' then Nothing else Just job{calcEnd = True}
                        , isInit = null tasks'
                        }
    endpointDecision pu@Accum{work, currentWork = Nothing} d
        | Just job <- getJob work =
            endpointDecision pu{work = work \\ [job], currentWork = Just job{calcEnd = False}, isInit = True} d
        where
            getJob = find (\Job{func} -> d `isIn` func)
            e `isIn` f = oneOf (variables e) `member` variables f
    endpointDecision pu d = error $ "error in Endpoint Decision function" ++ show pu ++ show d

instance Connected (Accum v x t) where
    data Ports (Accum v x t) = AccumPorts {resetAcc, load, neg, oe :: SignalTag}
        deriving (Show)

instance IOConnected (Accum v x t) where
    data IOPorts (Accum v x t) = AccumIO deriving (Show)

instance Controllable (Accum v x t) where
    data Instruction (Accum v x t) = ResetAndLoad Bool | Load Bool | Out deriving (Show)

    data Microcode (Accum v x t) = Microcode
        { oeSignal :: Bool
        , resetAccSignal :: Bool
        , loadSignal :: Bool
        , negSignal :: Maybe Bool
        }
        deriving (Show, Eq, Ord)

    zipSignalTagsAndValues AccumPorts{..} Microcode{..} =
        [ (resetAcc, Bool resetAccSignal)
        , (load, Bool loadSignal)
        , (oe, Bool oeSignal)
        , (neg, maybe Undef Bool negSignal)
        ]

    usedPortTags AccumPorts{resetAcc, load, neg, oe} = [resetAcc, load, neg, oe]

    takePortTags (resetAcc : load : neg : oe : _) _ = AccumPorts resetAcc load neg oe
    takePortTags _ _ = error "can not take port tags, tags are over"

instance Default (Microcode (Accum v x t)) where
    def =
        Microcode
            { oeSignal = False
            , resetAccSignal = False
            , loadSignal = False
            , negSignal = Nothing
            }

instance UnambiguouslyDecode (Accum v x t) where
    decodeInstruction (ResetAndLoad neg) = def{resetAccSignal = True, loadSignal = True, negSignal = Just neg}
    decodeInstruction (Load neg) = def{resetAccSignal = False, loadSignal = True, negSignal = Just neg}
    decodeInstruction Out = def{oeSignal = True}

instance (Var v) => Locks (Accum v x t) v where
    locks Accum{currentWork = Nothing} = []
    locks Accum{currentWork = Just job, work} = locks' job work
        where
            locks' Job{tasks = []} _ = []
            locks' Job{current = []} _ = []
            locks' Job{tasks = (m : ms), current = (r : _)} other =
                [ Lock{lockBy, locked}
                | locked <- concatMap (map snd) ms
                , lockBy <- map snd (m \\ r)
                ]
                    ++ [ Lock{lockBy, locked}
                       | locked <- concatMap (concatMap (map snd) . tasks) other
                       , lockBy <- map snd (m ++ r)
                       ]

instance (VarValTime v x t) => TargetSystemComponent (Accum v x t) where
    moduleName _ _ = "pu_accum"
    hardware _tag _pu = FromLibrary "pu_accum.v"
    software _ _ = Empty
    hardwareInstance
        tag
        _pu
        UnitEnv
            { sigClk
            , sigRst
            , ctrlPorts = Just AccumPorts{..}
            , valueIn = Just (dataIn, attrIn)
            , valueOut = Just (dataOut, attrOut)
            } =
            [__i|
                pu_accum \#
                        ( .DATA_WIDTH( #{ dataWidth (def :: x) } )
                        , .ATTR_WIDTH( #{ attrWidth (def :: x) } )
                        ) #{ tag }
                    ( .clk( #{ sigClk } )
                    , .rst( #{ sigRst } )
                    , .signal_resetAcc( #{ resetAcc } )
                    , .signal_load( #{ load } )
                    , .signal_neg( #{ neg } )
                    , .signal_oe( #{ oe } )
                    , .data_in( #{ dataIn } )
                    , .attr_in( #{ attrIn } )
                    , .data_out( #{ dataOut } )
                    , .attr_out( #{ attrOut } )
                    );
            |]
    hardwareInstance _title _pu _env = error "internal error"

instance (Ord t) => WithFunctions (Accum v x t) (F v x) where
    functions Accum{process_, work} =
        functions process_ ++ map func work

instance (VarValTime v x t) => Testable (Accum v x t) v x where
    testBenchImplementation prj@Project{pName, pUnit} =
        let tbcSignalsConst = ["resetAcc", "load", "oe", "neg"]

            showMicrocode Microcode{resetAccSignal, loadSignal, oeSignal, negSignal} =
                ([i|resetAcc <= #{ bool2verilog resetAccSignal };|] :: String)
                    <> [i| load <= #{ bool2verilog loadSignal };|]
                    <> [i| oe <= #{ bool2verilog oeSignal };|]
                    <> [i| neg <= #{ bool2verilog $ fromMaybe False negSignal };|]

            conf =
                SnippetTestBenchConf
                    { tbcSignals = tbcSignalsConst
                    , tbcPorts =
                        AccumPorts
                            { resetAcc = SignalTag "resetAcc"
                            , load = SignalTag "load"
                            , oe = SignalTag "oe"
                            , neg = SignalTag "neg"
                            }
                    , tbcMC2verilogLiteral = T.pack . showMicrocode
                    }
         in Immediate (toString $ moduleName pName pUnit <> "_tb.v") $ snippetTestBench prj conf

instance IOTestBench (Accum v x t) v x

instance BreakLoopProblem (Accum v x t) v x
instance OptimizeAccumProblem (Accum v x t) v x
instance CompileTimeEvalProblem (Accum v x t) v x
instance ResolveDeadlockProblem (Accum v x t) v x
