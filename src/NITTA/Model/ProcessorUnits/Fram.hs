{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : NITTA.Model.ProcessorUnits.Fram
Description : Buffers inside and across computational cycles
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Fram (
    Fram (..),
    Ports (..),
    IOPorts (..),
    framWithSize,
) where

import Control.Applicative ((<|>))
import Control.Monad
import qualified Data.Array as A
import Data.Array.Base (numElements)
import Data.Bits (testBit)
import Data.Default
import qualified Data.List as L
import Data.Maybe
import qualified Data.Set as S
import Data.String.Interpolate
import Data.String.ToString
import qualified Data.String.Utils as S
import qualified Data.Text as T
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits.Types
import NITTA.Model.Types
import NITTA.Project
import NITTA.Utils
import NITTA.Utils.ProcessDescription
import Numeric.Interval.NonEmpty (inf, sup, (...))

data Fram v x t = Fram
    { -- |memory cell array
      memory :: A.Array Int (Cell v x t)
    , -- |register queue
      remainBuffers :: [(Buffer v x, Job v x t)]
    , process_ :: Process t (StepInfo v x t)
    }
    deriving (Show)

framWithSize size =
    Fram
        { memory = A.listArray (0, size - 1) $ repeat def
        , remainBuffers = []
        , process_ = def
        }

instance (Default t, Default x) => Default (Fram v x t) where
    def =
        Fram
            { memory = A.listArray (0, defaultSize - 1) $ repeat def
            , remainBuffers = []
            , process_ = def
            }
        where
            defaultSize = 16

instance Default x => DefaultX (Fram v x t) x

instance (VarValTime v x t) => WithFunctions (Fram v x t) (F v x) where
    functions Fram{remainBuffers, memory} =
        map (packF . fst) remainBuffers ++ concatMap functions (A.elems memory)

instance (VarValTime v x t) => Variables (Fram v x t) v where
    variables fram = S.unions $ map variables $ functions fram

-- |Memory cell
data Cell v x t = Cell
    { state :: CellState v x t
    , lastWrite :: Maybe t
    , -- |current job description
      job :: Maybe (Job v x t)
    , history :: [F v x]
    , initialValue :: x
    }
    deriving (Show)

data Job v x t = Job
    { function :: F v x
    , startAt :: Maybe t
    , binds, endpoints :: [ProcessStepID]
    }
    deriving (Show, Eq)

defJob f =
    Job
        { function = f
        , startAt = Nothing
        , binds = []
        , endpoints = []
        }

instance WithFunctions (Cell v x t) (F v x) where
    functions Cell{history, job = Just Job{function}} = function : history
    functions Cell{history} = history

instance (Default x) => Default (Cell v x t) where
    def =
        Cell
            { state = NotUsed
            , lastWrite = Nothing
            , job = Nothing
            , history = []
            , initialValue = def
            }

{- |Memory cell states. Add Loop&Buffer for optimisation.
@
               bind                    source
    NotUsed ----------> DoConstant ------------+----> Done
     |                        ^                |
     |                        |                |
     |                        \----------------/
     |
     |    bind
     +-------------------> ForBuffer <----------\
     |                         |                |
     |                         |                |
     |                  target |                |
     |                         |     /----------+
     |                         |     |          |
     |    target               v     v  source  |
     +-------------------> DoBuffer ------------/
     |
     |              refactor              source                      target
     \-- NotBrokenLoop --> DoLoopSource ----------+---> DoLoopTarget --------> Done
                               ^                  |
                               |                  |
                               \------------------/
@
-}
data CellState v x t
    = NotUsed
    | Done
    | DoConstant [v]
    | DoBuffer [v]
    | ForBuffer
    | NotBrokenLoop
    | DoLoopSource [v] (Job v x t)
    | DoLoopTarget v
    deriving (Show, Eq)

isFree Cell{state = NotUsed} = True
isFree _ = False

isForBuffer Cell{state = ForBuffer} = True
isForBuffer _ = False

lockableNotUsedCell Fram{memory, remainBuffers} =
    let free = filter (isFree . snd) $ A.assocs memory
        n = length free
     in if null remainBuffers && n >= 1 || not (null remainBuffers) && n >= 2
            then Just $ head free
            else Nothing

findForBufferCell Fram{memory} =
    case L.find (isForBuffer . snd) $ A.assocs memory of
        x@(Just _) -> x
        Nothing -> L.find (isFree . snd) $ A.assocs memory

oJobV Job{function}
    | Just (LoopEnd _ (I v)) <- castF function = v
    | otherwise = undefined

-- |Function for calculating width of array in Fram
addrWidth Fram{memory} = log2 $ numElements memory
    where
        log2 = ceiling . (logBase 2 :: Double -> Double) . fromIntegral

instance (VarValTime v x t) => ProcessorUnit (Fram v x t) v x t where
    tryBind f fram
        | not $ null (variables f `S.intersection` variables fram) =
            Left "can not bind (self transaction)"
    tryBind f fram@Fram{memory, remainBuffers}
        | Just (Constant (X x) (O vs)) <- castF f
          , Just (addr, _) <- lockableNotUsedCell fram =
            let (binds, process_) = runSchedule fram $ scheduleFunctionBind f
                cell =
                    Cell
                        { state = DoConstant $ S.elems vs
                        , job = Just (defJob f){binds}
                        , history = [f]
                        , lastWrite = Nothing
                        , initialValue = x
                        }
             in Right
                    fram
                        { memory = memory A.// [(addr, cell)]
                        , process_
                        }
        | Just (Loop (X x) (O _) (I _)) <- castF f
          , Just (addr, _) <- lockableNotUsedCell fram =
            let (binds, process_) = runSchedule fram $ scheduleFunctionBind f
                cell =
                    Cell
                        { state = NotBrokenLoop
                        , job = Just (defJob f){binds}
                        , history = [f]
                        , lastWrite = Nothing
                        , initialValue = x
                        }
             in Right
                    fram
                        { memory = memory A.// [(addr, cell)]
                        , process_
                        }
        | Just r@Buffer{} <- castF f
          , any (\case ForBuffer{} -> True; DoBuffer{} -> True; NotUsed{} -> True; _ -> False) $ map state $ A.elems memory =
            let (binds, process_) = runSchedule fram $ scheduleFunctionBind f
                job = (defJob f){binds}
             in Right
                    fram
                        { remainBuffers = (r, job) : remainBuffers
                        , process_
                        }
        | otherwise = Left $ "unsupport or cells over: " ++ show f

    process Fram{process_} = process_

instance (Var v) => Locks (Fram v x t) v where
    -- FIXME:
    locks _ = []

instance (VarValTime v x t) => BreakLoopProblem (Fram v x t) v x where
    breakLoopOptions Fram{memory} =
        [ BreakLoop x o i_
        | (_, Cell{state = NotBrokenLoop, job = Just Job{function}}) <- A.assocs memory
        , let Just (Loop (X x) (O o) (I i_)) = castF function
        ]
    breakLoopDecision fram@Fram{memory} bl@BreakLoop{loopO} =
        let Just (addr, cell@Cell{history, job = Just Job{binds}}) =
                L.find
                    ( \case
                        (_, Cell{job = Just Job{function}}) -> function == recLoop bl
                        _ -> False
                    )
                    $ A.assocs memory
            ((iPid, oPid), process_) = runSchedule fram $ do
                revoke <- scheduleFunctionRevoke $ recLoop bl
                f1 <- scheduleFunctionBind $ recLoopOut bl
                f2 <- scheduleFunctionBind $ recLoopIn bl
                establishVerticalRelations binds (f1 ++ f2 ++ revoke)
                return (f1, f2)
            iJob = (defJob $ recLoopOut bl){binds = iPid, startAt = Just 0}
            oJob = (defJob $ recLoopIn bl){binds = oPid}
            cell' =
                cell
                    { job = Just iJob
                    , history = [recLoopOut bl, recLoopIn bl] ++ history
                    , state = DoLoopSource (S.elems loopO) oJob
                    }
         in fram
                { memory = memory A.// [(addr, cell')]
                , process_
                }

instance OptimizeAccumProblem (Fram v x t) v x
instance ConstantFoldingProblem (Fram v x t) v x
instance ResolveDeadlockProblem (Fram v x t) v x

instance (VarValTime v x t) => EndpointProblem (Fram v x t) v t where
    endpointOptions Fram{process_ = Process{nextTick}, remainBuffers, memory} =
        let target v = EndpointSt (Target v) $ TimeConstrain (nextTick ... maxBound) (1 ... maxBound)
            source True vs = EndpointSt (Source $ S.fromList vs) $ TimeConstrain (1 + 1 + nextTick ... maxBound) (1 ... maxBound)
            source False vs = EndpointSt (Source $ S.fromList vs) $ TimeConstrain (1 + nextTick ... maxBound) (1 ... maxBound)

            fromRemain =
                if any (\case ForBuffer{} -> True; NotUsed{} -> True; _ -> False) $ map state $ A.elems memory
                    then map ((\(Buffer (I v) (O _)) -> target v) . fst) remainBuffers
                    else []

            foo Cell{state = NotUsed} = Nothing
            foo Cell{state = Done} = Nothing
            foo Cell{state = DoConstant vs} = Just $ source False vs
            foo Cell{state = DoBuffer vs, lastWrite} = Just $ source (fromMaybe 0 lastWrite == nextTick - 1) vs
            foo Cell{state = ForBuffer} = Nothing
            foo Cell{state = NotBrokenLoop} = Nothing
            foo Cell{state = DoLoopSource vs _, lastWrite} = Just $ source (fromMaybe 0 lastWrite == nextTick - 1) vs
            foo Cell{state = DoLoopTarget v} = Just $ target v

            fromCells = mapMaybe foo $ A.elems memory
         in fromRemain ++ fromCells

    -- Constant
    endpointDecision fram@Fram{memory} d@EndpointSt{epRole = Source vs, epAt}
        | Just (addr, cell@Cell{state = DoConstant vs', job = Just Job{function, binds, endpoints}}) <-
            L.find
                ( \case
                    (_, Cell{state = DoConstant vs'}) -> (vs' L.\\ S.elems vs) /= vs'
                    _ -> False
                )
                $ A.assocs memory =
            let vsRemain = vs' L.\\ S.elems vs
                ((), process_') = runSchedule fram $ do
                    updateTick (sup epAt + 1)
                    endpoints' <- scheduleEndpoint d $ scheduleInstruction (shiftI (-1) epAt) $ PrepareRead addr
                    when (null vsRemain) $ do
                        fPID <- scheduleFunction (0 ... sup epAt) function
                        establishVerticalRelations binds fPID
                        establishVerticalRelations fPID (endpoints ++ endpoints')
                cell' = case vsRemain of
                    [] ->
                        cell
                            { job = Nothing
                            , state = Done
                            }
                    _ ->
                        cell
                            { state = DoConstant vsRemain
                            }
             in fram
                    { memory = memory A.// [(addr, cell')]
                    , process_ = process_'
                    }
    -- Loop
    endpointDecision fram@Fram{memory} d@EndpointSt{epRole = Source vs, epAt}
        | Just (addr, cell@Cell{state = DoLoopSource vs' oJob, job = Just job@Job{binds, function, startAt, endpoints}}) <-
            L.find
                ( \case
                    (_, Cell{state = DoLoopSource vs' _}) -> (vs' L.\\ S.elems vs) /= vs'
                    _ -> False
                )
                $ A.assocs memory =
            let vsRemain = vs' L.\\ S.elems vs
                (endpoints', process_) = runSchedule fram $ do
                    updateTick (sup epAt + 1)
                    eps <- scheduleEndpoint d $ scheduleInstruction (shiftI (-1) epAt) $ PrepareRead addr
                    when (null vsRemain) $ do
                        fPID <- scheduleFunction (0 ... sup epAt) function
                        establishVerticalRelations binds fPID
                        establishVerticalRelations fPID $ eps ++ endpoints
                    return eps
                cell' =
                    if not $ null vsRemain
                        then
                            cell
                                { job = Just job{startAt = startAt <|> Just (inf epAt - 1), endpoints = endpoints' ++ endpoints}
                                , state = DoLoopSource vsRemain oJob
                                }
                        else
                            cell
                                { job = Just oJob{startAt = startAt <|> Just (inf epAt - 1)}
                                , state = DoLoopTarget $ oJobV oJob
                                }
             in fram{process_, memory = memory A.// [(addr, cell')]}
    endpointDecision fram@Fram{memory} d@EndpointSt{epRole = Target v, epAt}
        | Just (addr, cell@Cell{job = Just Job{function, binds, endpoints}}) <-
            L.find (\case (_, Cell{state = DoLoopTarget v'}) -> v == v'; _ -> False) $ A.assocs memory =
            let ((), process_) = runSchedule fram $ do
                    endpoints' <- scheduleEndpoint d $ scheduleInstruction epAt $ Write addr
                    updateTick (sup epAt + 1)
                    fPID <- scheduleFunction epAt function
                    establishVerticalRelations binds fPID
                    establishVerticalRelations fPID (endpoints ++ endpoints')
                cell' =
                    cell
                        { job = Nothing
                        , state = Done
                        }
             in fram
                    { memory = memory A.// [(addr, cell')]
                    , process_
                    }
    -- Buffer Target
    endpointDecision fram@Fram{memory, remainBuffers} d@EndpointSt{epRole = Target v, epAt}
        | Just (addr, cell@Cell{history}) <- findForBufferCell fram
          , ([(Buffer (I _) (O vs), j@Job{function})], remainBuffers') <- L.partition (\(Buffer (I v') (O _), _) -> v' == v) remainBuffers =
            let (endpoints, process_) = runSchedule fram $ do
                    updateTick (sup epAt + 1)
                    scheduleEndpoint d $ scheduleInstruction epAt $ Write addr
                cell' =
                    cell
                        { job = Just j{startAt = Just $ inf epAt, endpoints}
                        , state = DoBuffer $ S.elems vs
                        , lastWrite = Just $ sup epAt
                        , history = function : history
                        }
             in fram
                    { memory = memory A.// [(addr, cell')]
                    , remainBuffers = remainBuffers'
                    , process_
                    }
    endpointDecision fram@Fram{memory} d@EndpointSt{epRole = Source vs, epAt}
        | Just (addr, cell@Cell{state = DoBuffer vs', job = Just Job{function, startAt = Just fBegin, binds, endpoints}}) <-
            L.find
                ( \case
                    (_, Cell{state = DoBuffer vs'}) -> (vs' L.\\ S.elems vs) /= vs'
                    _ -> False
                )
                $ A.assocs memory =
            let vsRemain = vs' L.\\ S.elems vs
                ((), process_) = runSchedule fram $ do
                    updateTick (sup epAt + 1)
                    endpoints' <- scheduleEndpoint d $ scheduleInstruction (shiftI (-1) epAt) $ PrepareRead addr
                    when (null vsRemain) $ do
                        fPID <- scheduleFunction (fBegin ... sup epAt) function
                        establishVerticalRelations binds fPID
                        establishVerticalRelations fPID (endpoints ++ endpoints')
                cell' = case vsRemain of
                    [] ->
                        cell
                            { job = Nothing
                            , state = ForBuffer
                            }
                    _ ->
                        cell
                            { state = DoBuffer vsRemain
                            }
             in fram
                    { memory = memory A.// [(addr, cell')]
                    , process_
                    }
    endpointDecision Fram{memory} d =
        error
            [__i|
                fram model internal error: #{ d }
                cells state:
                #{ S.join "\n" $ map (\(ix, c) -> show ix <> ": " <> show (state c)) $ A.assocs memory }
            |]

---------------------------------------------------------------------

instance Controllable (Fram v x t) where
    data Instruction (Fram v x t)
        = PrepareRead Int
        | Write Int
        deriving (Show)

    data Microcode (Fram v x t) = Microcode
        { oeSignal :: Bool
        , wrSignal :: Bool
        , addrSignal :: Maybe Int
        }
        deriving (Show, Eq, Ord)

    zipSignalTagsAndValues FramPorts{oe, wr, addr} Microcode{oeSignal, wrSignal, addrSignal} =
        [ (oe, Bool oeSignal)
        , (wr, Bool wrSignal)
        ]
            ++ addrs
        where
            addrs =
                map
                    ( \(linkId, ix) ->
                        ( linkId
                        , maybe Undef (Bool . (`testBit` ix)) addrSignal
                        )
                    )
                    $ zip (reverse addr) [0 ..]

    usedPortTags FramPorts{oe, wr, addr} = oe : wr : addr

    takePortTags (oe : wr : xs) pu = FramPorts oe wr addr
        where
            width = addrWidth pu
            addr = take width xs
    takePortTags _ _ = error "can not take port tags, tags are over"

instance Connected (Fram v x t) where
    data Ports (Fram v x t) = FramPorts
        { oe, wr :: SignalTag
        , addr :: [SignalTag]
        }
        deriving (Show)

instance IOConnected (Fram v x t) where
    data IOPorts (Fram v x t) = FramIO
        deriving (Show)

instance Default (Microcode (Fram v x t)) where
    def = Microcode False False Nothing

instance UnambiguouslyDecode (Fram v x t) where
    decodeInstruction (PrepareRead addr) = Microcode True False $ Just addr
    decodeInstruction (Write addr) = Microcode False True $ Just addr

instance (VarValTime v x t) => Testable (Fram v x t) v x where
    testBenchImplementation prj@Project{pName, pUnit} =
        let tbcSignalsConst = ["oe", "wr", "[3:0] addr"]
            showMicrocode Microcode{oeSignal, wrSignal, addrSignal} =
                [i|oe <= #{ bool2verilog oeSignal };|]
                    <> [i| wr <= #{ bool2verilog wrSignal };|]
                    <> [i| addr <= #{ maybe "0" show addrSignal };|]
         in Immediate (toString $ moduleName pName pUnit <> "_tb.v") $
                snippetTestBench
                    prj
                    SnippetTestBenchConf
                        { tbcSignals = tbcSignalsConst
                        , tbcPorts =
                            FramPorts
                                { oe = SignalTag "oe"
                                , wr = SignalTag "wr"
                                , addr = map SignalTag ["addr[3]", "addr[2]", "addr[1]", "addr[0]"]
                                }
                        , tbcMC2verilogLiteral = showMicrocode
                        }

softwareFile tag pu = moduleName tag pu <> "." <> tag <> ".dump"

instance (VarValTime v x t) => TargetSystemComponent (Fram v x t) where
    moduleName _ _ = "pu_fram"
    hardware _tag _pu = FromLibrary "pu_fram.v"
    software tag fram@Fram{memory} =
        Immediate
            (toString $ softwareFile tag fram)
            $ T.unlines $
                map
                    (\Cell{initialValue = initialValue} -> hdlValDump initialValue)
                    $ A.elems memory
    hardwareInstance
        tag
        fram@Fram{memory}
        UnitEnv
            { sigClk
            , ctrlPorts = Just FramPorts{..}
            , valueIn = Just (dataIn, attrIn)
            , valueOut = Just (dataOut, attrOut)
            } =
            [__i|
                pu_fram \#
                        ( .DATA_WIDTH( #{ dataWidth (def :: x) } )
                        , .ATTR_WIDTH( #{ attrWidth (def :: x) } )
                        , .RAM_SIZE( #{ numElements memory } )
                        , .FRAM_DUMP( "$PATH$/#{ softwareFile tag fram }" )
                        ) #{ tag }
                    ( .clk( #{ sigClk } )
                    , .signal_addr( { #{ T.intercalate ", " $ map (T.pack . show) addr } } )
                    , .signal_wr( #{ wr } )
                    , .data_in( #{ dataIn } )
                    , .attr_in( #{ attrIn } )
                    , .signal_oe( #{ oe } )
                    , .data_out( #{ dataOut } )
                    , .attr_out( #{ attrOut } )
                    );
            |]
    hardwareInstance _title _pu _env = error "internal error"

instance IOTestBench (Fram v x t) v x
