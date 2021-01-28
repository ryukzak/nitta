{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : NITTA.Model.ProcessorUnits.Fram
Description : Register file
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
import qualified Data.String.Utils as S
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits.Types
import NITTA.Model.Types
import NITTA.Project
import NITTA.Utils
import NITTA.Utils.ProcessDescription
import Numeric.Interval.NonEmpty (inf, sup, (...))
import Text.InterpolatedString.Perl6 (qc)

data Fram v x t = Fram
    { -- |memory cell array
      memory :: A.Array Int (Cell v x t)
    , -- |register queue
      remainRegs :: [(Reg v x, Job v x t)]
    , process_ :: Process v x t
    }
    deriving (Show)

framWithSize size =
    Fram
        { memory = A.listArray (0, size - 1) $ repeat def
        , remainRegs = []
        , process_ = def
        }

instance
    ( Default t
    , Default x
    ) =>
    Default (Fram v x t)
    where
    def =
        Fram
            { memory = A.listArray (0, defaultSize - 1) $ repeat def
            , remainRegs = []
            , process_ = def
            }
        where
            defaultSize = 16

instance Default x => DefaultX (Fram v x t) x

instance
    ( VarValTime v x t
    ) =>
    WithFunctions (Fram v x t) (F v x)
    where
    functions Fram{remainRegs, memory} =
        map (packF . fst) remainRegs ++ concatMap functions (A.elems memory)

instance
    ( VarValTime v x t
    ) =>
    Variables (Fram v x t) v
    where
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

{- |Memory cell states. Add Loop&Reg for optimisation.
@
               bind                    source
    NotUsed ----------> DoConstant ------------+----> Done
     |                        ^                |
     |                        |                |
     |                        \----------------/
     |
     |    bind
     +-------------------> ForReg <-------------\
     |                         |                |
     |                         |                |
     |                  target |                |
     |                         |     /----------+
     |                         |     |          |
     |    target               v     v  source  |
     +-------------------> DoReg ---------------/
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
    | DoReg [v]
    | ForReg
    | NotBrokenLoop
    | DoLoopSource [v] (Job v x t)
    | DoLoopTarget v
    deriving (Show, Eq)

isFree Cell{state = NotUsed} = True
isFree _ = False

isForReg Cell{state = ForReg} = True
isForReg _ = False

lockableNotUsedCell Fram{memory, remainRegs} =
    let free = filter (isFree . snd) $ A.assocs memory
        n = length free
     in if null remainRegs && n >= 1 || not (null remainRegs) && n >= 2
            then Just $ head free
            else Nothing

findForRegCell Fram{memory} =
    case L.find (isForReg . snd) $ A.assocs memory of
        x@(Just _) -> x
        Nothing -> L.find (isFree . snd) $ A.assocs memory

oJobV Job{function}
    | Just (LoopIn _ (I v)) <- castF function = v
    | otherwise = undefined

-- |Function for calculating width of array in Fram
addrWidth Fram{memory} = log2 $ numElements memory
    where
        log2 = ceiling . (logBase 2 :: Double -> Double) . fromIntegral

instance
    ( VarValTime v x t
    ) =>
    ProcessorUnit (Fram v x t) v x t
    where
    tryBind f fram
        | not $ null (variables f `S.intersection` variables fram) =
            Left "can not bind (self transaction)"
    tryBind f fram@Fram{memory, remainRegs}
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
        | Just r@Reg{} <- castF f
          , any (\case ForReg{} -> True; DoReg{} -> True; NotUsed{} -> True; _ -> False) $ map state $ A.elems memory =
            let (binds, process_) = runSchedule fram $ scheduleFunctionBind f
                job = (defJob f){binds}
             in Right
                    fram
                        { remainRegs = (r, job) : remainRegs
                        , process_
                        }
        | otherwise = Left $ "unsupport or cells over: " ++ show f

    process Fram{process_} = process_

instance (Var v) => Locks (Fram v x t) v where
    -- FIXME:
    locks _ = []

instance (VarValTime v x t) => BreakLoopProblem (Fram v x t) v x where
    breakLoopOptions Fram{memory} =
        [ BreakLoop x o i
        | (_, Cell{state = NotBrokenLoop, job = Just Job{function}}) <- A.assocs memory
        , let Just (Loop (X x) (O o) (I i)) = castF function
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
instance ResolveDeadlockProblem (Fram v x t) v x

instance
    ( VarValTime v x t
    ) =>
    EndpointProblem (Fram v x t) v t
    where
    endpointOptions Fram{process_ = Process{nextTick}, remainRegs, memory} =
        let target v = EndpointSt (Target v) $ TimeConstrain (nextTick ... maxBound) (1 ... maxBound)
            source True vs = EndpointSt (Source $ S.fromList vs) $ TimeConstrain (1 + 1 + nextTick ... maxBound) (1 ... maxBound)
            source False vs = EndpointSt (Source $ S.fromList vs) $ TimeConstrain (1 + nextTick ... maxBound) (1 ... maxBound)

            fromRemain =
                if any (\case ForReg{} -> True; NotUsed{} -> True; _ -> False) $ map state $ A.elems memory
                    then map ((\(Reg (I v) (O _)) -> target v) . fst) remainRegs
                    else []

            foo Cell{state = NotUsed} = Nothing
            foo Cell{state = Done} = Nothing
            foo Cell{state = DoConstant vs} = Just $ source False vs
            foo Cell{state = DoReg vs, lastWrite} = Just $ source (fromMaybe 0 lastWrite == nextTick - 1) vs
            foo Cell{state = ForReg} = Nothing
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
    -- Reg Target
    endpointDecision fram@Fram{memory, remainRegs} d@EndpointSt{epRole = Target v, epAt}
        | Just (addr, cell@Cell{history}) <- findForRegCell fram
          , ([(Reg (I _) (O vs), j@Job{function})], remainRegs') <- L.partition (\(Reg (I v') (O _), _) -> v' == v) remainRegs =
            let (endpoints, process_) = runSchedule fram $ do
                    updateTick (sup epAt + 1)
                    scheduleEndpoint d $ scheduleInstruction epAt $ Write addr
                cell' =
                    cell
                        { job = Just j{startAt = Just $ inf epAt, endpoints}
                        , state = DoReg $ S.elems vs
                        , lastWrite = Just $ sup epAt
                        , history = function : history
                        }
             in fram
                    { memory = memory A.// [(addr, cell')]
                    , remainRegs = remainRegs'
                    , process_
                    }
    endpointDecision fram@Fram{memory} d@EndpointSt{epRole = Source vs, epAt}
        | Just (addr, cell@Cell{state = DoReg vs', job = Just Job{function, startAt = Just fBegin, binds, endpoints}}) <-
            L.find
                ( \case
                    (_, Cell{state = DoReg vs'}) -> (vs' L.\\ S.elems vs) /= vs'
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
                            , state = ForReg
                            }
                    _ ->
                        cell
                            { state = DoReg vsRemain
                            }
             in fram
                    { memory = memory A.// [(addr, cell')]
                    , process_
                    }
    endpointDecision Fram{memory} d =
        error $
            "fram model internal error: "
                ++ show d
                ++ "\n cells state: \n"
                ++ S.join "\n" (map (\(i, c) -> show i ++ ": " ++ show (state c)) $ A.assocs memory)

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

    mapMicrocodeToPorts Microcode{oeSignal, wrSignal, addrSignal} FramPorts{oe, wr, addr} =
        [ (oe, Bool oeSignal)
        , (wr, Bool wrSignal)
        ]
            ++ addrs
        where
            addrs =
                map
                    ( \(linkId, i) ->
                        ( linkId
                        , maybe Undef (Bool . (`testBit` i)) addrSignal
                        )
                    )
                    $ zip (reverse addr) [0 ..]

    portsToSignals FramPorts{oe, wr, addr} = oe : wr : addr

    signalsToPorts (oe : wr : xs) pu = FramPorts oe wr addr
        where
            width = addrWidth pu
            addr = take width xs
    signalsToPorts _ _ = error "pattern match error in signalsToPorts FramIOPorts"

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
        let width = addrWidth pUnit
            tbcSignalsConst = ["oe", "wr", "[3:0] addr"]

            showMicrocode Microcode{oeSignal, wrSignal, addrSignal} =
                [qc|oe <= { bool2verilog oeSignal };|]
                    <> [qc| wr <= { bool2verilog wrSignal };|]
                    <> [qc| addr <= { maybe "0" show addrSignal };|]

            signal (SignalTag i) = case i of
                0 -> "oe"
                1 -> "wr"
                j -> "addr[" ++ show (width - (j - 1)) ++ "]"
         in Immediate (moduleName pName pUnit ++ "_tb.v") $
                snippetTestBench
                    prj
                    SnippetTestBenchConf
                        { tbcSignals = tbcSignalsConst
                        , tbcPorts =
                            FramPorts
                                { oe = SignalTag 0
                                , wr = SignalTag 1
                                , addr = map SignalTag [2, 3, 4, 5]
                                }
                        , tbcIOPorts = FramIO
                        , tbcSignalConnect = signal
                        , tbcCtrl = showMicrocode
                        }

softwareFile tag pu = moduleName tag pu ++ "." ++ tag ++ ".dump"

instance (VarValTime v x t) => TargetSystemComponent (Fram v x t) where
    moduleName _ _ = "pu_fram"
    hardware tag pu = FromLibrary $ moduleName tag pu ++ ".v"
    software tag fram@Fram{memory} =
        Immediate
            (softwareFile tag fram)
            $ unlines $
                map
                    (\Cell{initialValue = initialValue} -> hdlValDump initialValue)
                    $ A.elems memory
    hardwareInstance tag fram@Fram{memory} TargetEnvironment{unitEnv = ProcessUnitEnv{..}, signalClk} FramPorts{..} FramIO =
        codeBlock
            [qc|
            pu_fram #
                    ( .DATA_WIDTH( { dataWidth (def :: x) } )
                    , .ATTR_WIDTH( { attrWidth (def :: x) } )
                    , .RAM_SIZE( { numElements memory } )
                    , .FRAM_DUMP( "$path${ softwareFile tag fram }" )
                    ) { tag }
                ( .clk( { signalClk } )
                , .signal_addr( \{ { S.join ", " (map signal addr ) } } )
                , .signal_wr( { signal wr } )
                , .data_in( { dataIn } )
                , .attr_in( { attrIn } )
                , .signal_oe( { signal oe } )
                , .data_out( { dataOut } )
                , .attr_out( { attrOut } )
                );
            |]
    hardwareInstance _title _pu TargetEnvironment{unitEnv = NetworkEnv{}} _ports _io =
        error "Should be defined in network."

instance IOTestBench (Fram v x t) v x
