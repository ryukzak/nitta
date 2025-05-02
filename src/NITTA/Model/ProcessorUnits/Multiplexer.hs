{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module NITTA.Model.ProcessorUnits.Multiplexer (
    Multiplexer,
    multiplexer,
    IOPorts (..),
) where

import Control.Monad (when)
import Data.Default
import Data.List (find, (\\))
import Data.Maybe (maybeToList)
import qualified Data.Set as S
import Data.String.Interpolate
import Data.String.ToString
import qualified Data.Text as T
import qualified NITTA.Intermediate.Functions as F hiding (remain)
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits.Types
import NITTA.Model.Time
import NITTA.Project
import NITTA.Utils
import NITTA.Utils.ProcessDescription
import Numeric.Interval.NonEmpty hiding (elem, notElem)
import Prettyprinter

data Multiplexer v x t = Multiplexer
    { remain :: [F v x]
    , sources :: [v]
    , muxSels :: [v]
    , targets :: [v]
    , currentWork :: Maybe (F v x)
    , process_ :: Process t (StepInfo v x t)
    }

instance Default x => DefaultX (Multiplexer v x t) x

instance Time t => Default (Multiplexer v x t) where
    def = multiplexer

instance VarValTime v x t => Pretty (Multiplexer v x t) where
    pretty Multiplexer{remain, targets, sources, currentWork, process_, muxSels} =
        [__i|
            Multiplexer:
                remain: #{ remain }
                targets: #{ map toString targets }
                muxSels: #{ map toString muxSels }
                sources: #{ map toString sources }
                currentWork: #{ currentWork }
                #{ nest 4 $ pretty process_ }
            |]

multiplexer :: Time t => Multiplexer v x t
multiplexer =
    Multiplexer
        { remain = []
        , sources = []
        , muxSels = []
        , targets = []
        , currentWork = Nothing
        , process_ = def
        }

selWidth :: Int
selWidth = 4 -- todo should fix
instance VarValTime v x t => ProcessorUnit (Multiplexer v x t) v x t where
    tryBind f pu@Multiplexer{remain}
        | Just F.Mux{} <- castF f =
            Right
                pu
                    { remain = f : remain
                    }
        | otherwise = Left "Unsupported function type for Multiplexer"

    process = process_

instance Connected (Multiplexer v x t) where
    data Ports (Multiplexer v x t) = MultiplexerPorts
        { dataInPort :: SignalTag
        , selPort :: SignalTag
        , outPort :: SignalTag
        }

instance Controllable (Multiplexer v x t) where
    data Instruction (Multiplexer v x t)
        = LoadInput
        | LoadSel
        | Out
        deriving (Show, Eq)

    data Microcode (Multiplexer v x t) = MuxMicrocode
        { dataInActive :: Bool
        , selActive :: Bool
        , outActive :: Bool
        }

    zipSignalTagsAndValues MultiplexerPorts{..} MuxMicrocode{..} =
        [ (dataInPort, Bool dataInActive)
        , (selPort, Bool selActive)
        , (outPort, Bool outActive)
        ]
    usedPortTags MultiplexerPorts{..} = [dataInPort, selPort, outPort]

    takePortTags (oe : wr : sel : _) _ = MultiplexerPorts oe wr sel
    takePortTags _ _ = error "can not take port tags, tags are over"

instance VarValTime v x t => EndpointProblem (Multiplexer v x t) v t where
    endpointOptions pu@Multiplexer{sources, muxSels, targets}
        | not (null targets) || not (null muxSels) =
            let at = nextTick pu ... maxBound
                duration = 1 ... maxBound
             in -- in map (\v -> EndpointSt (Target v) $ TimeConstraint at duration) (targets ++ muxSels)
                [EndpointSt (Target $ head $ targets ++ muxSels) $ TimeConstraint at duration]
        | not $ null sources =
            let doneAt = nextTick (process_ pu) + 3
                at = doneAt ... maxBound
                duration = 1 ... maxBound
             in [EndpointSt (Source $ S.fromList sources) $ TimeConstraint at duration]
        | otherwise = concatMap (endpointOptions . execution pu) (remain pu)

    endpointDecision pu@Multiplexer{muxSels, targets} d@EndpointSt{epRole = Target v, epAt}
        | v `elem` targets =
            let process_' = execSchedule pu $ do
                    scheduleEndpoint d $ scheduleInstructionUnsafe epAt LoadInput
             in pu
                    { targets = filter (/= v) targets
                    , process_ = process_'
                    , muxSels = muxSels
                    }
        | v `elem` muxSels =
            let process_' = execSchedule pu $ do
                    scheduleEndpoint d $ scheduleInstructionUnsafe epAt LoadSel
             in pu
                    { muxSels = filter (/= v) muxSels
                    , process_ = process_'
                    , targets = targets
                    }
    endpointDecision pu@Multiplexer{sources, currentWork = Just f} d@EndpointSt{epRole = Source vs, epAt}
        | not $ null sources =
            let sources' = sources \\ S.elems vs
                process_' = execSchedule pu $ do
                    _ <- scheduleEndpoint d $ scheduleInstructionUnsafe epAt Out
                    when (null sources') $ do
                        let a = inf $ stepsInterval $ relatedEndpoints (process_ pu) (variables f)
                        scheduleFunctionFinish_ [] f (a ... sup epAt)
             in pu
                    { sources = sources'
                    , process_ = process_'
                    , currentWork = if null sources' then Nothing else Just f
                    }
    endpointDecision pu@Multiplexer{targets = [], sources = [], muxSels = [], remain} d
        | let v = oneOf $ variables d
        , Just f <- find (\f -> v `S.member` variables f) remain =
            endpointDecision (execution pu f) d
    endpointDecision pu d = error [i|incorrect decision #{ d } for #{ pretty pu }|]

execution pu@Multiplexer{targets = [], sources = [], muxSels = [], remain} f
    | Just (F.Mux a b (O c)) <- castF f =
        pu
            { sources = S.elems c
            , muxSels = [(\(I v) -> v) b]
            , targets = map (\(I v) -> v) a
            , remain = filter (/= f) remain
            , currentWork = Just f
            }
execution _ f = error $ "Multiplexer execution error. Expected Mux, got: " ++ show f

instance Var v => Locks (Multiplexer v x t) v where
    locks Multiplexer{targets, muxSels, sources} =
        [ Lock lockBy locked
        | locked <- sources
        , lockBy <- targets ++ muxSels
        ]
instance VarValTime v x t => TargetSystemComponent (Multiplexer v x t) where
    moduleName _ _ = T.pack "pu_multiplexer"

    hardware _tag _pu = FromLibrary "pu_multiplexer.v"
    software _ _ = Empty

    hardwareInstance
        tag
        _pu
        UnitEnv
            { sigClk
            , sigRst
            , ctrlPorts = Just MultiplexerPorts{..}
            , valueIn = Just (dataIn, attrIn)
            , valueOut = Just (dataOut, attrOut)
            } =
            [__i|
        pu_multiplexer \#
                ( .DATA_WIDTH( #{ dataWidth (def :: x) } )
                , .ATTR_WIDTH( #{ attrWidth (def :: x) } )
                , .SEL_WIDTH( #{ selWidth} )
                ) #{ tag } (
            .clk(#{ sigClk }),
            .rst(#{ sigRst }),
            .data_active(#{ dataInPort }),
            .sel_active(#{ selPort }),
            .out_active(#{ outPort }),

            .data_in( #{ dataIn } ),
            .attr_in( #{ attrIn } ),
            .data_out(#{ dataOut }),
            .attr_out(#{ attrOut })
        );|]
    hardwareInstance _title _pu _env = error "internal error"

instance IOConnected (Multiplexer v x t) where
    data IOPorts (Multiplexer v x t) = MultiplexerIO
        deriving (Show)

instance BreakLoopProblem (Multiplexer v x t) v x

instance ConstantFoldingProblem (Multiplexer v x t) v x

instance OptimizeAccumProblem (Multiplexer v x t) v x

instance OptimizeLutProblem (Multiplexer v x t) v x

instance ResolveDeadlockProblem (Multiplexer v x t) v x

instance IOTestBench (Multiplexer v x t) v x

instance Default (Microcode (Multiplexer v x t)) where
    def =
        MuxMicrocode
            { dataInActive = False
            , selActive = False
            , outActive = False
            }

instance UnambiguouslyDecode (Multiplexer v x t) where
    decodeInstruction Out = def{outActive = True, selActive = False, dataInActive = False}
    decodeInstruction LoadInput = def{dataInActive = True, outActive = False, selActive = False}
    decodeInstruction LoadSel = def{selActive = True, outActive = False, dataInActive = False}

instance VarValTime v x t => WithFunctions (Multiplexer v x t) (F v x) where
    functions Multiplexer{process_, remain, currentWork} =
        functions process_ ++ remain ++ maybeToList currentWork

instance VarValTime v x t => Testable (Multiplexer v x t) v x where
    testBenchImplementation prj@Project{pName, pUnit} =
        let tbcSignalsConst = map T.pack ["data_active", "sel_active", "out_active"]
            showMicrocode MuxMicrocode{..} =
                [i|data_active <= #{ bool2verilog dataInActive };|]
                    <> [i| sel_active <= #{ bool2verilog selActive };|]
                    <> [i| out_active <= #{ bool2verilog outActive };|]
         in Immediate (toString $ moduleName pName pUnit <> T.pack "_tb.v") $
                snippetTestBench
                    prj
                    SnippetTestBenchConf
                        { tbcSignals = tbcSignalsConst
                        , tbcPorts =
                            MultiplexerPorts
                                { dataInPort = SignalTag (T.pack "data_active")
                                , selPort = SignalTag (T.pack "sel_active")
                                , outPort = SignalTag (T.pack "out_active")
                                }
                        , tbcMC2verilogLiteral = showMicrocode
                        }
