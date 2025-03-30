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

import Data.Default
import Data.List
import Data.List (delete)
import Data.Set (Set)
import Data.Set qualified as S
import Data.String.Interpolate
import Data.String.ToString
import Data.Text qualified as T
import Debug.Trace
import NITTA.Intermediate.Functions hiding (remain)
import NITTA.Intermediate.Functions qualified as F hiding (remain)
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
                sources: #{ map toString sources }
                currentWork: #{ currentWork }

                #{ nest 4 $ pretty process_ }
            |]

multiplexer :: Time t => Multiplexer v x t
multiplexer =
    trace "M14" $
        Multiplexer
            { remain = []
            , sources = []
            , muxSels = []
            , targets = []
            , currentWork = Nothing
            , process_ = def
            }

instance VarValTime v x t => ProcessorUnit (Multiplexer v x t) v x t where
    tryBind f pu@Multiplexer{remain}
        | Just F.Mux{} <- castF f =
            trace "M1" $
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
        trace "M11" $
            [ (dataInPort, Bool dataInActive)
            , (selPort, Bool selActive)
            , (outPort, Bool outActive)
            ]
    usedPortTags MultiplexerPorts{..} = trace "M12" $ [dataInPort, selPort, outPort]

    takePortTags (oe : wr : sel : _) _ = trace "M13" $ MultiplexerPorts oe wr sel
    takePortTags _ _ = error "can not take port tags, tags are over"

instance VarValTime v x t => EndpointProblem (Multiplexer v x t) v t where
    endpointOptions pu@Multiplexer{sources, muxSels, targets}
        | (not $ null targets) || (not $ null muxSels) =
            trace
                ( "M2: "
                    ++ show (length targets)
                    ++ " "
                    ++ show (length muxSels)
                )
                $ let at = nextTick pu ... maxBound
                      duration = 1 ... maxBound
                   in map (\v -> EndpointSt (Target v) $ TimeConstraint at duration) (targets ++ muxSels)
        | not $ null sources =
            trace "M3" $
                let doneAt = nextTick (process_ pu) + 2
                    at = doneAt ... maxBound
                    duration = 1 ... maxBound
                 in [EndpointSt (Source $ S.fromList sources) $ TimeConstraint at duration]
        | otherwise = trace ("M4: " ++ show (remain pu)) $ concatMap (endpointOptions . execution pu) (remain pu)

    endpointDecision pu@Multiplexer{muxSels, targets} d@EndpointSt{epRole = Target v, epAt}
        | v `elem` targets =
            trace ("M5: " ++ show (length targets) ++ " " ++ show (length muxSels)) $
                let process_' = execSchedule pu $ do
                        scheduleEndpoint d $ scheduleInstructionUnsafe epAt LoadInput
                 in pu
                        { targets = filter (/= v) targets
                        , process_ = process_'
                        , muxSels = muxSels
                        }
        | v `elem` muxSels =
            trace ("M6: " ++ show (length muxSels)) $
                let process_' = execSchedule pu $ do
                        scheduleEndpoint d $ scheduleInstructionUnsafe epAt LoadSel
                 in pu
                        { muxSels = filter (/= v) muxSels
                        , process_ = process_'
                        , targets = targets
                        }
    endpointDecision pu@Multiplexer{sources} d@EndpointSt{epRole = Source vs, epAt}
        | not $ null sources =
            trace "M7" $
                let process_' = execSchedule pu $ do
                        scheduleEndpoint d $ scheduleInstructionUnsafe epAt Out
                    sources' = sources \\ S.elems vs
                 in pu
                        { sources = sources' -- []
                        , process_ = process_'
                        }
    endpointDecision pu@Multiplexer{targets = [], sources = [], muxSels = [], remain} d
        | let v = trace "M8" $ oneOf $ variables d
        , Just f <- find (\f -> v `S.member` variables f) remain =
            endpointDecision (execution pu f) d
    endpointDecision pu d = error [i|incorrect decision #{ d } for #{ pretty pu }|]

-- endpointDecision pu@Multiplexer{sources, muxSels, targets} d@EndpointSt{epRole = Target v, epAt}
--     | v `elem` sources =
--         trace "M5" $
--             let process_' = execSchedule pu $ do
--                     scheduleEndpoint d $ scheduleInstructionUnsafe epAt LoadInput
--              in pu
--                     { sources = filter (/= v) sources
--                     , process_ = process_'
--                     }
--     | v `elem` muxSels =
--         trace "M6" $
--             let process_' = execSchedule pu $ do
--                     scheduleEndpoint d $ scheduleInstructionUnsafe epAt LoadSel
--              in pu
--                     { muxSels = filter (/= v) muxSels
--                     , process_ = process_'
--                     }
-- endpointDecision pu@Multiplexer{targets} d@EndpointSt{epRole = Source vs, epAt}
--     | not $ null targets =
--         trace "M7" $
--             let process_' = execSchedule pu $ do
--                     scheduleEndpoint d $ scheduleInstructionUnsafe epAt Out
--              in pu
--                     { targets = []
--                     , process_ = process_'
--                     }
-- endpointDecision pu@Multiplexer{targets = [], sources = [], muxSels = [], remain} d
--     | let v = trace "M8" $ oneOf $ variables d
--     , Just f <- find (\f -> v `S.member` variables f) remain =
--         endpointDecision (execution pu f) d
-- endpointDecision pu d = error [i|incorrect decision #{ d } for #{ pretty pu }|]

execution pu@Multiplexer{targets = [], sources = [], muxSels = [], remain} f
    | Just (F.Mux a b (O c)) <- castF f =
        trace "M9" $
            pu
                { sources = S.elems c
                , muxSels = map (\(I v) -> v) b
                , targets = map (\(I v) -> v) a
                , remain = filter (/= f) remain
                , currentWork = Just f
                }
execution _ f = error $ "Multiplexer execution error. Expected Mux, got: " ++ show f

-- instance Var v => Locks (Multiplexer v x t) v where
--     locks Multiplexer{targets, sources, muxSels} =
--         [Lock{lockBy = l, locked = o} | o <- targets, l <- sources ++ muxSels]

instance Var v => Locks (Multiplexer v x t) v where
    -- locks _ = []
    locks Multiplexer{targets, muxSels, sources} =
        trace "M10" $
            [ Lock lockBy locked
            | locked <- sources
            , lockBy <- targets ++ muxSels
            ]
instance TargetSystemComponent (Multiplexer v x t) where
    moduleName _ _ = T.pack "pu_multiplexer"

    hardware _tag _pu = FromLibrary "multiplexer.v"
    software _ _ = Empty

    hardwareInstance
        tag
        _pu
        UnitEnv
            { sigClk
            , ctrlPorts = Just MultiplexerPorts{}
            , valueIn = Just (dataIn, attrIn)
            , valueOut = Just (dataOut, _)
            } =
            [__i|
        pu_multiplexer #{ tag } (
            .clk(#{ sigClk }),
            .data_in(#{ dataIn }),
            .sel(#{ attrIn }),
            .out(#{ dataOut })
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
        trace "M15" $
            MuxMicrocode
                { dataInActive = False
                , selActive = False
                , outActive = False
                }

instance UnambiguouslyDecode (Multiplexer v x t) where
    decodeInstruction Out = trace "M16" $ def{outActive = True}
    decodeInstruction LoadInput = trace "M17" $ def{dataInActive = True}
    decodeInstruction LoadSel = trace "M18" $ def{selActive = True}

-- anyInterval :: Time t => t -> TimeConstraint t
-- anyInterval duration = 0 ... maxBound ~~ duration ... duration

-- scheduleInstruction :: EndpointSt v t -> Instruction pu -> Process t (StepInfo v x t)
-- scheduleInstruction endpoint instr = do
--     scheduleEndpoint endpoint
--     addStep (InstructionStep instr)
