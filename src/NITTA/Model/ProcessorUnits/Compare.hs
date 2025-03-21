{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module NITTA.Model.ProcessorUnits.Compare (
    Compare,
    compare,
    Ports (..),
    IOPorts (..),
) where

import Data.Default (Default, def)
import Data.List (partition, (\\))
import Data.Set qualified as S
import Data.String.ToString
import GHC.Generics
import NITTA.Intermediate.Functions qualified as F
import NITTA.Intermediate.Types
import NITTA.Model.ProcessorUnits.Types

import Control.Monad (when)
import Data.Foldable
import Data.Maybe
import Data.String.Interpolate
import Data.Text qualified as T
import NITTA.Model.Problems
import NITTA.Model.Time
import NITTA.Project
import NITTA.Utils
import NITTA.Utils.ProcessDescription
import Numeric.Interval.NonEmpty hiding (elem, notElem)
import Prettyprinter
import Prelude hiding (compare)

data Compare v x t = Compare
    { remain :: [F v x]
    , targets :: [v]
    , sources :: [v]
    , currentWork :: Maybe (F v x)
    , process_ :: Process t (StepInfo v x t)
    , cmpOp :: Maybe F.Op
    }

compare :: Time t => Compare v x t
compare =
    Compare
        { remain = []
        , targets = []
        , sources = []
        , currentWork = Nothing
        , process_ = def
        , cmpOp = Nothing
        }

instance VarValTime v x t => ProcessorUnit (Compare v x t) v x t where
    tryBind f pu@Compare{remain}
        | Just F.LogicCompare{} <- castF f =
            Right
                pu
                    { remain = f : remain
                    }
        | otherwise = Left "Unsupported function type for Compare"

    process = process_

instance Connected (Compare v x t) where
    data Ports (Compare v x t) = ComparePorts
        { wrPort :: SignalTag
        , oePort :: SignalTag
        , opSelPort :: [SignalTag]
        }
        deriving (Show, Generic)

instance Controllable (Compare v x t) where
    data Instruction (Compare v x t)
        = Load F.Op
        | Out
        deriving (Show, Generic)

    data Microcode (Compare v x t) = Microcode
        { wr :: Bool
        , oe :: Bool
        , opSel :: [Bool]
        }
        deriving (Show, Eq, Generic)

    zipSignalTagsAndValues ComparePorts{..} Microcode{..} =
        [ (wrPort, Bool wr)
        , (oePort, Bool oe)
        ]
            ++ zip opSelPort (map Bool opSel)

    usedPortTags ComparePorts{wrPort, oePort, opSelPort} = wrPort : oePort : opSelPort

    takePortTags (oe : wr : sel) _ = ComparePorts oe wr sel
    takePortTags _ _ = error "can not take port tags, tags are over"

instance Var v => Locks (Compare v x t) v where
    locks Compare{remain, sources, targets} =
        [ Lock{lockBy, locked}
        | locked <- sources
        , lockBy <- targets
        ]
            ++ [ Lock{lockBy, locked}
               | locked <- concatMap (S.elems . variables) remain
               , lockBy <- sources ++ targets
               ]
            ++ concatMap locks remain

instance Default (Microcode (Compare v x t)) where
    def =
        Microcode
            { wr = False
            , oe = False
            , opSel = [False, False, False, False, False]
            }

instance UnambiguouslyDecode (Compare v x t) where
    decodeInstruction Out = def{oe = True}
    decodeInstruction (Load op) = case op of
        F.EQ -> def{opSel = [True, False, False, False, False], wr = True}
        F.LT -> def{opSel = [False, True, False, False, False], wr = True}
        F.LTE -> def{opSel = [False, False, True, False, False], wr = True}
        F.GT -> def{opSel = [False, False, False, True, False], wr = True}
        F.GTE -> def{opSel = [False, False, False, False, True], wr = True}

instance Default x => DefaultX (Compare v x t) x

instance Time t => Default (Compare v x t) where
    def = compare

instance VarValTime v x t => EndpointProblem (Compare v x t) v t where
    endpointOptions pu@Compare{targets}
        | not $ null targets =
            let at = nextTick pu ... maxBound
                duration = 1 ... maxBound
             in map (\v -> EndpointSt (Target v) $ TimeConstraint at duration) targets
    endpointOptions Compare{sources, currentWork = Just f, process_}
        | not $ null sources =
            let doneAt = inputsPushedAt process_ f + 3
                at = max doneAt (nextTick process_) ... maxBound
                duration = 1 ... maxBound
             in [EndpointSt (Source $ S.fromList sources) $ TimeConstraint at duration]
    endpointOptions pu@Compare{remain} = concatMap (endpointOptions . execution pu) remain

    endpointDecision pu@Compare{targets, cmpOp} d@EndpointSt{epRole = Target v, epAt}
        | not $ null targets
        , ([_], targets') <- partition (== v) targets
        , --  Computation process planning is carried out.
          let process_' = execSchedule pu $ do
                -- this is required for correct work of automatically generated tests,
                -- that takes information about time from Process
                case cmpOp of
                    Just op -> scheduleEndpoint d $ scheduleInstructionUnsafe epAt (Load op)
                    Nothing -> error "cmpOp is Nothing" =
            pu
                { process_ = process_'
                , -- The remainder of the work is saved for the next loop
                  targets = targets'
                }
    endpointDecision pu@Compare{targets = [], sources, currentWork = Just f, process_} d@EndpointSt{epRole = Source v, epAt}
        | not $ null sources
        , let sources' = sources \\ S.elems v
        , sources' /= sources
        , let a = inf $ stepsInterval $ relatedEndpoints process_ $ variables f
        , -- Compututation process planning is carring on.
          let process_' = execSchedule pu $ do
                endpoints <- scheduleEndpoint d $ scheduleInstructionUnsafe epAt Out
                when (null sources') $ do
                    scheduleFunctionFinish_ [] f $ a ... sup epAt
                return endpoints =
            pu
                { process_ = process_'
                , -- In case if not all variables what asked - remaining are saved.
                  sources = sources'
                , -- if all of works is done, then time when result is ready,
                  -- current work and data transfering, what is done is the current function is reset.
                  currentWork = if null sources' then Nothing else Just f
                }
    endpointDecision pu@Compare{targets = [], sources = [], remain} d
        | let v = oneOf $ variables d
        , Just f <- find (\f -> v `S.member` variables f) remain =
            endpointDecision (execution pu f) d
    endpointDecision pu d = error [i|incorrect decision #{ d } for #{ pretty pu }|]

execution pu@Compare{targets = [], sources = [], remain} f
    | Just (F.LogicCompare op (I a) (I b) (O c)) <- castF f =
        pu
            { targets = [a, b]
            , currentWork = Just f
            , sources = S.elems c
            , remain = remain \\ [f]
            , cmpOp = Just op
            }
execution _ f =
    error $
        "Compare: internal execution error. Expected LogicCompare, got: " ++ show f

instance VarValTime v x t => Pretty (Compare v x t) where
    pretty Compare{remain, targets, sources, currentWork, process_, cmpOp} =
        [__i|
            Compare:
                remain: #{ remain }
                targets: #{ map toString targets }
                sources: #{ map toString sources }
                currentWork: #{ currentWork }
                cmpOp: #{ cmpOp }
                #{ nest 4 $ pretty process_ }
            |]

instance IOConnected (Compare v x t) where
    data IOPorts (Compare v x t) = CompareIO
        deriving (Show)

instance BreakLoopProblem (Compare v x t) v x

instance ConstantFoldingProblem (Compare v x t) v x

instance OptimizeAccumProblem (Compare v x t) v x

instance OptimizeLutProblem (Compare v x t) v x

instance ResolveDeadlockProblem (Compare v x t) v x

instance IOTestBench (Compare v x t) v x

instance TargetSystemComponent (Compare v x t) where
    moduleName _ _ = T.pack "compare_unit"
    software _ _ = Empty
    hardware _tag _pu = FromLibrary "compare_unit.v"

    hardwareInstance
        tag
        _pu
        UnitEnv
            { sigClk
            , ctrlPorts = Just ComparePorts{}
            , valueIn = Just (dataIn, attrIn)
            , valueOut = Just (dataOut, _)
            } =
            [__i|pu_compare #{ tag } (
            .clk(#{ sigClk }),
            .a(#{ dataIn }),
            .b(#{ attrIn }),
            .op(#{ attrIn }),
            .result(#{ dataOut })
            );
        |]
    hardwareInstance _title _pu _env = error "internal error"

instance Ord t => WithFunctions (Compare v x t) (F v x) where
    functions Compare{process_, remain, currentWork} =
        functions process_
            ++ remain
            ++ maybeToList currentWork
