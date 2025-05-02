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
import qualified Data.Set as S
import Data.String.ToString
import qualified NITTA.Intermediate.Functions as F
import NITTA.Intermediate.Types
import NITTA.Model.ProcessorUnits.Types

import Control.Monad (when)
import Data.Bits hiding (bit)
import Data.Foldable
import Data.Maybe
import Data.String.Interpolate
import qualified Data.Text as T
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
        { oePort :: SignalTag
        , wrPort :: SignalTag
        , opSelPort :: [SignalTag]
        }
        deriving (Show)

supportedOpsNum :: Integer
supportedOpsNum = 5
selWidth = ceiling (logBase 2 (fromIntegral supportedOpsNum) :: Double) :: Int

instance Controllable (Compare v x t) where
    data Instruction (Compare v x t)
        = Load F.Op
        | Out
        deriving (Show)

    data Microcode (Compare v x t) = Microcode
        { oe :: Bool
        , wr :: Bool
        , opSel :: Int -- todo to enum
        }
        deriving (Show, Eq)

    zipSignalTagsAndValues ComparePorts{..} Microcode{..} =
        [ (oePort, Bool oe)
        , (wrPort, Bool wr)
        ]
            ++ zipWith (\tag bit -> (tag, Bool bit)) opSelPort (bits opSel selWidth)
        where
            bits val localWidth = [testBit val (localWidth - idx - 1) | idx <- [0 .. localWidth - 1]]
    usedPortTags ComparePorts{oePort, wrPort, opSelPort} = oePort : wrPort : opSelPort

    takePortTags (oe : wr : xs) _ = ComparePorts oe wr sel
        where
            sel = take selWidth xs
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
            , opSel = 0
            }

instance UnambiguouslyDecode (Compare v x t) where
    decodeInstruction Out = def{oe = True}
    decodeInstruction (Load op) = case op of
        F.CMP_EQ -> def{opSel = 0, wr = True}
        F.CMP_LT -> def{opSel = 1, wr = True}
        F.CMP_LTE -> def{opSel = 2, wr = True}
        F.CMP_GT -> def{opSel = 3, wr = True}
        F.CMP_GTE -> def{opSel = 4, wr = True}

instance Default x => DefaultX (Compare v x t) x

instance Time t => Default (Compare v x t) where
    def = compare

instance VarValTime v x t => EndpointProblem (Compare v x t) v t where
    endpointOptions pu@Compare{targets}
        | not $ null targets =
            let at = nextTick pu ... maxBound
                duration = 1 ... maxBound
             in [EndpointSt (Target $ head targets) $ TimeConstraint at duration]
    endpointOptions Compare{sources, currentWork = Just f, process_}
        | not $ null sources =
            let doneAt = inputsPushedAt process_ f + 3
                at = max doneAt (nextTick process_) ... maxBound
                duration = 1 ... maxBound
             in [EndpointSt (Source $ S.fromList sources) $ TimeConstraint at duration]
    endpointOptions pu@Compare{remain} = concatMap (\f -> (endpointOptions . execution pu) f) remain

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
            , remain = filter (/= f) remain
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

instance VarValTime v x t => TargetSystemComponent (Compare v x t) where
    moduleName _ _ = T.pack "pu_compare"
    software _ _ = Empty
    hardware _tag _pu = FromLibrary "pu_compare.v"

    hardwareInstance
        tag
        _pu
        UnitEnv
            { sigClk
            , sigRst
            , ctrlPorts = Just ComparePorts{..}
            , valueIn = Just (dataIn, attrIn)
            , valueOut = Just (dataOut, attrOut)
            } =
            [__i|
            pu_compare \#
                ( .DATA_WIDTH( #{ dataWidth (def :: x) } )
                , .ATTR_WIDTH( #{ attrWidth (def :: x) } )
                , .SEL_WIDTH( #{ selWidth } )
                ) #{ tag } (
            .clk(#{ sigClk }),
            .rst( #{ sigRst } ),
            .oe(#{ oePort }),
            .wr(#{ wrPort }),
            .op_sel({ #{ T.intercalate (T.pack ", ") $ map showText opSelPort } })

            , .data_in( #{ dataIn } )
            , .attr_in( #{ attrIn } )
            , .data_out( #{ dataOut } )
            , .attr_out( #{ attrOut } )
            );
        |]
    hardwareInstance _title _pu _env = error "internal error"

instance Ord t => WithFunctions (Compare v x t) (F v x) where
    functions Compare{process_, remain, currentWork} =
        functions process_
            ++ remain
            ++ maybeToList currentWork

instance VarValTime v x t => Testable (Compare v x t) v x where
    testBenchImplementation prj@Project{pName, pUnit} =
        let tbcSignalsConst = [T.pack "oe", T.pack "wr", T.pack $ "[" ++ show (selWidth - 1) ++ ":0] op_sel"]
            showMicrocode Microcode{oe, wr, opSel} =
                [i|oe <= #{ bool2verilog oe };|]
                    <> [i| wr <= #{ bool2verilog wr };|]
                    <> [i| op_sel <= #{ show opSel };|]
         in Immediate (toString $ moduleName pName pUnit <> T.pack "_tb.v") $
                snippetTestBench
                    prj
                    SnippetTestBenchConf
                        { tbcSignals = tbcSignalsConst
                        , tbcPorts =
                            ComparePorts
                                { oePort = SignalTag (T.pack "oe")
                                , wrPort = SignalTag (T.pack "wr")
                                , opSelPort =
                                    [ (SignalTag . T.pack) ("op_sel[" <> show p <> "]")
                                    | p <- [selWidth - 1, selWidth - 2 .. 0]
                                    ]
                                }
                        , tbcMC2verilogLiteral = showMicrocode
                        }
