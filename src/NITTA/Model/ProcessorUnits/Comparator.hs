{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : NITTA.Model.ProcessorUnits.Comparator
Description : A comparator that supports operations: <, <=, >, >=, ==
Copyright   : (c) Boris Novoselov, 2025
License     : BSD3
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Comparator (
    Comparator,
    compare,
    Ports (..),
    IOPorts (..),
) where

import Control.Monad (when)
import Data.Bits hiding (bit)
import Data.Data (dataTypeConstrs, dataTypeOf)
import Data.Default (Default, def)
import Data.Foldable
import Data.List (partition, (\\))
import Data.Maybe
import Data.Set qualified as S
import Data.String.Interpolate
import Data.String.ToString
import Data.Text qualified as T
import NITTA.Intermediate.Functions qualified as F
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits.Types
import NITTA.Model.Time
import NITTA.Project
import NITTA.Utils
import NITTA.Utils.ProcessDescription
import Numeric.Interval.NonEmpty hiding (elem, notElem)
import Prettyprinter
import Prelude hiding (compare)

data Comparator v x t = Comparator
    { remain :: [F v x]
    , targets :: [v]
    , sources :: [v]
    , currentWork :: Maybe (F v x)
    , process_ :: Process t (StepInfo v x t)
    }

compare :: Time t => Comparator v x t
compare =
    Comparator
        { remain = []
        , targets = []
        , sources = []
        , currentWork = Nothing
        , process_ = def
        }

instance VarValTime v x t => ProcessorUnit (Comparator v x t) v x t where
    tryBind f pu@Comparator{remain}
        | Just F.Compare{} <- castF f =
            Right
                pu
                    { remain = f : remain
                    }
        | otherwise = Left "Unsupported function type for Comparator"

    process = process_

instance Connected (Comparator v x t) where
    data Ports (Comparator v x t) = ComparePorts
        { oePort :: SignalTag
        , wrPort :: SignalTag
        , opSelPort :: [SignalTag]
        }
        deriving (Show)

supportedOpsNum :: Int
supportedOpsNum = fromIntegral $ length (dataTypeConstrs $ dataTypeOf F.CmpEq)
selWidth = ceiling (logBase 2 (fromIntegral supportedOpsNum) :: Double) :: Int

instance Controllable (Comparator v x t) where
    data Instruction (Comparator v x t)
        = Load F.CmpOp
        | Out
        deriving (Show)

    data Microcode (Comparator v x t) = Microcode
        { oe :: Bool
        , wr :: Bool
        , opSel :: Int
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

instance Var v => Locks (Comparator v x t) v where
    locks Comparator{remain, sources, targets} =
        [ Lock{lockBy, locked}
        | locked <- sources
        , lockBy <- targets
        ]
            ++ [ Lock{lockBy, locked}
               | locked <- concatMap (S.elems . variables) remain
               , lockBy <- sources ++ targets
               ]
            ++ concatMap locks remain
instance Default (Microcode (Comparator v x t)) where
    def =
        Microcode
            { wr = False
            , oe = False
            , opSel = 0
            }

instance UnambiguouslyDecode (Comparator v x t) where
    decodeInstruction Out = def{oe = True}
    decodeInstruction (Load op) = case op of
        F.CmpEq -> def{opSel = 0, wr = True}
        F.CmpLt -> def{opSel = 1, wr = True}
        F.CmpLte -> def{opSel = 2, wr = True}
        F.CmpGt -> def{opSel = 3, wr = True}
        F.CmpGte -> def{opSel = 4, wr = True}

instance Default x => DefaultX (Comparator v x t) x

instance Time t => Default (Comparator v x t) where
    def = compare

flipCmpOp :: F.CmpOp -> F.CmpOp
flipCmpOp F.CmpEq = F.CmpEq
flipCmpOp F.CmpLt = F.CmpGt
flipCmpOp F.CmpLte = F.CmpGte
flipCmpOp F.CmpGt = F.CmpLt
flipCmpOp F.CmpGte = F.CmpLte

instance VarValTime v x t => EndpointProblem (Comparator v x t) v t where
    endpointOptions pu@Comparator{targets = target : _} =
        [EndpointSt (Target target) $ TimeConstraint at duration]
        where
            at = nextTick pu ... maxBound
            duration = 1 ... maxBound
    endpointOptions
        pu@Comparator
            { sources = _ : _
            , currentWork = Just f
            , process_
            } = [EndpointSt (Source $ S.fromList (sources pu)) $ TimeConstraint at duration]
            where
                doneAt = inputsPushedAt process_ f + 3
                at = max doneAt (nextTick process_) ... maxBound
                duration = 1 ... maxBound
    endpointOptions pu@Comparator{remain} =
        concatMap (endpointOptions . execution pu) remain

    endpointDecision pu@Comparator{targets, currentWork} d@EndpointSt{epRole = Target v, epAt}
        | not $ null targets
        , ([_], targets') <- partition (== v) targets
        , --  Computation process planning is carried out.
          let process_' = execSchedule pu $ do
                -- this is required for correct work of automatically generated tests,
                -- that takes information about time from Process
                case currentWork of
                    Just f
                        | Just (F.Compare op (I a) (I _) _) <- castF f ->
                            let adjustedOp = if v == a then op else flipCmpOp op
                             in scheduleEndpoint d $ scheduleInstructionUnsafe epAt (Load adjustedOp)
                        | otherwise -> error "Unsupported function type for Comparator"
                    Nothing -> error "cmpOp is Nothing" =
            pu
                { process_ = process_'
                , -- The remainder of the work is saved for the next loop
                  targets = targets'
                }
    endpointDecision pu@Comparator{targets = [], sources, currentWork = Just f, process_} d@EndpointSt{epRole = Source v, epAt}
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
    endpointDecision pu@Comparator{targets = [], sources = [], remain} d
        | let v = oneOf $ variables d
        , Just f <- find (\f -> v `S.member` variables f) remain =
            endpointDecision (execution pu f) d
    endpointDecision pu d = error [i|incorrect decision #{ d } for #{ pretty pu }|]

execution pu@Comparator{targets = [], sources = [], remain} f
    | Just (F.Compare _ (I a) (I b) (O c)) <- castF f =
        pu
            { targets = [a, b]
            , currentWork = Just f
            , sources = S.elems c
            , remain = filter (/= f) remain
            }
execution _ f =
    error $
        "Comparator: internal execution error. Expected Compare, got: " ++ show f

instance VarValTime v x t => Pretty (Comparator v x t) where
    pretty Comparator{remain, targets, sources, currentWork, process_} =
        [__i|
            Comparator:
                remain: #{ remain }
                targets: #{ map toString targets }
                sources: #{ map toString sources }
                currentWork: #{ currentWork }
                #{ nest 4 $ pretty process_ }
            |]

instance IOConnected (Comparator v x t) where
    data IOPorts (Comparator v x t) = CompareIO
        deriving (Show)

instance BreakLoopProblem (Comparator v x t) v x

instance ConstantFoldingProblem (Comparator v x t) v x

instance OptimizeAccumProblem (Comparator v x t) v x

instance ResolveDeadlockProblem (Comparator v x t) v x

instance IOTestBench (Comparator v x t) v x

instance VarValTime v x t => TargetSystemComponent (Comparator v x t) where
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

instance Ord t => WithFunctions (Comparator v x t) (F v x) where
    functions Comparator{process_, remain, currentWork} =
        functions process_
            ++ remain
            ++ maybeToList currentWork

instance VarValTime v x t => Testable (Comparator v x t) v x where
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
