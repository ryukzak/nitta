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
import NITTA.Intermediate.Functions qualified as F
import NITTA.Intermediate.Types
import NITTA.Model.ProcessorUnits.Types

import Control.Monad (when)
import Data.Foldable
import Data.Maybe
import Data.String.Interpolate
import Data.Text qualified as T
import Debug.Trace
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
    trace "D14" $
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
            trace "D1" $
                Right
                    pu
                        { remain = trace ("f = " ++ show f) $ f : remain
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

supportedOpsNum = 5
selWidth = ceiling (logBase 2 (fromIntegral supportedOpsNum))

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
        trace "DD1" $
            [ (oePort, Bool oe)
            , (wrPort, Bool wr)
            ]
                ++ zipWith (\tag idx -> (tag, Bool (idx == opSel))) opSelPort [0 ..]
    usedPortTags ComparePorts{oePort, wrPort, opSelPort} = trace "D2" $ oePort : wrPort : opSelPort

    takePortTags (oe : wr : xs) _ = trace "D3" $ ComparePorts oe wr sel
        where
            sel = take selWidth xs
    takePortTags _ _ = error "can not take port tags, tags are over"

instance Var v => Locks (Compare v x t) v where
    locks _ = []
instance Default (Microcode (Compare v x t)) where
    def =
        Microcode
            { wr = False
            , oe = False
            , opSel = 0
            }

instance UnambiguouslyDecode (Compare v x t) where
    decodeInstruction Out = def{oe = True}
    decodeInstruction (Load op) = trace "D5" $ case op of
        F.CEQ -> def{opSel = 0, wr = True}
        F.CLT -> def{opSel = 1, wr = True}
        F.CLTE -> def{opSel = 2, wr = True}
        F.CGT -> def{opSel = 3, wr = True}
        F.CGTE -> def{opSel = 4, wr = True}

instance Default x => DefaultX (Compare v x t) x

instance Time t => Default (Compare v x t) where
    def = compare

instance VarValTime v x t => EndpointProblem (Compare v x t) v t where
    endpointOptions pu@Compare{targets}
        | not $ null targets =
            trace "D6" $
                let at = nextTick pu ... maxBound
                    duration = 1 ... maxBound
                 in map (\v -> EndpointSt (Target v) $ TimeConstraint at duration) targets
    endpointOptions Compare{sources, currentWork = Just f, process_}
        | not $ null sources =
            trace "D7" $
                let doneAt = inputsPushedAt process_ f + 3
                    at = max doneAt (nextTick process_) ... maxBound
                    duration = 1 ... maxBound
                 in [EndpointSt (Source $ S.fromList sources) $ TimeConstraint at duration]
    endpointOptions pu@Compare{remain} = trace "D8:" $ concatMap (\f -> trace ("  f = " ++ show f) $ (endpointOptions . execution pu) f) remain

    -- endpointOptions pu@Compare{remain}
    --     | null remain = []
    --     | otherwise =
    --         concatMap
    --             ( \f ->
    --                 if f `elem` remain
    --                     then endpointOptions (execution pu f)
    --                     else error "Function not in remain"
    --             )
    --             remain
    -- endpointOptions pu@Compare{remain} = trace "D8:" $ concatMap (\f -> trace ("  f = " ++ show f) $ (endpointOptions . execution pu) f) remain

    endpointDecision pu@Compare{targets, cmpOp} d@EndpointSt{epRole = Target v, epAt}
        | not $ null targets
        , ([_], targets') <- trace "D9" $ partition (== v) targets
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
        , let sources' = trace "D10" $ sources \\ S.elems v
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
        , Just f <- trace "D11" $ find (\f -> v `S.member` variables f) remain =
            endpointDecision (execution pu f) d
    endpointDecision pu d = error [i|incorrect decision #{ d } for #{ pretty pu }|]

execution pu@Compare{targets = [], sources = [], remain} f
    | Just (F.LogicCompare op (I a) (I b) (O c)) <- castF f =
        trace ("Executing " ++ show f) $
            pu
                { targets = [a, b]
                , currentWork = Just f
                , sources = S.elems c
                , remain = filter (/= f) remain -- remain \\ [f]
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
            , valueOut = Just (dataOut, attrOut)
            } =
            [__i|pu_compare #{ tag } (
            .clk(#{ sigClk }),
            .oe(#{ dataIn }),
            .wr(#{ attrIn }),
            .opSel(#{ attrIn }),

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
