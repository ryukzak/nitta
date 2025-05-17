{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module NITTA.Model.ProcessorUnits.LogicalUnit (
    LogicalUnit (..),
    logicalUnit,
    Ports (LUTPorts),
    IOPorts (..),
)
where

import Control.Monad (when)
import Data.Bits (Bits (testBit))
import Data.Default (Default, def)
import Data.Foldable as DF (Foldable (null), find)
import Data.List (elemIndex, partition, (\\))
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
import Data.String.Interpolate
import Data.String.ToString
import qualified Data.Text as T
import Data.Typeable (Typeable)
import qualified NITTA.Intermediate.Functions as F
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits.Types
import NITTA.Model.Time
import NITTA.Project
import NITTA.Utils
import NITTA.Utils.ProcessDescription
import Numeric.Interval.NonEmpty hiding (elem, notElem)
import Prettyprinter

data LogicalUnit v x t = LogicalUnit
    { remain :: [F v x]
    , targets :: [v]
    , sources :: [v]
    , currentWork :: Maybe (F v x)
    , lutFunctions :: [F v x]
    , selBitNum :: Int
    , maxNumArgs :: Int
    , process_ :: Process t (StepInfo v x t)
    }
    deriving (Typeable)

logicalUnit :: Time t => LogicalUnit v x t
logicalUnit =
    LogicalUnit
        { remain = []
        , targets = []
        , sources = []
        , lutFunctions = []
        , currentWork = Nothing
        , selBitNum = 4
        , maxNumArgs = 16
        , process_ = def
        }

instance VarValTime v x t => Pretty (LogicalUnit v x t) where
    pretty LogicalUnit{remain, targets, sources, currentWork, lutFunctions, process_} =
        [__i|
            LogicalUnit:
                remain: #{ remain }
                targets: #{ map toString targets }
                sources: #{ map toString sources }
                currentWork: #{ currentWork }
                lutFunctions: #{ lutFunctions }
                #{ nest 4 $ pretty process_ }
            |]

instance VarValTime v x t => Show (LogicalUnit v x t) where
    show = show . pretty

instance Default (Microcode (LogicalUnit v x t)) where
    def =
        Microcode
            { oeSignal = False
            , wrSignal = False
            , selSignal = Nothing
            }

instance Connected (LogicalUnit v x t) where
    data Ports (LogicalUnit v x t) = LUTPorts
        { oe :: SignalTag
        , wr :: SignalTag
        , sel :: [SignalTag]
        }
        deriving (Show)

instance IOConnected (LogicalUnit v x t) where
    data IOPorts (LogicalUnit v x t) = LUTIO
        deriving (Show)

selWidth :: LogicalUnit v x t -> Int
selWidth l = calcSelWidth (length (lutFunctions l))
calcSelWidth n = max 1 $ ceiling (logBase (2 :: Double) (fromIntegral $ max 1 n))

getFunctionIndex :: LogicalUnit v x t -> Int
getFunctionIndex LogicalUnit{currentWork, lutFunctions} =
    fromMaybe (-1) (currentWork >>= \cw -> elemIndex cw lutFunctions)

instance Controllable (LogicalUnit v x t) where
    data Instruction (LogicalUnit v x t)
        = Load
        | Out Int
        deriving (Show)

    data Microcode (LogicalUnit v x t) = Microcode
        { oeSignal :: Bool
        , wrSignal :: Bool
        , selSignal :: Maybe Int
        }
        deriving (Show, Eq, Ord)

    zipSignalTagsAndValues LUTPorts{..} Microcode{..} =
        [ (oe, Bool oeSignal)
        , (wr, Bool wrSignal)
        ]
            ++ sel'
        where
            sel' =
                map
                    ( \(linkId, ix) ->
                        ( linkId
                        , maybe Undef (Bool . (`testBit` ix)) selSignal
                        )
                    )
                    $ zip (reverse sel) [0 ..]

    usedPortTags LUTPorts{oe, wr, sel} = oe : wr : sel

    takePortTags (oe : wr : xs) l = LUTPorts oe wr sel
        where
            sel = take (selBitNum l) xs
    takePortTags _ _ = error "can not take port tags, tags are over"

instance UnambiguouslyDecode (LogicalUnit v x t) where
    decodeInstruction Load = def{wrSignal = True}
    decodeInstruction (Out op) = def{oeSignal = True, selSignal = Just op}

softwareFile tag pu = moduleName tag pu <> T.pack "." <> tag <> T.pack ".dump"
maxArgsLen LogicalUnit{lutFunctions} =
    if null lutFunctions
        then 0
        else maximum [S.size (inputs f) | F f _ <- lutFunctions]

maxAddrLen pu = maxArgsLen pu + selBitNum pu

instance VarValTime v x t => TargetSystemComponent (LogicalUnit v x t) where
    moduleName _title _pu = T.pack "pu_lut"
    hardware _tag _pu = FromLibrary "pu_lut.v"

    software tag pu@LogicalUnit{lutFunctions, selBitNum} =
        let
            entries = concatMap getLutEntries (zip [0 ..] lutFunctions)
            memoryDump = T.unlines $ map (T.pack . padEntry (maxAddrLen pu)) entries
         in
            Immediate (toString $ softwareFile tag pu) memoryDump
        where
            getLutEntries (funcIdx, f)
                | Just (F.TruthTable lutMap _ (O _)) <- castF f =
                    let
                        selBits = intToBits selBitNum funcIdx
                        numArgs = maybe 0 length (listToMaybe $ M.keys lutMap)
                        totalCombinations = 2 ^ maxArgsLen pu
                        existingCombinations = M.size lutMap
                        missingCount = totalCombinations - existingCombinations
                     in
                        map
                            ( \(inp, out) ->
                                ( boolToBits (selBits ++ inp)
                                , if out then '1' else '0'
                                )
                            )
                            (M.toList lutMap)
                            ++ replicate
                                missingCount
                                ( boolToBits selBits ++ replicate numArgs '0'
                                , '0'
                                )
                | otherwise = []

            intToBits :: Int -> Int -> [Bool]
            intToBits wdth n = [testBit n i' | i' <- [wdth - 1, wdth - 2 .. 0]]

            boolToBits = map (\b -> if b then '1' else '0')
            padEntry len (addr, out) = addr ++ replicate (len - length addr) '0' ++ [out]

    hardwareInstance
        tag
        _pu
        UnitEnv
            { sigClk
            , ctrlPorts = Just LUTPorts{..}
            , valueIn = Just (dataIn, attrIn)
            , valueOut = Just (dataOut, attrOut)
            } =
            [__i|
            pu_lut \#
                    ( .ATTR_WIDTH( #{ attrWidth (def :: x) } )
                    , .DATA_WIDTH( #{ dataWidth (def :: x) } )
                    , .SEL_WIDTH( #{ (selBitNum _pu)} )
                    , .MAX_NUM_ARGS( #{ maxArgsLen _pu } )
                    , .LUT_DUMP( "{{ impl.paths.nest }}/#{ softwareFile tag _pu }" )
                    ) #{ tag }
                ( .clk( #{ sigClk } )

                , .signal_oe( #{ oe } )
                , .signal_wr( #{ wr } )
                , .signal_sel( { #{ T.intercalate (T.pack ", ") $ map showText sel } } )

                , .data_in( #{ dataIn } )
                , .attr_in( #{ attrIn } )
                , .data_out( #{ dataOut } )
                , .attr_out( #{ attrOut } )
                );
        |]
    hardwareInstance _title _pu _env = error "internal error"

instance VarValTime v x t => ProcessorUnit (LogicalUnit v x t) v x t where
    tryBind f pu@LogicalUnit{remain, lutFunctions}
        | Just F.TruthTable{} <- castF f = Right pu{remain = f : remain ++ remain, lutFunctions = f : lutFunctions}
        | Just F.LogicAnd{} <- castF f = Right pu{remain = f : remain, lutFunctions = f : lutFunctions}
        | Just F.LogicOr{} <- castF f = Right pu{remain = f : remain, lutFunctions = f : lutFunctions}
        | Just F.LogicNot{} <- castF f = Right pu{remain = f : remain, lutFunctions = f : lutFunctions}
        | otherwise = Left $ "The function is unsupported by LogicalUnit: " ++ show f
    process = process_

execution :: LogicalUnit v x t -> F v x -> LogicalUnit v x t
execution pu@LogicalUnit{targets = [], sources = [], remain} f =
    pu
        { remain = filter (/= f) remain
        , currentWork = Just f
        , targets = S.elems $ inputs f
        , sources = S.elems $ outputs f
        }
execution _ _ = error "LogicalUnit: internal execution error."

instance VarValTime v x t => EndpointProblem (LogicalUnit v x t) v t where
    endpointOptions pu@LogicalUnit{targets}
        | not $ DF.null targets =
            let at = nextTick pu ... maxBound
                duration = 1 ... maxBound
             in map (\v -> EndpointSt (Target v) $ TimeConstraint at duration) targets
    endpointOptions LogicalUnit{sources, currentWork = Just f, process_}
        | not $ DF.null sources =
            let doneAt = inputsPushedAt process_ f + 3
                at = max doneAt (nextTick process_) ... maxBound
                duration = 1 ... maxBound
                allSources = sources
             in [EndpointSt (Source $ S.fromList allSources) $ TimeConstraint at duration]
    endpointOptions pu@LogicalUnit{remain} = concatMap (endpointOptions . execution pu) remain

    endpointDecision pu@LogicalUnit{targets} d@EndpointSt{epRole = Target v, epAt}
        | not $ null targets
        , let allTargets = targets
        , ([_], targets') <- partition (== v) allTargets
        , let process_' = execSchedule pu $ do
                scheduleEndpoint d $ scheduleInstructionUnsafe epAt Load =
            pu
                { targets = targets'
                , process_ = process_'
                }
    endpointDecision pu@LogicalUnit{targets = [], sources, currentWork = Just f, process_} d@EndpointSt{epRole = Source v, epAt}
        | not $ null sources
        , let allSources = sources
        , let sources' = allSources \\ S.elems v
        , sources' /= allSources
        , let a = inf $ stepsInterval $ relatedEndpoints process_ $ variables f
        , let process_' = execSchedule pu $ do
                endpoints <- scheduleEndpoint d $ scheduleInstructionUnsafe epAt (Out (getFunctionIndex pu))
                when (null sources') $ do
                    scheduleFunctionFinish_ [] f $ a ... sup epAt
                return endpoints =
            pu
                { sources = sources'
                , process_ = process_'
                , currentWork = Just f
                }
    endpointDecision pu@LogicalUnit{targets = [], sources = [], remain} d
        | let v = oneOf $ variables d
        , Just f <- find (\f -> v `S.member` variables f) remain =
            endpointDecision (execution pu f) d
    endpointDecision pu d = error [i|incorrect decision #{ d } for #{ pretty pu }|]

instance Ord t => WithFunctions (LogicalUnit v x t) (F v x) where
    functions LogicalUnit{process_, remain, currentWork} =
        functions process_
            ++ remain
            ++ maybeToList currentWork

instance BreakLoopProblem (LogicalUnit v x t) v x

instance ConstantFoldingProblem (LogicalUnit v x t) v x

instance OptimizeAccumProblem (LogicalUnit v x t) v x

instance OptimizeLutProblem (LogicalUnit v x t) v x

instance ResolveDeadlockProblem (LogicalUnit v x t) v x

instance Var v => Locks (LogicalUnit v x t) v where
    locks LogicalUnit{remain, sources, targets} =
        [ Lock{lockBy, locked}
        | locked <- sources
        , lockBy <- targets
        ]
            ++ [ Lock{lockBy, locked}
               | locked <- concatMap (S.elems . variables) remain
               , lockBy <- sources ++ targets
               ]
            ++ concatMap locks remain

instance IOTestBench (LogicalUnit v x t) v x

instance Default x => DefaultX (LogicalUnit v x t) x

instance Time t => Default (LogicalUnit v x t) where
    def = logicalUnit

instance VarValTime v x t => Testable (LogicalUnit v x t) v x where
    testBenchImplementation prj@Project{pName, pUnit} =
        let lutDef :: LogicalUnit v x t
            lutDef = def
            tbcSignalsConst = [T.pack "oe", T.pack "wr", T.pack $ "[" ++ show (selBitNum lutDef - 1) ++ ":0] sel"]
            showMicrocode Microcode{oeSignal, wrSignal, selSignal} =
                [i|oe <= #{ bool2verilog oeSignal };|]
                    <> [i| wr <= #{ bool2verilog wrSignal };|]
                    <> case selSignal of
                        Just sel -> [i| sel <= #{ selWidth lutDef }'d#{ sel };|]
                        Nothing -> [i| sel <= {#{ selWidth lutDef }{1'bx}};|]
         in Immediate (toString $ moduleName pName pUnit <> T.pack "_tb.v") $
                snippetTestBench
                    prj
                    SnippetTestBenchConf
                        { tbcSignals = tbcSignalsConst
                        , tbcPorts =
                            LUTPorts
                                { oe = SignalTag (T.pack "oe")
                                , wr = SignalTag (T.pack "wr")
                                , sel =
                                    [ (SignalTag . T.pack) ("sel[" <> show p <> "]")
                                    | p <- [selBitNum lutDef - 1, selBitNum lutDef - 2 .. 0]
                                    ]
                                }
                        , tbcMC2verilogLiteral = showMicrocode
                        }
