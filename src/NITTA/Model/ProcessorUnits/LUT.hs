{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}

module NITTA.Model.ProcessorUnits.LUT (
    LUT(..),
    lut,
    Ports(LUTPorts),
    IOPorts (..),
) where


import qualified NITTA.Intermediate.Functions as F
import Data.Typeable (Typeable)
import NITTA.Intermediate.Types
import NITTA.Model.ProcessorUnits.Types
import NITTA.Model.Time
import NITTA.Project
import Data.String.Interpolate
import Data.Default (Default, def)
import Data.Maybe
import Prettyprinter
import Data.String.ToString
import NITTA.Model.Problems.Endpoint
import qualified Data.Set as S
import Numeric.Interval.NonEmpty 
import Data.Foldable as DF ( Foldable(null), find )
import NITTA.Utils
import NITTA.Utils.ProcessDescription
import Data.List ((\\), partition)
import Control.Monad (when)
import Data.Text qualified as T
import NITTA.Model.Problems

data LUT v x t = LUT
    {remain :: [F v x]
    , targets :: [v]
    , sources :: [v]
    , currentWork :: Maybe (F v x)
    , process_  :: Process t (StepInfo v x t)
    } deriving (Typeable)

lut :: ( Time t) => LUT v x t
lut =
    LUT
        { remain = []
        , targets = []
        , sources = []
        , currentWork = Nothing
        , process_ = def
        }

instance VarValTime v x t => Pretty (LUT v x t) where
    pretty LUT{remain, targets, sources, currentWork, process_} =
        [__i|
            LUT:
                remain: #{ remain }
                targets: #{ map toString targets }
                sources: #{ map toString sources }
                currentWork: #{ currentWork }
                #{ nest 4 $ pretty process_ }
            |]

instance Default (Microcode (LUT v x t)) where
    def =
        Microcode
            { wrSignal = False
            , oeSignal = False
            , selSignal = []
            }

instance Connected (LUT v x t) where
    data Ports (LUT v x t) = LUTPorts
        { wr :: SignalTag
        , oe :: SignalTag
        , sel :: [SignalTag]
        }
        deriving (Show)

instance IOConnected (LUT v x t) where
    data IOPorts (LUT v x t) = LUTIO
        deriving (Show)

instance Controllable (LUT v x t) where
    data Instruction (LUT v x t)
        = Load
        | Out
        deriving (Show)

    data Microcode (LUT v x t) = Microcode
        { -- \| Write to mUnit signal.
          wrSignal :: Bool
        , -- \| Downloading from mUnit signal.
          oeSignal :: Bool
        , -- \| Function selector signal.
          selSignal :: [Bool]
        }
        deriving (Show, Eq, Ord)

    zipSignalTagsAndValues LUTPorts{..} Microcode{..} =
        [ (wr, Bool wrSignal)
        , (oe, Bool oeSignal)
        ] 
        ++ zip sel (map Bool selSignal)

    usedPortTags LUTPorts{wr, oe} = [wr, oe]

    takePortTags (wr : oe : sel ) _ = LUTPorts wr oe sel
    takePortTags _ _  = error "can not take port tags, tags are over"

instance UnambiguouslyDecode (LUT v x t) where
    decodeInstruction Load = def{wrSignal = True}
    decodeInstruction Out = def{oeSignal = True}


--softwareFile tag pu = moduleName tag pu <> T.pack "." <> tag <> T.pack ".dump"

instance Val x => TargetSystemComponent (LUT v x t) where

    hardware _tag _pu = FromLibrary "pu_lut.v"
    moduleName _title _pu = T.pack "pu_lut"

    software _ _ = Empty
    hardwareInstance
        tag
        _pu
        UnitEnv
            { 
            sigClk
            , ctrlPorts = Just LUTPorts{..}
            , valueIn = Just dataIn
            , valueOut = Just dataOut
            } =
        [__i|
            pu_lut \#
                    ( .ADDR_WIDTH( #{ attrWidth (def :: x) } )
                    , .DATA_WIDTH( #{ dataWidth (def :: x) } )
                    , .SEL_WIDTH( #{ attrWidth (def :: x) } )
                    , .LUT_DUMP( "{{ impl.paths.nest }}/#{"dump/lut.hex"}" )
                    ) #{ tag }
                ( .clk( #{ sigClk } )
                , .addr( #{ dataIn } )
                , .data( #{ dataOut } )
                , .wr( #{ wr } )
                , .oe( #{ oe } )
                , .sel( #{ sel } )
                );
        |] -- todo fix path LUT_DUMP with toString $ softwareFile tag lut
    hardwareInstance _title _pu _env = error "internal error"

-- instance VarValTime v x t => Testable (Multiplier v x t) v x where
--     testBenchImplementation prj@Project{pName, pUnit} =
--         Immediate (toString $ moduleName pName pUnit <> "_tb.v") $
--             snippetTestBench
--                 prj
--                 SnippetTestBenchConf
--                     { -- List of control signals. It is needed to initialize
--                       -- registers with the same names.
--                       tbcSignals = ["oe", "wr"]
--                     , -- A processor unit connects to the environment by signal
--                       -- lines. In 'NITTA.Project.TestBench.tbcPorts'
--                       -- describes IDs signal lines of testbench. In
--                       -- 'NITTA.Project.TestBench.tbcSignalConnect' how
--                       -- abstract numbers are translate to source code.
--                       tbcPorts =
--                         LUTPorts
--                             { oe = SignalTag "oe"
--                             , wr = SignalTag "wr"
--                             }
--                     , -- Map microcode to registers in the testbench.
--                       tbcMC2verilogLiteral = \Microcode{oeSignal, wrSignal} ->
--                         [i|oe <= #{bool2verilog oeSignal};|]
--                             <> [i| wr <= #{bool2verilog wrSignal};|]
--                     }

instance VarValTime v x t => ProcessorUnit (LUT v x t) v x t where
    tryBind f pu@LUT{remain}
        | Just F.LUT{} <- castF f = Right pu{remain = f : remain}
        | Just F.LogicAnd{} <- castF f = Right pu{remain = f : remain}
        | Just F.LogicOr{} <- castF f = Right pu{remain = f : remain}
        | Just F.LogicNot{} <- castF f = Right pu{remain = f : remain}
        | otherwise = Left $ "The function is unsupported by LUT: " ++ show f
    process = process_

execution :: LUT v x t -> F v x -> LUT v x t
execution pu@LUT{targets = [], sources = [], remain} f =
    pu
        { remain = filter (/= f) remain
        , currentWork = Just f
        , targets = S.elems $ inputs f
        , sources = S.elems $ outputs f
        }
execution _ _ = error "LUT: internal execution error."

instance VarValTime v x t => EndpointProblem (LUT v x t) v t where
    endpointOptions pu@LUT{targets}
        | not $ DF.null targets =
            let at = nextTick pu ... maxBound
                duration = 1 ... maxBound
            in map (\v -> EndpointSt (Target v) $ TimeConstraint at duration) targets
    endpointOptions LUT{sources, currentWork = Just f, process_}
        | not $ DF.null sources =
            let doneAt = inputsPushedAt process_ f + 3
                at = max doneAt (nextTick process_) ... maxBound
                duration = 1 ... maxBound
            in [EndpointSt (Source $ S.fromList sources) $ TimeConstraint at duration]
    endpointOptions pu@LUT{remain} = concatMap (endpointOptions . execution pu) remain

    endpointDecision pu@LUT{targets} d@EndpointSt{epRole = Target v, epAt}
        | not $ null targets
        , ([_], targets') <- partition (== v) targets
        , let process_' = execSchedule pu $ do
                scheduleEndpoint d $ scheduleInstructionUnsafe epAt Load =
            pu
            { targets = targets'
            , process_ = process_'
            }
    endpointDecision pu@LUT{targets = [], sources, currentWork = Just f, process_} d@EndpointSt{epRole = Source v, epAt}
        | not $ null sources
        , let sources' = sources \\ S.elems v
        , sources' /= sources
        , let a = inf $ stepsInterval $ relatedEndpoints process_ $ variables f
        , let process_' = execSchedule pu $ do
                endpoints <- scheduleEndpoint d $ scheduleInstructionUnsafe epAt Out
                when (null sources') $ do
                    scheduleFunctionFinish_ [] f $ a ... sup epAt
                return endpoints =
        pu
            { sources = sources'
            , process_ = process_'
            , currentWork = if null sources' then Nothing else Just f
            }
    endpointDecision pu@LUT{targets = [], sources = [], remain} d
        | let v = oneOf $ variables d
        , Just f <- find (\f -> v `S.member` variables f) remain =
            endpointDecision (execution pu f) d
    endpointDecision pu d = error [i|incorrect decision #{ d } for #{ pretty pu }|]

instance Ord t => WithFunctions (LUT v x t) (F v x) where
    functions LUT{process_, remain, currentWork} =
        functions process_
            ++ remain
            ++ maybeToList currentWork

instance BreakLoopProblem (LUT v x t) v x

instance ConstantFoldingProblem (LUT v x t) v x
instance OptimizeAccumProblem (LUT v x t) v x
instance OptimizeLutProblem (LUT v x t) v x
instance ResolveDeadlockProblem (LUT v x t) v x

instance Var v => Locks (LUT v x t) v where
    -- FIXME:
    locks _ = []

instance IOTestBench (LUT v x t) v x
instance Default x => DefaultX (LUT v x t) x
instance Time t => Default (LUT v x t) where
    def = lut

-- instance Ord v => Function (LUT v x t) v where
--     inputs = S.fromList . sources --S.fromList . lutInputs
--     outputs = S.fromList . targets --S.singleton . lutOutput
