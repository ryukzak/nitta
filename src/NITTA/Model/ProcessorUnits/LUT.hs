{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module NITTA.Model.ProcessorUnits.LUT (
    LUT (..),
    lut,
    Ports (LUTPorts),
    IOPorts (..),
)
where

import Control.Monad (when)

-- import Data.Char (intToDigit)
import Data.Default (Default, def)
import Data.Foldable as DF (Foldable (null), find)
import Data.List (partition, (\\))
import Data.Maybe
import Data.Set qualified as S
import Data.String.Interpolate
import Data.String.ToString
import Data.Text qualified as T
import Data.Typeable (Typeable)
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

data LUT v x t = LUT
    { remain :: [F v x]
    , targets :: [v]
    , sources :: [v]
    , currentWork :: Maybe (F v x)
    , process_ :: Process t (StepInfo v x t)
    }
    deriving (Typeable)

lut :: Time t => LUT v x t
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

instance VarValTime v x t => Show (LUT v x t) where
    show = show . pretty

instance Default (Microcode (LUT v x t)) where
    def =
        Microcode
            { oeSignal = False
            , wrSignal = False
            , selSignal = False
            }

instance Connected (LUT v x t) where
    data Ports (LUT v x t) = LUTPorts
        { oe :: SignalTag
        , wr :: SignalTag
        , sel :: SignalTag -- todo [SignalTag] ?
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
        { oeSignal :: Bool
        , -- \| Downloading from mUnit signal.
          wrSignal :: Bool
        , -- \| Function selector signal.
          selSignal :: Bool
        }
        deriving (Show, Eq, Ord)

    zipSignalTagsAndValues LUTPorts{..} Microcode{..} =
        [ (oe, Bool oeSignal)
        , (wr, Bool wrSignal)
        , (sel, Bool selSignal)
        ]

    usedPortTags LUTPorts{oe, wr, sel} = [oe, wr, sel]

    takePortTags (oe : wr : sel : _) _ = LUTPorts oe wr sel
    takePortTags _ _ = error "can not take port tags, tags are over"

instance UnambiguouslyDecode (LUT v x t) where
    decodeInstruction Load = def{wrSignal = True}
    decodeInstruction Out = def{oeSignal = True}

softwareFile tag pu = moduleName tag pu <> T.pack "." <> tag <> T.pack ".dump"

instance VarValTime v x t => TargetSystemComponent (LUT v x t) where
    moduleName _title _pu = T.pack "pu_lut"
    hardware _tag _pu = FromLibrary "pu_lut.v"

    software _ _ = Empty

    --   software tag lut@LUT {..} =
    -- let functions = _process lut

    --     _process LUT {process_ = Process {steps}} =
    --       [f | Step _ _ (IntermediateStep f) <- steps]

    --     truthTables = map dummyTruthTable functions

    --     selectorSize = ceiling (logBase 2 (fromIntegral $ length truthTables))
    --     maxInputSize = 2
    --     outputSize = 1

    --     indexedTables = zip [0 ..] truthTables
    --     memoryDump = unlines $ concatMap tableToMemory indexedTables
    --     tableToMemory (index, table) = map (formatEntry index) table

    --     formatEntry index (inputs, output) =
    --       toBinary index selectorSize
    --         ++ "_"
    --         ++ toBinaryList inputs maxInputSize
    --         ++ "_"
    --         ++ toBinary output outputSize

    --     toBinary value size = replicate (size - length bin) '0' ++ bin
    --       where
    --         bin = showIntAtBase' value
    --     toBinaryList inputs size =
    --       let bin = concatMap show inputs
    --        in replicate (size - length bin) '0' ++ bin

    --     dummyTruthTable _ =
    --       [ ([0, 0], 0),
    --         ([0, 1], 1),
    --         ([1, 0], 1),
    --         ([1, 1], 1)
    --       ]

    --     showIntAtBase' :: Int -> String
    --     showIntAtBase' 0 = "0"
    --     showIntAtBase' n =
    --       reverse $
    --         unfoldr
    --           ( \x ->
    --               if x == 0
    --                 then Nothing
    --                 else Just (intToDigit (x `mod` 2), x `div` 2)
    --           )
    --           n
    --  in Aggregate
    --       (Just $ toString mn)
    --       [Immediate (toString mn <> "_lut.dump") $ T.pack memoryDump]
    -- where
    --   mn = moduleName tag lut

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
                    ( .ADDR_WIDTH( #{ attrWidth (def :: x) } )
                    , .DATA_WIDTH( #{ dataWidth (def :: x) } )
                    , .SEL_WIDTH( #{ attrWidth (def :: x) } )
                    , .LUT_DUMP( "{{ impl.paths.nest }}/#{ softwareFile tag _pu }" )
                    ) #{ tag }
                ( .clk( #{ sigClk } )

                , .signal_oe( #{ oe } )
                , .signal_wr( #{ wr } )
                , .signal_sel( #{ sel } )

                , .data_in( #{ dataIn } )
                , .attr_in( #{ attrIn } )
                , .data_out( #{ dataOut } )
                , .attr_out( #{ attrOut } )
                );
        |]
    hardwareInstance _title _pu _env = error "internal error"

-- instance VarValTime v x t => Testable (LUT v x t) v x where
--     testBenchImplementation prj@Project{pName, pUnit} =
--         Immediate (toString $ moduleName pName pUnit <> T.pack "_tb.v") $
--             snippetTestBench
--                 prj
--                 SnippetTestBenchConf
--                     { -- List of control signals. It is needed to initialize
--                       -- registers with the same names.
--                       tbcSignals = map T.pack ["oe", "wr", "sel"]
--                     , -- A processor unit connects to the environment by signal
--                       -- lines. In 'NITTA.Project.TestBench.tbcPorts'
--                       -- describes IDs signal lines of testbench. In
--                       -- 'NITTA.Project.TestBench.tbcSignalConnect' how
--                       -- abstract numbers are translate to source code.
--                       tbcPorts =
--                         LUTPorts
--                             { oe = SignalTag $ T.pack "oe"
--                             , wr = SignalTag $ T.pack "wr"
--                             , sel = SignalTag $ T.pack "sel"
--                             -- , sel = [SignalTag $ T.pack "sel"]
--                             }
--                     , -- Map microcode to registers in the testbench.
--                       tbcMC2verilogLiteral = \Microcode{oeSignal, wrSignal} ->
--                         [i|oe <= #{bool2verilog oeSignal};|]
--                             <> [i| wr <= #{bool2verilog wrSignal};|]
--                     }

instance VarValTime v x t => ProcessorUnit (LUT v x t) v x t where
    tryBind f pu@LUT{remain}
        | Just F.Lut{} <- castF f = Right pu{remain = f : remain ++ remain}
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
                allTargets = targets
             in map (\v -> EndpointSt (Target v) $ TimeConstraint at duration) allTargets
    endpointOptions LUT{sources, currentWork = Just f, process_}
        | not $ DF.null sources =
            let doneAt = inputsPushedAt process_ f + 3
                at = max doneAt (nextTick process_) ... maxBound
                duration = 1 ... maxBound
                allSources = sources
             in [EndpointSt (Source $ S.fromList allSources) $ TimeConstraint at duration]
    endpointOptions pu@LUT{remain} = concatMap (endpointOptions . execution pu) remain

    endpointDecision pu@LUT{targets} d@EndpointSt{epRole = Target v, epAt}
        | not $ null targets
        , let allTargets = targets
        , ([_], targets') <- partition (== v) allTargets
        , let process_' = execSchedule pu $ do
                scheduleEndpoint d $ scheduleInstructionUnsafe epAt Load =
            pu
                { targets = targets'
                , process_ = process_'
                }
    endpointDecision pu@LUT{targets = [], sources, currentWork = Just f, process_} d@EndpointSt{epRole = Source v, epAt}
        | not $ null sources
        , let allSources = sources
        , let sources' = allSources \\ S.elems v
        , sources' /= allSources
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

-- instance Var v => Locks (LUT v x t) v where
--     locks _ = []

instance Var v => Locks (LUT v x t) v where
    locks LUT{remain, sources, targets} =
        [ Lock{lockBy, locked}
        | locked <- sources
        , lockBy <- targets
        ]
            ++ [ Lock{lockBy, locked}
               | locked <- concatMap (S.elems . variables) remain
               , lockBy <- sources ++ targets
               ]
            ++ concatMap locks remain

instance IOTestBench (LUT v x t) v x

instance Default x => DefaultX (LUT v x t) x

instance Time t => Default (LUT v x t) where
    def = lut
