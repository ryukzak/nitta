{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : NITTA.Model.Networks.Bus
Description : Simple process unit network - pseudo bus.
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

For creating BusNetwork see 'NITTA.Model.Microarchitecture.Builder'.
-}
module NITTA.Model.Networks.Bus (
    BusNetwork (..),
    Instruction (..),
    Ports (..),
    IOPorts (..),
    bindedFunctions,
    controlSignalLiteral,
    busNetwork,
) where

import Control.Monad.State
import Data.Bifunctor
import Data.Default
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Data.String
import Data.String.Interpolate
import Data.String.ToString
import qualified Data.Text as T
import Data.Typeable
import NITTA.Intermediate.Types
import NITTA.Model.Networks.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits.Types
import NITTA.Model.Time
import NITTA.Project.TestBench
import NITTA.Project.Types
import NITTA.Project.VerilogSnippets
import NITTA.Utils
import NITTA.Utils.ProcessDescription
import Numeric.Interval.NonEmpty (inf, sup, (...))
import qualified Numeric.Interval.NonEmpty as I
import Prettyprinter
import Text.Regex

data BusNetwork tag v x t = BusNetwork
    { bnName :: tag
    , -- |List of functions binded to network, but not binded to any process unit.
      bnRemains :: [F v x]
    , -- |Map process unit name to list of binded functions.
      bnBinded :: M.Map tag [F v x]
    , -- |Network process (bindings and transport instructions)
      bnProcess :: Process t (StepInfo v x t)
    , -- |Map of process units.
      bnPus :: M.Map tag (PU v x t)
    , -- |Controll bus width.
      bnSignalBusWidth :: Int
    , ioSync :: IOSynchronization
    , bnEnv :: UnitEnv (BusNetwork tag v x t)
    }

busNetwork name iosync =
    BusNetwork
        { bnName = name
        , bnRemains = []
        , bnBinded = M.empty
        , bnProcess = def
        , bnPus = M.empty
        , bnSignalBusWidth = 0
        , ioSync = iosync
        , bnEnv = def
        }

instance (Default t, IsString tag) => Default (BusNetwork tag v x t) where
    def = busNetwork "defaultBus" ASync

instance (Var v) => Variables (BusNetwork tag v x t) v where
    variables BusNetwork{bnBinded} = unionsMap variables $ concat $ M.elems bnBinded

bindedFunctions puTitle BusNetwork{bnBinded}
    | puTitle `M.member` bnBinded = bnBinded M.! puTitle
    | otherwise = []

instance (Default x) => DefaultX (BusNetwork tag v x t) x

instance WithFunctions (BusNetwork tag v x t) (F v x) where
    functions BusNetwork{bnRemains, bnBinded} = bnRemains ++ concat (M.elems bnBinded)

instance (UnitTag tag, VarValTime v x t) => DataflowProblem (BusNetwork tag v x t) tag v t where
    dataflowOptions BusNetwork{bnPus, bnProcess} =
        let sources =
                concatMap
                    (\(tag, pu) -> map (\ep -> (tag, ep)) $ filter isSource $ endpointOptions pu)
                    $ M.assocs bnPus
            targets =
                M.fromList $
                    concatMap
                        ( \(tag, pu) ->
                            concatMap (\ep -> map (,(tag, ep)) $ S.elems $ variables ep) $
                                filter isTarget $ endpointOptions pu
                        )
                        $ M.assocs bnPus
         in filter (not . null . dfTargets) $
                concatMap
                    ( \(src, sEndpoint) ->
                        let dfSource = (src, netConstrain sEndpoint)
                            -- collsion example (can not be sended at the same time):
                            -- fram1
                            --   x1 -> accum
                            --   x2 -> accum
                            (hold, sendWithColisions) =
                                L.partition (\v -> isNothing $ targets M.!? v) $
                                    S.elems $ variables sEndpoint
                            sends =
                                sequence $
                                    M.elems $
                                        foldr
                                            (\v -> M.alter (Just . maybe [v] (v :)) (fst $ targets M.! v))
                                            def
                                            sendWithColisions
                         in map
                                ( \send ->
                                    DataflowSt
                                        { dfSource
                                        , dfTargets =
                                            mapMaybe
                                                (\v -> fmap (second netConstrain) (targets M.!? v))
                                                $ send ++ hold
                                        }
                                )
                                sends
                    )
                    sources
        where
            netConstrain =
                updAt $ \at@TimeConstraint{tcAvailable} ->
                    let a = max (nextTick bnProcess) $ inf tcAvailable
                        b = sup tcAvailable
                     in at{tcAvailable = a ... b}

    dataflowDecision bn@BusNetwork{bnProcess, bnPus} DataflowSt{dfSource = (srcTitle, src), dfTargets}
        | nextTick bnProcess > inf (epAt src) =
            error $ "BusNetwork wraping time! Time: " ++ show (nextTick bnProcess) ++ " Act start at: " ++ show src
        | otherwise =
            let srcStart = inf $ epAt src
                srcDuration = maximum $ map ((\EndpointSt{epAt} -> (inf epAt - srcStart) + I.width epAt) . snd) dfTargets
                srcEnd = srcStart + srcDuration

                subDecisions =
                    (srcTitle, EndpointSt (Source $ unionsMap (variables . snd) dfTargets) (epAt src)) : dfTargets
             in bn
                    { bnPus = foldl applyDecision bnPus subDecisions
                    , bnProcess = execScheduleWithProcess bn bnProcess $ do
                        mapM_
                            ( \(targetTitle, ep) ->
                                scheduleInstructionUnsafe
                                    (srcStart ... srcEnd)
                                    (Transport (oneOf $ variables ep) srcTitle targetTitle :: Instruction (BusNetwork tag v x t))
                            )
                            dfTargets
                    }
        where
            applyDecision pus (trgTitle, d') = M.adjust (`endpointDecision` d') trgTitle pus

instance (UnitTag tag, VarValTime v x t) => ProcessorUnit (BusNetwork tag v x t) v x t where
    tryBind f net@BusNetwork{bnRemains, bnPus}
        | any (allowToProcess f) $ M.elems bnPus =
            Right net{bnRemains = f : bnRemains}
    tryBind f BusNetwork{bnPus} =
        Left [i|All sub process units reject the functional block: #{ f }; rejects: #{ rejects }|]
        where
            rejects = T.intercalate "; " $ map showReject $ M.assocs bnPus
            showReject (tag, pu) | Left err <- tryBind f pu = [i|[#{ toString tag }]: #{ err }"|]
            showReject (tag, _) = [i|[#{ toString tag }]: undefined"|]

    process net@BusNetwork{bnProcess, bnPus} =
        let v2transportStepKey =
                M.fromList
                    [ (v, pID)
                    | step@Step{pID, pDesc} <- steps bnProcess
                    , isInstruction step
                    , v <- case pDesc of
                        (InstructionStep ins) | Just (Transport var _ _) <- castInstruction net ins -> [var]
                        _ -> []
                    ]
            wholeProcess = execScheduleWithProcess net bnProcess $ do
                mapM_ (uncurry includeNestedProcess) $ L.sortOn fst $ M.assocs bnPus
                Process{steps} <- getProcessSlice

                -- Vertical relations between Transport and Endpoint
                let enpointStepKeyVars =
                        concatMap
                            ( \Step{pID, pDesc} ->
                                case pDesc of
                                    NestedStep{nStep = Step{pDesc = EndpointRoleStep role}} ->
                                        zip (repeat pID) $ S.elems $ variables role
                                    _ -> []
                            )
                            steps
                mapM_
                    ( \(epKey, v) ->
                        when (v `M.member` v2transportStepKey) $
                            establishVerticalRelations [v2transportStepKey M.! v] [epKey]
                    )
                    enpointStepKeyVars

                -- Vertical relations between FB and Transport
                mapM_
                    ( \Step{pID, pDesc = NestedStep{nStep = Step{pDesc = FStep f}}} ->
                        mapM_
                            ( \v ->
                                when (v `M.member` v2transportStepKey) $
                                    establishVerticalRelations [pID] [v2transportStepKey M.! v]
                            )
                            $ variables f
                    )
                    $ filter isFB steps
         in wholeProcess
        where
            includeNestedProcess tag pu = do
                let Process{steps, relations} = process pu
                pu2netKey <-
                    M.fromList
                        <$> mapM
                            ( \step@Step{pID} -> do
                                pID' <- scheduleNestedStep tag step
                                return (pID, pID')
                            )
                            steps
                mapM_
                    ( \case
                        (Vertical h l) -> establishVerticalRelations [pu2netKey M.! h] [pu2netKey M.! l]
                        (Horizontal h l) -> establishHorizontalRelations [pu2netKey M.! h] [pu2netKey M.! l]
                    )
                    relations

instance Controllable (BusNetwork tag v x t) where
    data Instruction (BusNetwork tag v x t)
        = Transport v tag tag
        deriving (Typeable)

    data Microcode (BusNetwork tag v x t)
        = BusNetworkMC (M.Map SignalTag SignalValue)

    -- Right now, BusNetwork don't have external control (exclude rst signal and some hacks). All
    -- signals starts and ends inside network unit.
    zipSignalTagsAndValues BusNetworkPorts BusNetworkMC{} = []

    usedPortTags _ = error "internal error"

    takePortTags _ _ = error "internal error"

instance (ToString tag, Var v) => Show (Instruction (BusNetwork tag v x t)) where
    show (Transport v src trg) = "Transport " <> toString v <> " " <> toString src <> " " <> toString trg

instance {-# OVERLAPS #-} ByTime (BusNetwork tag v x t) t where
    microcodeAt BusNetwork{..} t =
        BusNetworkMC $ foldl merge initSt $ M.elems bnPus
        where
            initSt = M.fromList $ map (\ins -> (SignalTag $ controlSignalLiteral ins, def)) [0 .. bnSignalBusWidth - 1]

            merge st PU{unit, uEnv = UnitEnv{ctrlPorts = Just ports}} =
                foldl merge' st $ zipSignalTagsAndValues ports $microcodeAt unit t
            merge _ _ = error "internal error"

            merge' st (signalTag, value) = M.adjust (+++ value) signalTag st

----------------------------------------------------------------------

instance
    (UnitTag tag, VarValTime v x t) =>
    BindProblem (BusNetwork tag v x t) tag v x
    where
    bindOptions BusNetwork{bnRemains, bnPus} = concatMap optionsFor bnRemains
        where
            optionsFor f =
                [ Bind f puTitle
                | (puTitle, pu) <- M.assocs bnPus
                , allowToProcess f pu
                ]

    bindDecision bn@BusNetwork{bnProcess, bnPus, bnBinded, bnRemains} (Bind f tag) =
        bn
            { bnPus = M.adjust (bind f) tag bnPus
            , bnBinded = registerBinding tag f bnBinded
            , bnProcess = execScheduleWithProcess bn bnProcess $ scheduleFunctionBind f
            , bnRemains = filter (/= f) bnRemains
            }

instance (UnitTag tag, VarValTime v x t) => BreakLoopProblem (BusNetwork tag v x t) v x where
    breakLoopOptions BusNetwork{bnPus} = concatMap breakLoopOptions $ M.elems bnPus

    breakLoopDecision bn@BusNetwork{bnBinded, bnPus} bl@BreakLoop{} =
        let Just (puTag, bindedToPU) = L.find (elem (recLoop bl) . snd) $ M.assocs bnBinded
            bindedToPU' = recLoopIn bl : recLoopOut bl : (bindedToPU L.\\ [recLoop bl])
         in bn
                { bnPus = M.adjust (`breakLoopDecision` bl) puTag bnPus
                , bnBinded = M.insert puTag bindedToPU' bnBinded
                }

instance (UnitTag tag, VarValTime v x t) => OptimizeAccumProblem (BusNetwork tag v x t) v x where
    optimizeAccumOptions BusNetwork{bnRemains} = optimizeAccumOptions bnRemains

    optimizeAccumDecision bn@BusNetwork{bnRemains, bnProcess} oa@OptimizeAccum{} =
        bn
            { bnRemains = optimizeAccumDecision bnRemains oa
            , bnProcess = execScheduleWithProcess bn bnProcess $ do
                scheduleRefactoring (I.singleton $ nextTick bn) oa
            }

instance (UnitTag tag, VarValTime v x t) => ConstantFoldingProblem (BusNetwork tag v x t) v x where
    constantFoldingOptions BusNetwork{bnRemains} = constantFoldingOptions bnRemains

    constantFoldingDecision bn@BusNetwork{bnRemains, bnProcess} cf@ConstantFolding{} =
        bn
            { bnRemains = constantFoldingDecision bnRemains cf
            , bnProcess = execScheduleWithProcess bn bnProcess $ do
                scheduleRefactoring (I.singleton $ nextTick bn) cf
            }

instance (UnitTag tag, VarValTime v x t) => ResolveDeadlockProblem (BusNetwork tag v x t) v x where
    resolveDeadlockOptions bn@BusNetwork{bnPus, bnBinded} =
        let prepareResolve :: S.Set v -> [ResolveDeadlock v x]
            prepareResolve =
                map resolveDeadlock
                    . S.elems
                    . S.filter (not . S.null)
                    . ( \lockedVs ->
                            if S.null lockedVs
                                then S.empty
                                else S.filter (not . (lockedVs `S.disjoint`)) $ S.powerSet (var2endpointRole M.! oneOf lockedVs)
                      )
                    . S.filter (isBufferRepetionOK maxBufferStack)

            isBufferRepetionOK 0 _ = False
            isBufferRepetionOK n v
                | bufferSuffix v `S.notMember` variables bn = True
                | otherwise = isBufferRepetionOK (n -1) (bufferSuffix v)

            selfSending =
                concatMap
                    (\(tag, fs) -> prepareResolve (unionsMap inputs fs `S.intersection` puOutputs tag))
                    $ M.assocs bnBinded

            allPULocks = map (second locks) $ M.assocs bnPus

            resolveLocks =
                concat
                    [ prepareResolve $ S.singleton lockBy
                    | (tag, ls) <- allPULocks
                    , Lock{lockBy, locked} <- ls
                    , lockBy `S.member` maybeSended
                    , let reversedLock = Lock{lockBy = locked, locked = lockBy}
                    , any (\(t, puLocks) -> tag /= t && reversedLock `elem` puLocks) allPULocks
                    ]
         in L.nub $ selfSending ++ resolveLocks
        where
            endPointRoles = M.map (\pu -> map epRole $ endpointOptions pu) bnPus

            puOutputs tag =
                unionsMap variables $
                    filter (\case Source{} -> True; _ -> False) $ endPointRoles M.! tag

            var2endpointRole =
                M.fromList $
                    concatMap
                        ( \case
                            (Source vs) -> [(v, vs) | v <- S.elems vs]
                            (Target v) -> [(v, S.singleton v)]
                        )
                        $ concat $ M.elems endPointRoles

            maybeSended = M.keysSet var2endpointRole

    resolveDeadlockDecision
        bn@BusNetwork{bnRemains, bnBinded, bnPus, bnProcess}
        ref@ResolveDeadlock{newBuffer, changeset} =
            let Just (tag, _) =
                    L.find
                        (\(_, f) -> not $ null $ S.intersection (outputs newBuffer) $ unionsMap outputs f)
                        $ M.assocs bnBinded
             in bn
                    { bnRemains = newBuffer : patch changeset bnRemains
                    , bnPus = M.adjust (patch changeset) tag bnPus
                    , bnBinded = M.map (patch changeset) bnBinded
                    , bnProcess = execScheduleWithProcess bn bnProcess $ do
                        scheduleRefactoring (I.singleton $ nextTick bn) ref
                    }

--------------------------------------------------------------------------

controlSignalLiteral ix = [i|control_bus[#{ ix }]|]

-- |Add binding to Map tag [F v x] dict
registerBinding tag f dict =
    M.alter (maybe (Just [f]) (Just . (f :))) tag dict

programTicks bn = [-1 .. nextTick bn]

bnExternalPorts pus =
    M.assocs $
        M.map
            ( \pu ->
                ( map inputPortTag $ S.toList $ puInputPorts pu
                , map outputPortTag $ S.toList $ puOutputPorts pu
                , map inoutPortTag $ S.toList $ puInOutPorts pu
                )
            )
            pus

externalPortsDecl ports =
    concatMap
        ( \(tag, (is, os, ios)) ->
            concat
                [ ["// external ports for: " <> toText tag]
                , map (", input " <>) is
                , map (", output " <>) os
                , map (", inout " <>) ios
                ]
        )
        ports

instance (UnitTag tag, VarValTime v x t) => TargetSystemComponent (BusNetwork tag v x t) where
    moduleName _tag BusNetwork{bnName} = toText bnName

    hardware tag pu@BusNetwork{..} =
        let (instances, valuesRegs) = renderInstance [] [] $ M.assocs bnPus
            mn = moduleName tag pu
            iml =
                [__i|
                    module #{ mn } \#
                            ( parameter DATA_WIDTH = #{ dataWidth (def :: x) }
                            , parameter ATTR_WIDTH = #{ attrWidth (def :: x) }
                            )
                        ( input                     clk
                        , input                     rst
                        , input                     is_drop_allow
                        , output                    flag_cycle_begin
                        , output                    flag_in_cycle
                        , output                    flag_cycle_end
                        #{ nest 4 $ vsep $ map pretty $ externalPortsDecl $ bnExternalPorts bnPus }
                        , output              [7:0] debug_status
                        , output              [7:0] debug_bus1
                        , output              [7:0] debug_bus2
                        );

                    parameter MICROCODE_WIDTH = #{ bnSignalBusWidth };

                    wire start, stop;

                    wire [MICROCODE_WIDTH-1:0] control_bus;
                    wire [DATA_WIDTH-1:0] data_bus;
                    wire [ATTR_WIDTH-1:0] attr_bus;

                    // Debug
                    assign debug_status = { flag_cycle_begin, flag_in_cycle, flag_cycle_end, data_bus[4:0] };
                    assign debug_bus1 = data_bus[7:0];
                    assign debug_bus2 = data_bus[31:24] | data_bus[23:16] | data_bus[15:8] | data_bus[7:0];


                    // Sub module instances

                    pu_simple_control \#
                            ( .MICROCODE_WIDTH( MICROCODE_WIDTH )
                            , .PROGRAM_DUMP( "{{ impl.paths.nest }}/#{ mn }.dump" )
                            , .MEMORY_SIZE( #{ length $ programTicks pu } ) // 0 - address for nop microcode
                            ) control_unit
                        ( .clk( clk )
                        , .rst( rst )

                        , .signal_cycle_start( #{ isDrowAllowSignal ioSync } || stop )

                        , .signals_out( control_bus )

                        , .flag_cycle_begin( flag_cycle_begin )
                        , .flag_in_cycle( flag_in_cycle )
                        , .flag_cycle_end( flag_cycle_end )
                        );

                    #{ vsep $ punctuate "\n\n" instances }

                    assign data_bus = #{ T.intercalate " | " $ map snd valuesRegs };
                    assign attr_bus = #{ T.intercalate " | " $ map fst valuesRegs };

                    endmodule
                    |]
         in Aggregate (Just $ toString mn) $
                [ Immediate (toString $ mn <> ".v") iml
                , FromLibrary "pu_simple_control.v"
                ]
                    <> map (uncurry hardware . first toText) (M.assocs bnPus)
        where
            regInstance t =
                [__i|
                    wire [DATA_WIDTH-1:0] #{ t }_data_out;
                    wire [ATTR_WIDTH-1:0] #{ t }_attr_out;
                |]

            renderInstance insts regs [] = (reverse insts, reverse regs)
            renderInstance insts regs ((t, PU{unit, uEnv}) : xs) =
                let inst = hardwareInstance (toText t) unit uEnv
                    insts' = inst : regInstance (toText t) : insts
                    regs' = (toText t <> "_attr_out", toText t <> "_data_out") : regs
                 in renderInstance insts' regs' xs

    software tag pu@BusNetwork{bnProcess = Process{}, ..} =
        let subSW = map (uncurry software . first toText) $ M.assocs bnPus
            sw = [Immediate (toString $ mn <> ".dump") $ T.pack memoryDump]
         in Aggregate (Just $ toString mn) $ subSW ++ sw
        where
            mn = moduleName tag pu
            -- Nop operation sets for all processor units at address 0. It is a
            -- safe state of the processor which is selected when rst signal is
            -- active.
            memoryDump = unlines $ map (values2dump . values . microcodeAt pu) $ programTicks pu
            values (BusNetworkMC arr) =
                reverse $
                    map snd $
                        L.sortOn ((\(Just [ix]) -> read ix :: Int) . matchRegex (mkRegex "([[:digit:]]+)") . T.unpack . signalTag . fst) $
                            M.assocs arr

    hardwareInstance tag BusNetwork{} UnitEnv{sigRst, sigClk, ioPorts = Just ioPorts}
        | let io2v n = [i|, .#{ n }( #{ n } )|]
              is = map (io2v . inputPortTag) $ S.toList $ inputPorts ioPorts
              os = map (io2v . outputPortTag) $ S.toList $ outputPorts ioPorts =
            [__i|
                    #{ tag } \#
                            ( .DATA_WIDTH( #{ dataWidth (def :: x) } )
                            , .ATTR_WIDTH( #{ attrWidth (def :: x) } )
                            ) net
                        ( .rst( #{ sigRst } )
                        , .clk( #{ sigClk } )
                        // inputs:
                        #{ nest 4 $ vsep is }
                        // outputs:
                        #{ nest 4 $ vsep os }
                        , .debug_status( debug_status ) // FIXME:
                        , .debug_bus1( debug_bus1 )     // FIXME:
                        , .debug_bus2( debug_bus2 )     // FIXME:
                        , .is_drop_allow( rendezvous )  // FIXME:
                        );
                |]
    hardwareInstance _title _bn _env =
        error "BusNetwork should be NetworkEnv"

instance Connected (BusNetwork tag v x t) where
    data Ports (BusNetwork tag v x t) = BusNetworkPorts
        deriving (Show)

instance IOConnected (BusNetwork tag v x t) where
    data IOPorts (BusNetwork tag v x t) = BusNetworkIO
        { extInputs :: S.Set InputPortTag
        , extOutputs :: S.Set OutputPortTag
        , extInOuts :: S.Set InoutPortTag
        }
        deriving (Show)
    inputPorts = extInputs
    outputPorts = extOutputs
    inoutPorts = extInOuts

instance (UnitTag tag, VarValTime v x t) => Testable (BusNetwork tag v x t) v x where
    testBenchImplementation
        Project
            { pName
            , pUnit = bn@BusNetwork{bnPus, ioSync, bnName}
            , pTestCntx = pTestCntx@Cntx{cntxProcess, cntxCycleNumber}
            } =
            let testEnv =
                    vsep $
                        mapMaybe
                            ( \(tag, PU{unit, uEnv}) ->
                                let tEnv =
                                        TestEnvironment
                                            { teCntx = pTestCntx
                                            , teComputationDuration = fromEnum $ nextTick bn
                                            }
                                 in testEnvironment (toText tag) unit uEnv tEnv
                            )
                            $ M.assocs bnPus

                externalPortNames = map pretty $ concatMap ((\(is, os, ios) -> is <> os <> ios) . snd) $ bnExternalPorts bnPus
                externalIO = vsep $ punctuate ", " ("" : map (\p -> [i|.#{ p }( #{ p } )|]) externalPortNames)

                envInitFlags = map pretty $ mapMaybe (uncurry testEnvironmentInitFlag . first toText) $ M.assocs bnPus

                tickWithTransfers =
                    map
                        ( \(cycleI, cycleCntx) ->
                            map
                                (\t -> (cycleI, t, cntxToTransfer cycleCntx t))
                                [0 .. nextTick bn]
                        )
                        $ zip [0 :: Int ..] $ take cntxCycleNumber cntxProcess

                assertions = vsep $ map (\cycleTickTransfer -> posedgeCycle <> line <> vsep (map assertion cycleTickTransfer)) tickWithTransfers

                assertion (cycleI, t, Nothing) =
                    [i|@(posedge clk); traceWithAttr(#{ cycleI }, #{ t }, #{ toString bnName }.data_bus, #{ toString bnName }.attr_bus);|]
                assertion (cycleI, t, Just (v, x)) =
                    [i|@(posedge clk); assertWithAttr(#{ cycleI }, #{ t }, #{ toString bnName }.data_bus, #{ toString bnName }.attr_bus, #{ dataLiteral x }, #{ attrLiteral x }, "#{ toString v }");|]

                tbName = moduleName pName bn <> "_tb"
             in Aggregate
                    Nothing
                    [ Immediate (toString $ tbName <> ".v") $
                        doc2text
                            [__i|
                        `timescale 1 ps / 1 ps
                        module #{ tbName }();

                        /*
                        Functions:
                        #{ indent 4 $ vsep $ map viaShow $ functions bn }
                        */

                        /*
                        Steps:
                        #{ indent 4 $ vsep $ map viaShow $ reverse $ steps $ process bn }
                        */

                        // system signals
                        reg clk, rst;
                        wire cycle;

                        // clk and rst generator
                        #{ snippetClkGen }

                        // vcd dump
                        #{ snippetDumpFile $ moduleName pName bn }


                        ////////////////////////////////////////////////////////////
                        // test environment

                        // external ports (IO)
                        #{ if null externalPortNames then "" else "wire " <> hsep (punctuate ", " externalPortNames) <> ";" }

                        // initialization flags
                        #{ if null envInitFlags then "" else "reg " <> hsep (punctuate ", " envInitFlags) <> ";" }
                        assign env_init_flag = #{ hsep $ defEnvInitFlag envInitFlags ioSync };

                        #{ testEnv }


                        ////////////////////////////////////////////////////////////
                        // unit under test

                        #{ moduleName pName bn } \#
                                ( .DATA_WIDTH( #{ dataWidth (def :: x) } )
                                , .ATTR_WIDTH( #{ attrWidth (def :: x) } )
                                ) #{ toString bnName }
                            ( .clk( clk )
                            , .rst( rst )
                            , .flag_cycle_begin( cycle )
                            #{ nest 4 externalIO }
                            // if 1 - The process cycle are indipendent from a SPI.
                            // else - The process cycle are wait for the SPI.
                            , .is_drop_allow( #{ isDrowAllowSignal ioSync } )
                            );

                        // internal unit under test checks
                        initial
                            begin
                                // microcode when rst == 1 -> program[0], and must be nop for all PUs
                                @(negedge rst); // Turn mUnit on.
                                // Start computational cycle from program[1] to program[n] and repeat.
                                // Signals effect to mUnit state after first clk posedge.
                                @(posedge clk);
                                while (!env_init_flag) @(posedge clk);
                                #{ nest 8 assertions }
                                repeat ( #{ 2 * nextTick bn } ) @(posedge clk);
                                $finish;
                            end

                        // TIMEOUT
                        initial
                            begin
                                repeat (100000) @(posedge clk);
                                $display("FAIL too long simulation process");
                                $finish;
                            end

                        ////////////////////////////////////////////////////////////
                        // Utils
                        #{ verilogHelper (def :: x) }

                        endmodule
                        |]
                    , Immediate (toString $ tbName <> ".gtkw") $
                        T.pack
                            [__i|
                                [*]
                                [*] GTKWave Analyzer v3.3.107 (w)1999-2020 BSI
                                [*] Fri Mar 12 11:37:55 2021
                                [*]
                                [dumpfile] "{{ nitta.paths.abs_nitta }}/#{ tbName }.vcd"
                                [timestart] 0
                                *-6.864726 0 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
                                [treeopen] #{ tbName }.
                                [treeopen] #{ tbName }.#{ toString bnName }.
                                [sst_width] 193
                                [signals_width] 203
                                [sst_expanded] 1
                                [sst_vpaned_height] 167
                                @28
                                #{ tbName }.clk
                                #{ tbName }.rst
                                #{ tbName }.cycle
                                @24
                                #{ tbName }.#{ toString bnName }.control_unit.pc[5:0]
                                @28
                                #{ tbName }.#{ toString bnName }.control_unit.flag_cycle_begin
                                #{ tbName }.#{ toString bnName }.control_unit.flag_cycle_end
                                @25
                                #{ tbName }.#{ toString bnName }.data_bus[31:0]
                                @22
                                #{ tbName }.#{ toString bnName }.attr_bus[3:0]
                                [pattern_trace] 1
                                [pattern_trace] 0
                                |]
                    ]
            where
                defEnvInitFlag flags Sync = punctuate " && " $ "1'b1" : flags
                defEnvInitFlag flags ASync = punctuate " || " $ "1'b1" : flags
                defEnvInitFlag _flags OnBoard = error "can't generate testbench without specific IOSynchronization"

                cntxToTransfer cycleCntx t =
                    case extractInstructionAt bn t of
                        Transport v _ _ : _ -> Just (v, getCntx cycleCntx v)
                        _ -> Nothing

                posedgeCycle =
                    [__i|
                        //-----------------------------------------------------------------
                        @(posedge cycle);
                    |]

isDrowAllowSignal Sync = bool2verilog False
isDrowAllowSignal ASync = bool2verilog True
isDrowAllowSignal OnBoard = "is_drop_allow"
