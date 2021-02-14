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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : NITTA.Model.Networks.Bus
Description : Simple process unit network - pseudo bus.
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Networks.Bus (
    busNetwork,
    BusNetwork (..),
    Ports (..),
    IOPorts (..),
    bindedFunctions,
) where

import Control.Monad.State
import qualified Data.Array as A
import Data.Bifunctor
import Data.Default
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Set as S
import Data.String.Interpolate (i)
import qualified Data.String.Utils as S
import qualified Data.Text as T
import Data.Typeable
import NITTA.Intermediate.DataFlow
import NITTA.Intermediate.Types
import NITTA.Model.Networks.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits.Types
import NITTA.Model.Types
import NITTA.Project.Implementation
import NITTA.Project.Parts.TestBench
import NITTA.Project.SnippetsText
import NITTA.Project.Types
import NITTA.Utils hiding (codeBlock, codeLine, comment, inline)
import NITTA.Utils.CodeFormatText
import NITTA.Utils.ProcessDescription
import Numeric.Interval.NonEmpty (inf, sup, width, (...))

data BusNetwork tag v x t = BusNetwork
    { -- |List of functions binded to network, but not binded to any process unit.
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
    , bnEnv :: TargetEnvironment
    , bnIOPorts :: IOPorts (BusNetwork tag v x t)
    }

instance (Var v) => Variables (BusNetwork tag v x t) v where
    variables BusNetwork{bnBinded} = unionsMap variables $ concat $ M.elems bnBinded

bindedFunctions puTitle BusNetwork{bnBinded}
    | puTitle `M.member` bnBinded = bnBinded M.! puTitle
    | otherwise = []

instance (Default x) => DefaultX (BusNetwork tag v x t) x

busNetwork signalBusWidth ioSync pus =
    BusNetwork
        { bnRemains = []
        , bnBinded = M.empty
        , bnProcess = def
        , bnPus = M.fromList pus'
        , bnSignalBusWidth = signalBusWidth
        , ioSync
        , bnIOPorts = BusNetworkIO{extInputs, extOutputs}
        , bnEnv
        }
    where
        bnEnv =
            TargetEnvironment
                { signalClk = "clk"
                , signalRst = "rst"
                , signalCycleBegin = "flag_cycle_begin"
                , signalInCycle = "flag_in_cycle"
                , signalCycleEnd = "flag_cycle_end"
                , inputPort = \(InputPortTag n) -> n
                , outputPort = \(OutputPortTag n) -> n
                , inoutPort = \(InoutPortTag n) -> n
                , unitEnv = NetworkEnv
                }
        puEnv tag =
            bnEnv
                { unitEnv =
                    ProcessUnitEnv
                        { dataIn = "data_bus"
                        , dataOut = tag ++ "_data_out"
                        , attrIn = "attr_bus"
                        , attrOut = tag ++ "_attr_out"
                        , signal = \(SignalTag sig) -> "control_bus[" ++ show sig ++ "]"
                        }
                }
        pus' = map (\(tag, f) -> (tag, f $ puEnv tag)) pus
        extInputs = L.nub $ concatMap (\(_, PU{ioPorts}) -> inputPorts ioPorts) pus'
        extOutputs = L.nub $ concatMap (\(_, PU{ioPorts}) -> outputPorts ioPorts) pus'

instance WithFunctions (BusNetwork tag v x t) (F v x) where
    functions BusNetwork{bnRemains, bnBinded} = bnRemains ++ concat (M.elems bnBinded)

instance (UnitTag tag, VarValTime v x t) => DataflowProblem (BusNetwork tag v x t) tag v t where
    dataflowOptions BusNetwork{bnPus, bnProcess} =
        notEmptyDestination $
            concat
                [ map (DataflowSt (source, fixConstrain pullAt)) $ targetOptionsFor $ S.elems vars
                | (source, opts) <- puOptions
                , EndpointSt (Source vars) pullAt <- opts
                ]
        where
            puOptions = M.assocs $ M.map endpointOptions bnPus
            targetOptionsFor vs =
                let conflictableTargets =
                        [ (pushVar, Just (target, fixConstrain pushAt))
                        | (target, opts) <- puOptions
                        , EndpointSt (Target pushVar) pushAt <- opts
                        , pushVar `elem` vs
                        ]
                    targets = sequence $ L.groupBy (\a b -> tgr a == tgr b) $ L.sortOn tgr conflictableTargets
                    zero = zip vs $ repeat Nothing
                 in map (M.fromList . (++) zero) targets

            fixConstrain constrain@TimeConstrain{tcAvailable} =
                let a = max (nextTick bnProcess) $ inf tcAvailable
                    b = sup tcAvailable
                 in constrain{tcAvailable = a ... b}

            notEmptyDestination = filter $ \DataflowSt{dfTargets} -> any isJust $ M.elems dfTargets
            tgr (_, Just (target, _)) = Just target
            tgr _ = Nothing

    dataflowDecision n@BusNetwork{bnProcess, bnPus} DataflowSt{dfSource = (srcTitle, pullAt), dfTargets}
        | nextTick bnProcess > inf pullAt =
            error $ "BusNetwork wraping time! Time: " ++ show (nextTick bnProcess) ++ " Act start at: " ++ show pullAt
        | otherwise =
            let pushs =
                    M.fromList $
                        mapMaybe
                            ( \case
                                (k, Just v) -> Just (k, v)
                                (_, Nothing) -> Nothing
                            )
                            $ M.assocs dfTargets
                transportStartAt = inf pullAt
                transportDuration = maximum $ map (\(_trg, time) -> (inf time - transportStartAt) + width time) $ M.elems pushs
                transportEndAt = transportStartAt + transportDuration

                subDecisions =
                    (srcTitle, EndpointSt (Source $ S.fromList $ M.keys pushs) pullAt) :
                        [ (trgTitle, EndpointSt (Target v) pushAt)
                        | (v, (trgTitle, pushAt)) <- M.assocs pushs
                        ]
             in n
                    { bnPus = foldl applyDecision bnPus subDecisions
                    , bnProcess = execScheduleWithProcess n bnProcess $ do
                        updateTick (sup pullAt + 1)
                        mapM_
                            ( \(pushedValue, (targetTitle, _tc)) ->
                                scheduleInstruction
                                    (transportStartAt ... transportEndAt)
                                    (Transport pushedValue srcTitle targetTitle :: Instruction (BusNetwork tag v x t))
                            )
                            $ M.assocs pushs
                    }
        where
            applyDecision pus (trgTitle, d') = M.adjust (`endpointDecision` d') trgTitle pus

instance (UnitTag tag, VarValTime v x t) => ProcessorUnit (BusNetwork tag v x t) v x t where
    tryBind f net@BusNetwork{bnRemains, bnPus}
        | any (allowToProcess f) $ M.elems bnPus =
            Right net{bnRemains = f : bnRemains}
    tryBind f BusNetwork{bnPus} =
        Left $ "All sub process units reject the functional block: " ++ show f ++ "\n" ++ rejects
        where
            rejects = S.join "\n" $ map showReject $ M.assocs bnPus
            showReject (tag, pu) | Left err <- tryBind f pu = "    [" ++ show tag ++ "]: " ++ err
            showReject (tag, _) = "    [" ++ show tag ++ "]: undefined"

    process net@BusNetwork{bnProcess, bnPus} =
        let v2transportStepKey =
                M.fromList
                    [ (v, pID)
                    | Step{pID, pDesc} <- steps bnProcess
                    , isInstruction pDesc
                    , v <- case pDesc of
                        (InstructionStep is) | Just (Transport var _ _) <- castInstruction net is -> [var]
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
                            establishVerticalRelation (v2transportStepKey M.! v) epKey
                    )
                    enpointStepKeyVars

                -- Vertical relations between FB and Transport
                mapM_
                    ( \Step{pID, pDesc = NestedStep{nStep = Step{pDesc = FStep f}}} ->
                        mapM_
                            ( \v ->
                                when (v `M.member` v2transportStepKey) $
                                    establishVerticalRelation pID (v2transportStepKey M.! v)
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
                mapM_ (\(Vertical h l) -> establishVerticalRelation (pu2netKey M.! h) (pu2netKey M.! l)) relations

instance Controllable (BusNetwork tag v x t) where
    data Instruction (BusNetwork tag v x t)
        = Transport v tag tag
        deriving (Typeable, Show)

    data Microcode (BusNetwork tag v x t)
        = BusNetworkMC (A.Array SignalTag SignalValue)

    -- Right now, BusNetwork don't have external control (exclude rst signal and some hacks). All
    -- signals starts and ends inside network unit.
    mapMicrocodeToPorts BusNetworkMC{} BusNetworkPorts = []

    portsToSignals _ = undefined

    signalsToPorts _ _ = undefined

instance {-# OVERLAPS #-} ByTime (BusNetwork tag v x t) t where
    microcodeAt BusNetwork{..} t =
        BusNetworkMC $ foldl merge initSt $ M.elems bnPus
        where
            initSt = A.listArray (SignalTag 0, SignalTag $ bnSignalBusWidth - 1) $ repeat def
            merge st PU{unit, ports} =
                foldl merge' st $ mapMicrocodeToPorts (microcodeAt unit t) ports
            merge' st (s, x) = st A.// [(s, st A.! s +++ x)]

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

    bindDecision n@BusNetwork{bnProcess, bnPus, bnBinded, bnRemains} (Bind f tag) =
        n
            { bnPus = M.adjust (bind f) tag bnPus
            , bnBinded = registerBinding tag f bnBinded
            , bnProcess = execScheduleWithProcess n bnProcess $ scheduleFunctionBind f
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

instance (VarValTime v x t) => OptimizeAccumProblem (BusNetwork tag v x t) v x where
    optimizeAccumOptions BusNetwork{bnRemains} = optimizeAccumOptions $ fsToDataFlowGraph bnRemains

    optimizeAccumDecision bn@BusNetwork{bnRemains} oa@OptimizeAccum{} =
        bn{bnRemains = functions $ optimizeAccumDecision (fsToDataFlowGraph bnRemains) oa}

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

    resolveDeadlockDecision bn@BusNetwork{bnRemains, bnBinded, bnPus} ResolveDeadlock{newBuffer, changeset} =
        let Just (tag, _) =
                L.find
                    (\(_, f) -> not $ null $ S.intersection (outputs newBuffer) $ unionsMap outputs f)
                    $ M.assocs bnBinded
         in bn
                { bnRemains = newBuffer : patch changeset bnRemains
                , bnPus = M.adjust (patch changeset) tag bnPus
                , bnBinded = M.map (patch changeset) bnBinded
                }

--------------------------------------------------------------------------

-- |Add binding to Map tag [F v x] dict
registerBinding tag f dict =
    M.alter (maybe (Just [f]) (Just . (f :))) tag dict

programTicks BusNetwork{bnProcess = Process{nextTick}} = [-1 .. nextTick]

bnExternalPorts pus =
    M.assocs $
        M.map
            ( \PU{ioPorts} ->
                ( map inputPortTag $ inputPorts ioPorts
                , map outputPortTag $ outputPorts ioPorts
                )
            )
            pus

externalPortsDecl ports =
    T.unlines $
        concatMap
            ( \(tag, (is, os)) ->
                ("// external ports for: " <> T.pack tag) :
                map ((", input " <>) . T.pack) is
                    ++ map ((", output " <>) . T.pack) os
            )
            ports

instance (VarValTime v x t) => TargetSystemComponent (BusNetwork String v x t) where
    moduleName tag BusNetwork{..} = tag ++ "_net"

    hardware tag pu@BusNetwork{..} =
        let (instances, valuesRegs) = renderInstance [] [] $ M.assocs bnPus
            mn = moduleName tag pu
            iml =
                T.unpack $
                    codeBlock
                        [i|
                    module #{ mn } #
                            ( parameter DATA_WIDTH = #{ dataWidth (def :: x) }
                            , parameter ATTR_WIDTH = #{ attrWidth (def :: x) }
                            )
                        ( input                     clk
                        , input                     rst
                        , input                     is_drop_allow
                        , output                    flag_cycle_begin
                        , output                    flag_in_cycle
                        , output                    flag_cycle_end
                        #{ inline $ externalPortsDecl $ bnExternalPorts bnPus }
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

                    pu_simple_control #
                            ( .MICROCODE_WIDTH( MICROCODE_WIDTH )
                            , .PROGRAM_DUMP( "$path$#{ mn }.dump" )
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

                    #{ inline $ T.intercalate "\n\n" instances }

                    assign data_bus = #{ T.intercalate " | " $ map snd valuesRegs };
                    assign attr_bus = #{ T.intercalate " | " $ map fst valuesRegs };

                    endmodule
                    |]
         in Aggregate (Just mn) $
                [ Immediate (mn <> ".v") iml
                , FromLibrary "pu_simple_control.v"
                ]
                    ++ map (uncurry hardware) (M.assocs bnPus)
        where
            regInstance t =
                codeBlock
                    [i|
                    wire [DATA_WIDTH-1:0] #{t}_data_out;
                    wire [ATTR_WIDTH-1:0] #{t}_attr_out;
                    |]

            renderInstance insts regs [] = (reverse insts, reverse regs)
            renderInstance insts regs ((t, PU{unit, systemEnv, ports, ioPorts}) : xs) =
                -- TODO: 1S->T when will be possible rewrite without T.pack
                let inst = T.pack $ hardwareInstance t unit systemEnv ports ioPorts
                    insts' = inst : regInstance t : insts
                    regs' = (T.pack $ t <> "_attr_out", T.pack $ t <> "_data_out") : regs
                 in renderInstance insts' regs' xs

    software tag pu@BusNetwork{bnProcess = Process{..}, ..} =
        let subSW = map (uncurry software) (M.assocs bnPus)
            sw = [Immediate (mn ++ ".dump") memoryDump]
         in Aggregate (Just mn) $ subSW ++ sw
        where
            mn = moduleName tag pu
            -- Nop operation sets for all processor units at address 0. It is a
            -- safe state of the processor which is selected when rst signal is
            -- active.
            memoryDump = unlines $ map (values2dump . values . microcodeAt pu) $ programTicks pu
            values (BusNetworkMC arr) = reverse $ A.elems arr

    hardwareInstance tag BusNetwork{} TargetEnvironment{unitEnv = NetworkEnv, signalClk, signalRst} _ports ioPorts
        | let io2v n = ", ." <> n <> "( " <> n <> " )"
              -- 1S->T rewrite without T.pack when ioPorts become Text
              is = map (\(InputPortTag n) -> io2v $ T.pack n) $ inputPorts ioPorts
              os = map (\(OutputPortTag n) -> io2v $ T.pack n) $ outputPorts ioPorts =
            T.unpack $
                codeBlock
                    [i|
            #{ tag } #
                    ( .DATA_WIDTH( #{ dataWidth (def :: x) } )
                    , .ATTR_WIDTH( #{ attrWidth (def :: x) } )
                    ) net
                ( .rst( #{ signalRst } )
                , .clk( #{ signalClk } )
                // inputs:
                #{ inline $ T.intercalate "\\n" is }
                // outputs:
                #{ inline $ T.intercalate "\\n" os }
                , .debug_status( debug_status ) // FIXME:
                , .debug_bus1( debug_bus1 )     // FIXME:
                , .debug_bus2( debug_bus2 )     // FIXME:
                , .is_drop_allow( rendezvous )  // FIXME:
                );
            |]
    hardwareInstance _title _bn TargetEnvironment{unitEnv = ProcessUnitEnv{}} _ports _io =
        error "BusNetwork should be NetworkEnv"

instance Connected (BusNetwork tag v x t) where
    data Ports (BusNetwork tag v x t) = BusNetworkPorts
        deriving (Show)

instance IOConnected (BusNetwork tag v x t) where
    data IOPorts (BusNetwork tag v x t) = BusNetworkIO
        { extInputs :: [InputPortTag]
        , extOutputs :: [OutputPortTag]
        }
        deriving (Show)
    inputPorts = extInputs
    outputPorts = extOutputs

instance
    (VarValTime v x t, TargetSystemComponent (BusNetwork String v x t)) =>
    Testable (BusNetwork String v x t) v x
    where
    testBenchImplementation
        Project
            { pName
            , pUnit = n@BusNetwork{bnProcess, bnPus, ioSync}
            , pTestCntx = pTestCntx@Cntx{cntxProcess, cntxCycleNumber}
            } =
            let testEnv =
                    S.join
                        "\\n\\n"
                        [ tbEnv
                        | (t, PU{unit, systemEnv, ports, ioPorts}) <- M.assocs bnPus
                        , let t' = filter (/= '"') $ show t
                        , let tbEnv =
                                testEnvironment
                                    t'
                                    unit
                                    systemEnv
                                    ports
                                    ioPorts
                                    TestEnvironment
                                        { teCntx = pTestCntx
                                        , teComputationDuration = fromEnum $ nextTick bnProcess
                                        }
                        , not $ null tbEnv
                        ]

                -- TODO 1S->T if possibe transform map fields to Text
                externalPortNames = concatMap (\(_tag, (is, os)) -> is ++ os) $ bnExternalPorts bnPus
                externalIO = T.intercalate ", " ("" : map (\p -> T.pack $ "." ++ p ++ "( " ++ p ++ " )") externalPortNames)
                envInitFlags = mapMaybe (uncurry testEnvironmentInitFlag) $ M.assocs bnPus

                tickWithTransfers =
                    map
                        ( \(cycleI, cycleCntx) ->
                            map
                                (\t -> (cycleI, t, cntxToTransfer cycleCntx t))
                                [0 .. nextTick bnProcess]
                        )
                        $ zip [0 :: Int ..] $ take cntxCycleNumber cntxProcess

                assertions = T.concat . map (\cycleTickTransfer -> posedgeCycle <> (T.concat . map assertion) cycleTickTransfer) $ tickWithTransfers

                assertion (cycleI, t, Nothing) =
                    codeLine [i|@(posedge clk); traceWithAttr(#{ cycleI }, #{ t }, net.data_bus, net.attr_bus);|]
                assertion (cycleI, t, Just (v, x)) =
                    codeLine [i|@(posedge clk); assertWithAttr(#{ cycleI }, #{ t }, net.data_bus, net.attr_bus, #{ dataLiteral x }, #{ attrLiteral x }, #{ v });|]
             in Immediate (moduleName pName n ++ "_tb.v") $
                    T.unpack $
                        codeBlock
                            [i|
            `timescale 1 ps / 1 ps
            module #{ moduleName pName n }_tb();

            /*
            Functions:
            #{ inline $ T.intercalate "\\n" $ map (T.pack . show) $ functions n }
            */

            /*
            Steps:
            #{ inline $ T.intercalate "\\n" $ map (T.pack . show) $ reverse $ steps $ process n }
            */

            // system signals
            reg clk, rst;
            wire cycle;

            // clk and rst generator
            #{ inline $ snippetClkGen }

            // vcd dump
            #{ inline $ snippetDumpFile $ T.pack $ moduleName pName n }



            ////////////////////////////////////////////////////////////
            // test environment

            // external ports (IO)
            #{ inline $ T.pack $ if null externalPortNames then "" else "wire " ++ S.join ", " externalPortNames ++ ";" }

            // initialization flags
            #{ if null envInitFlags then "" else "reg " <> S.join ", " envInitFlags <> ";" }
            assign env_init_flag = #{ defEnvInitFlag envInitFlags ioSync };

            #{ inline $ T.pack testEnv }



            ////////////////////////////////////////////////////////////
            // unit under test

            #{ moduleName pName n } #
                    ( .DATA_WIDTH( #{ dataWidth (def :: x) } )
                    , .ATTR_WIDTH( #{ attrWidth (def :: x) } )
                    ) net
                ( .clk( clk )
                , .rst( rst )
                , .flag_cycle_begin( cycle )
                #{ externalIO }
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
                    #{ inline assertions }
                    repeat ( #{ 2 * nextTick bnProcess } ) @(posedge clk);
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
            #{ inline $ T.pack $ verilogHelper (def :: x) }

            endmodule
            |]
            where
                defEnvInitFlag flags Sync = S.join " && " $ "1'b1" : flags
                defEnvInitFlag flags ASync = S.join " || " $ "1'b1" : flags
                defEnvInitFlag _flags OnBoard = error "can't generate testbench without specific IOSynchronization"

                cntxToTransfer cycleCntx t =
                    case extractInstructionAt n t of
                        Transport v _ _ : _ -> Just (v, getCntx cycleCntx v)
                        _ -> Nothing

                posedgeCycle =
                    codeBlock
                        [i|

                //-----------------------------------------------------------------
                @(posedge cycle);
                |]

isDrowAllowSignal Sync = bool2verilog False
isDrowAllowSignal ASync = bool2verilog True
isDrowAllowSignal OnBoard = "is_drop_allow"
