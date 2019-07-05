{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Model.Networks.Bus
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

Есть следующие подходы к реализации множественных сетей:

1. Сеть представляется в виде вычислительного блока жадно вычисляющая все функции привязанные к ней.
Как следствие, она должна содержать в себе некоторый фрагмент компилятора. Наружу, в качестве опций
выдаются исключительные внешние взаимодействиясети. Соответсвенно любая привязка функционального
блока может сократить количество вариантов внутри сети, что требует особой обработки при принятие
решения компилятором. Обвязка для передачи данных реализуется автоматически и рассматривается как
встроенная часть интерфейса вычислительного блока с сетью. Все сети становятся вложенными друг
относительно друга.
2. Все коммуникационные сети представляются  как единое целое, разделённое на домены.
При биндинге решаются задачи модификации прикладного алгоритма для передачи данных между доменами
(если надо). Планирование вычислительного процесса производится в рамках отдельных доменов, а также
относительно пересылок данных между ними, при этом время в сетях должно быть максимально выравнено.
Любая сетевая структура становится плоской с точки зрения наблюдателя.
-}
module NITTA.Model.Networks.Bus
    ( busNetwork
    , BusNetwork(..)
    , bindedFunctions, bindedVars
    ) where

import           Control.Monad.State
import qualified Data.Array                       as A
import           Data.Bits                        (FiniteBits (..))
import           Data.Default
import           Data.List                        (find, groupBy, nub, sortOn)
import qualified Data.Map                         as M
import           Data.Maybe                       (catMaybes, isJust, mapMaybe)
import qualified Data.Set                         as S
import qualified Data.String.Utils                as S
import           Data.Typeable
import           NITTA.Intermediate.Functions     (reg)
import           NITTA.Intermediate.Types
import           NITTA.Model.Networks.Types
import           NITTA.Model.Problems.Binding
import           NITTA.Model.Problems.Endpoint
import           NITTA.Model.Problems.Refactor
import           NITTA.Model.Problems.Transport
import           NITTA.Model.Problems.Types
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Model.Types
import           NITTA.Project.Implementation
import           NITTA.Project.Parts.TestBench
import           NITTA.Project.Snippets
import           NITTA.Project.Types
import           NITTA.UIBackend.VisJS            ()
import           NITTA.Utils
import           NITTA.Utils.Lens
import           NITTA.Utils.ProcessDescription
import           Numeric.Interval                 (inf, width, (...))
import           Text.InterpolatedString.Perl6    (qc)


data BusNetwork tag v x t = BusNetwork
    { -- | Список функциональных блоков привязанных к сети, но ещё не привязанных к конкретным
      -- вычислительным блокам.
      bnRemains        :: [F v x]
    -- | Таблица привязок функциональных блоков ко вложенным вычислительным блокам.
    , bnBinded         :: M.Map tag [F v x]
    -- | Описание вычислительного процесса сети, как элемента процессора.
    , bnProcess        :: Process v x t
    -- | Словарь вложенных вычислительных блоков по именам.
    , bnPus            :: M.Map tag (PU v x t)
    -- | Ширина шины управления.
    , bnSignalBusWidth :: Int
    -- |Why Maybe? If Just : hardcoded parameter; if Nothing - connect to @is_drop_allow@ wire.
    , bnAllowDrop      :: Maybe Bool
    , bnEnv            :: TargetEnvironment
    , bnPorts          :: Ports (BusNetwork tag v x t)
    }


-- TODO: Проверка подключения сигнальных линий.

-- TODO: Вариант функции, где провода будут подключаться автоматически.
busNetwork w bnAllowDrop pus = BusNetwork
        { bnRemains=[]
        , bnBinded=M.empty
        , bnProcess=def
        , bnPus=M.fromList pus'
        , bnSignalBusWidth=w
        , bnAllowDrop
        , bnPorts=NetPorts{ extInputs, extOutputs }
        , bnEnv
        }
    where
        bnEnv = TargetEnvironment
            { signalClk="clk"
            , signalRst="rst"
            , signalCycle="cycle"
            , inputPort= \(InputPortTag n) -> n
            , outputPort= \(OutputPortTag n) -> n
            , unitEnv=NetworkEnv
            }
        puEnv tag = bnEnv
            { unitEnv=ProcessUnitEnv
                { parameterAttrWidth=InlineParam "ATTR_WIDTH"
                , dataIn="data_bus"
                , dataOut=tag ++ "_data_out"
                , attrIn="attr_bus"
                , attrOut=tag ++ "_attr_out"
                , signal= \(SignalTag i) -> "control_bus[" ++ show i ++ "]"
                }
            }
        pus' = map (\(tag, f) -> ( tag, f $ puEnv tag ) ) pus
        extInputs=nub $ concatMap (\(_, PU{ ports }) -> externalInputPorts ports ) pus'
        extOutputs=nub $ concatMap (\(_, PU{ ports }) -> externalOutputPorts ports ) pus'

instance WithFunctions (BusNetwork tag v x t) (F v x) where
    functions BusNetwork{ bnRemains, bnBinded } = bnRemains ++ concat (M.elems bnBinded)

instance ( UnitTag tag, VarValTime v x t
         ) => DecisionProblem (DataFlowDT tag v t)
                   DataFlowDT (BusNetwork tag v x t)
    where
    options _proxy BusNetwork{ bnPus, bnProcess }
        = notEmptyDestination $ concat
            [ map (DataFlowO (source, fixConstrain pullAt)) $ targetOptionsFor $ S.elems vars
            | (source, opts) <- puOptions
            , EndpointO (Source vars) pullAt <- opts
            ]
        where
            puOptions = M.assocs $ M.map (options endpointDT) bnPus
            targetOptionsFor vs = let
                    conflictableTargets =
                        [ (pushVar, Just (target, fixConstrain pushAt))
                        | (target, opts) <- puOptions
                        , EndpointO (Target pushVar) pushAt <- opts
                        , pushVar `elem` vs
                        ]
                    targets = sequence $ groupBy (\a b -> tgr a == tgr b) $ sortOn tgr conflictableTargets
                    zero = zip vs $ repeat Nothing
                in map (M.fromList . (++) zero) targets

            fixConstrain constrain
                = let
                    a = max (nextTick bnProcess) $ constrain^.avail.infimum
                    b = constrain^.avail.supremum
                in constrain & avail .~ (a ... b)

            notEmptyDestination = filter $ \DataFlowO{ dfoTargets } -> any isJust $ M.elems dfoTargets
            tgr (_, Just (target, _)) = Just target
            tgr _                     = Nothing

    decision _proxy n@BusNetwork{ bnProcess, bnPus } d@DataFlowD{ dfdSource=( srcTitle, pullAt ), dfdTargets }
        | nextTick bnProcess > d^.at.infimum
        = error $ "BusNetwork wraping time! Time: " ++ show (nextTick bnProcess) ++ " Act start at: " ++ show (d^.at)
        | otherwise
        = let
            pushs = M.fromList $ catMaybes
                $ map (\case
                    (k, Just v) -> Just (k,  v)
                    (_, Nothing) -> Nothing
                ) $ M.assocs dfdTargets
            transportStartAt = d^.at.infimum
            transportDuration = maximum $ map (\(_trg, time) -> (inf time - transportStartAt) + width time) $ M.elems pushs
            transportEndAt = transportStartAt + transportDuration

            subDecisions = ( srcTitle, EndpointD (Source $ S.fromList $ M.keys pushs) pullAt )
                        :   [ ( trgTitle, EndpointD (Target v) pushAt )
                            | (v, (trgTitle, pushAt)) <- M.assocs pushs
                            ]
        in n
            { bnPus=foldl applyDecision bnPus subDecisions
            , bnProcess=snd $ modifyProcess bnProcess $ do
                mapM_
                    (\(pushedValue, (targetTitle, _tc)) -> addStep
                        (Activity $ transportStartAt ... transportEndAt)
                        $ InstructionStep (Transport pushedValue srcTitle targetTitle :: Instruction (BusNetwork tag v x t))
                    )
                    $ M.assocs pushs
                addStep_ (Activity $ transportStartAt ... transportEndAt) $ CADStep $ show d
                setProcessTime $ d^.at.supremum + 1
            }
        where
            applyDecision pus (trgTitle, d') = M.adjust (\pu -> decision endpointDT pu d') trgTitle pus



instance ( UnitTag tag, VarValTime v x t
         ) => ProcessorUnit (BusNetwork tag v x t) v x t where

    tryBind f net@BusNetwork{ bnRemains, bnPus }
        | any (allowToProcess f) $ M.elems bnPus
        = Right net{ bnRemains=f : bnRemains }
    tryBind f BusNetwork{ bnPus }
        = Left $ "All sub process units reject the functional block: " ++ show f ++ "\n" ++ rejects
        where
            rejects = S.join "\n" $ map showReject $ M.assocs bnPus
            showReject (tag, pu) | Left err <- tryBind f pu = "    [" ++ show tag ++ "]: " ++ err
            showReject (tag, _) = "    [" ++ show tag ++ "]: undefined"


    process net@BusNetwork{ bnProcess, bnPus }
        = let
            v2transportStepUid = M.fromList
                [ (v, sKey)
                | Step{ sKey, sDesc } <- steps bnProcess
                , isInstruction sDesc
                , v <- case sDesc of
                    (InstructionStep i)
                        | Just (Transport var _ _) <- cast i `maybeInstructionOf` net
                        -> [var]
                    _ -> []
                ]
        in execScheduleWithProcess net bnProcess $ do
            -- Копируем нижележащие процессы наверх.
            mapM_ addNestedProcess $ sortOn fst $ M.assocs bnPus

            Process{ steps } <- getProcessSlice
            -- Transport - Endpoint
            let low = concatMap (\Step{ sKey, sDesc } ->
                    case sDesc of
                        NestedStep{ nStep=Step{ sDesc=EndpointRoleStep role } } -> [ (sKey, v) | v <- S.elems $ variables role ]
                        _ -> []
                    ) steps
            mapM_
                ( \(l, v) ->
                    when (v `M.member` v2transportStepUid)
                        $ establishVerticalRelation (v2transportStepUid M.! v) l )
                low
            -- FB - Transport
            mapM_ ( \Step{ sKey, sDesc=NestedStep{ nStep=Step{ sDesc=FStep f } } } ->
                    mapM_ ( \v ->
                            when (v `M.member` v2transportStepUid)
                                $ establishVerticalRelation sKey (v2transportStepUid M.! v) )
                        $ variables f )
                $ filter isFB steps
        where
            addNestedProcess (tag, pu) = do
                let Process{ steps, relations } = process pu
                uidDict <- M.fromList <$> mapM
                    ( \step@Step{ sKey } -> do
                        sKey' <- scheduleNestedStep tag step
                        return (sKey, sKey') )
                    steps
                mapM_ (\(Vertical h l) -> establishVerticalRelation (uidDict M.! h) (uidDict M.! l)) relations

    setTime t net@BusNetwork{..} = net
        { bnProcess=bnProcess{ nextTick=t }
        , bnPus=M.map (setTime t) bnPus
        }



instance Controllable (BusNetwork tag v x t) where
    data Instruction (BusNetwork tag v x t)
        = Transport v tag tag
        deriving (Typeable, Show)

    data Microcode (BusNetwork tag v x t)
        = BusNetworkMC (A.Array SignalTag SignalValue)

    -- Right now, BusNetwork don't have external control (exclude rst signal and some hacks). All
    -- signals starts and ends inside network unit.
    mapMicrocodeToPorts BusNetworkMC{} NetPorts{} = []


instance {-# OVERLAPS #-}
        ByTime (BusNetwork tag v x t) t where
    microcodeAt BusNetwork{..} t
        = BusNetworkMC $ foldl merge initSt $ M.elems bnPus
        where
            initSt = A.listArray (SignalTag 0, SignalTag $ bnSignalBusWidth - 1) $ repeat def
            merge st PU{ unit, ports }
                = foldl merge' st $ mapMicrocodeToPorts (microcodeAt unit t) ports
            merge' st (s, x) = st A.// [ (s, st A.! s +++ x) ]



instance ( UnitTag tag ) => Simulatable (BusNetwork tag v x t) v x where
    simulateOn cntx BusNetwork{..} fb
        = let
            Just (tag, _) = find (\(_, v) -> fb `elem` v) $ M.assocs bnBinded
            pu = bnPus M.! tag
        in simulateOn cntx pu fb



----------------------------------------------------------------------


-- Функция позволяет проанализировать сеть и получить наружу варианты для управления привязками
-- функциональных блоков к вычислительным блокам. В текущем виде место для данной функции не
-- определено, так как:
--
-- 1. В случае если сеть выступает в качестве вычислительного блока, то она должна инкапсулировать
--    в себя эти настройки (но не hardcode-ить).
-- 2. Эти функции должны быть представленны классом типов.
instance ( UnitTag tag, VarValTime v x t ) =>
        DecisionProblem (BindingDT tag v x)
              BindingDT (BusNetwork tag v x t)
        where
    options _ BusNetwork{ bnRemains, bnPus } = concatMap optionsFor bnRemains
        where
            optionsFor f =
                [ BindingO f puTitle
                | ( puTitle, pu ) <- M.assocs bnPus
                , allowToProcess f pu
                ]

    decision _ bn@BusNetwork{ bnProcess=p@Process{..}, ..} (BindingD fb puTitle)
        = bn
            { bnPus=M.adjust (bind fb) puTitle bnPus
            , bnBinded=M.alter
                (\case  Just fbs -> Just $ fb : fbs
                        Nothing  -> Just [fb]
                ) puTitle bnBinded
            , bnProcess=snd $ modifyProcess p $
                addStep (Event nextTick) $ CADStep $ "Bind " ++ show fb ++ " to " ++ show puTitle
            , bnRemains=filter (/= fb) bnRemains
        }



instance ( UnitTag tag, VarValTime v x t
        ) => DecisionProblem (RefactorDT v)
                  RefactorDT (BusNetwork tag v x t)
        where
    options _ bn
        = nub
            [ InsertOutRegisterO lockBy
            | (BindingO f tag) <- options binding bn
            , Lock{ lockBy } <- locks f
            , lockBy `S.member` unionsMap variables (bindedFunctions tag bn)
            ]

    decision _ bn@BusNetwork{ bnRemains } (InsertOutRegisterD v v')
        = bn{ bnRemains=reg v [v'] : patch (v, v') bnRemains }



bindedVars :: ( Var v ) => BusNetwork tag v x t -> S.Set v
bindedVars BusNetwork{ bnBinded } = unionsMap variables $ concat $ M.elems bnBinded

bindedFunctions puTitle BusNetwork{ bnBinded }
    | puTitle `M.member` bnBinded = bnBinded M.! puTitle
    | otherwise = []


--------------------------------------------------------------------------

programTicks BusNetwork{ bnProcess=Process{ nextTick } } = [ -1 .. nextTick ]

externalPorts pus = M.assocs $ M.map (
        \PU{ ports } ->
            ( map inputPortTag $ externalInputPorts ports
            , map outputPortTag $ externalOutputPorts ports
            )
    ) pus

externalPortsDecl ports = S.join "\n" $ concatMap (
        \(tag, (is, os)) ->
            ("    // external ports for: " ++ tag)
            :  map ("        , input " ++) is
            ++ map ("        , output " ++) os
    ) ports


instance ( VarValTime v x t
        ) => TargetSystemComponent (BusNetwork String v x t) where
    moduleName tag BusNetwork{..} = tag ++ "_net"

    hardware tag pu@BusNetwork{..}
        = let
            (instances, valuesRegs) = renderInstance [] [] $ M.assocs bnPus
            mn = moduleName tag pu
            iml = fixIndent [qc|
|                   {"module"} { mn }
|                       #( parameter DATA_WIDTH = { finiteBitSize (def :: x) }
|                        , parameter ATTR_WIDTH = 4
|                        )
|                       ( input                     clk
|                       , input                     rst
|                       , output                    cycle
|                   { externalPortsDecl $ externalPorts bnPus }
|                       , output              [7:0] debug_status
|                       , output              [7:0] debug_bus1
|                       , output              [7:0] debug_bus2
|                       , input                     is_drop_allow
|                       );
|
|                   parameter MICROCODE_WIDTH = { bnSignalBusWidth };
|                   // Sub module_ instances
|                   wire [MICROCODE_WIDTH-1:0] control_bus;
|                   wire [DATA_WIDTH-1:0] data_bus;
|                   wire [ATTR_WIDTH-1:0] attr_bus;
|                   wire start, stop;
|
|                   wire [7:0] debug_pc;
|                   assign debug_status = \{ cycle, debug_pc[6:0] };
|                   assign debug_bus1 = data_bus[7:0];
|                   assign debug_bus2 = data_bus[31:24] | data_bus[23:16] | data_bus[15:8] | data_bus[7:0];
|
|                   pu_simple_control
|                       #( .MICROCODE_WIDTH( MICROCODE_WIDTH )
|                        , .PROGRAM_DUMP( "$path${ mn }.dump" )
|                        , .MEMORY_SIZE( { length $ programTicks pu } ) // 0 - address for nop microcode
|                        ) control_unit
|                       ( .clk( clk )
|                       , .rst( rst )
|                       , .start_cycle( { maybe "is_drop_allow" bool2verilog bnAllowDrop } || stop )
|                       , .cycle( cycle )
|                       , .signals_out( control_bus )
|                       , .trace_pc( debug_pc )
|                       );
|
|                   { S.join "\\n\\n" instances }
|
|                   assign data_bus = { S.join " | " $ map snd valuesRegs };
|                   assign attr_bus = { S.join " | " $ map fst valuesRegs };
|
|                   endmodule
|                   |]
        in Aggregate (Just mn) $
            [ Immediate (mn ++ ".v") iml
            , FromLibrary "pu_simple_control.v"
            ] ++ map (uncurry hardware) (M.assocs bnPus)
        where
            regInstance (t :: String)
                = fixIndent [qc|
|                   wire [DATA_WIDTH-1:0] {t}_data_out;
|                   wire [ATTR_WIDTH-1:0] {t}_attr_out;
|                   |]

            renderInstance insts regs [] = ( reverse insts, reverse regs )
            renderInstance insts regs ((t, PU{ unit, systemEnv, ports }) : xs)
                = let
                    inst = hardwareInstance t unit systemEnv ports
                    insts' = inst : regInstance t : insts
                    regs' = (t ++ "_attr_out", t ++ "_data_out") : regs
                in renderInstance insts' regs' xs

    software tag pu@BusNetwork{ bnProcess=Process{..}, ..}
        = let
            subSW = map (uncurry software) (M.assocs bnPus)
            sw = [ Immediate (mn ++ ".dump") memoryDump ]
        in Aggregate (Just mn) $ subSW ++ sw
        where
            mn = moduleName tag pu
            -- По нулевоу адресу устанавливается команда Nop (он же def) для всех вычислиетльных блоков.
            -- Именно этот адрес выставляется на сигнальные линии когда поднят сигнал rst.
            memoryDump = unlines $ map ( values2dump . values . microcodeAt pu ) $ programTicks pu
            values (BusNetworkMC arr) = reverse $ A.elems arr

    hardwareInstance tag BusNetwork{} TargetEnvironment{ unitEnv=NetworkEnv, signalClk, signalRst } bnPorts
        | let
            io2v n = "    , " ++ n ++ "( " ++ n ++ " )"
            is = map io2v $ map (\(InputPortTag n) -> n) $ externalInputPorts bnPorts
            os = map io2v $ map (\(OutputPortTag n) -> n) $ externalOutputPorts bnPorts
        = fixIndent [qc|
|           { tag } #
|                   ( .DATA_WIDTH( { finiteBitSize (def :: x) } )
|                   , .ATTR_WIDTH( 4 )
|                   ) net
|               ( .rst( { signalRst } )
|               , .clk( { signalClk } )
|               // inputs:
|           { S.join "\\n" is }
|               // outputs:
|           { S.join "\\n" os }
|               , .debug_status( debug_status ) // FIXME:
|               , .debug_bus1( debug_bus1 )     // FIXME:
|               , .debug_bus2( debug_bus2 )     // FIXME:
|               , .is_drop_allow( rendezvous )  // FIXME:
|               );
|           |]
    hardwareInstance _title _bn TargetEnvironment{ unitEnv=ProcessUnitEnv{} } _bnPorts
        = error "BusNetwork should be NetworkEnv"


instance Connected (BusNetwork tag v x t) where
    data Ports (BusNetwork tag v x t)
        = NetPorts
            { extInputs :: [ InputPortTag ]
            , extOutputs :: [ OutputPortTag ]
            }
        deriving ( Show )
    externalInputPorts = extInputs
    externalOutputPorts = extOutputs



instance ( VarValTime v x t
         , TargetSystemComponent (BusNetwork String v x t)
         ) => Testable (BusNetwork String v x t) v x where
    testBenchImplementation
                Project
                    { pName
                    , pUnit=n@BusNetwork{ bnProcess, bnPus, bnAllowDrop }
                    , pTestCntx=pTestCntx@Cntx{ cntxProcess, cntxCycleNumber }
                    } = let
            testEnv = S.join "\\n\\n"
                [ tbEnv
                | (t, PU{ unit, systemEnv, ports }) <- M.assocs bnPus
                , let t' = filter (/= '"') $ show t
                , let tbEnv = testEnvironment t' unit systemEnv ports pTestCntx
                , not $ null tbEnv
                ]

            externalPortNames = concatMap ( \(_tag, (is, os)) -> is ++ os ) $ externalPorts bnPus
            externalIO = S.join ", " ("" : map (\p -> "." ++ p ++ "( " ++ p ++ " )") externalPortNames)
            envInitFlags = mapMaybe (uncurry testEnvironmentInitFlag) $ M.assocs bnPus

            tickWithTransfers = map
                ( \cycleCntx -> map
                     ( \t -> ( t, cntxToTransfer cycleCntx t ) )
                     [ 0 .. nextTick bnProcess ] )
                $ take cntxCycleNumber cntxProcess

            assertions = concatMap ( \cycleTransfers -> posedgeCycle ++ concatMap assertion cycleTransfers ) tickWithTransfers

            assertion ( t, Nothing ) = fixIndentNoLn [qc|
|                       @(posedge clk); $write("tick: { t };\tnet.data_bus == %h ", net.data_bus);
|                       $display();
|               |]
            assertion ( t, Just (v, x) ) = fixIndentNoLn [qc|
|                       @(posedge clk); $write("tick: { t };\tnet.data_bus == %h ", net.data_bus);
|                           $write("=== %h (var: %s := { x })", { verilogInteger $ x }, { v } );
|                           if ( !( net.data_bus === { verilogInteger $ x } ) ) $display( "\tFAIL");
|                           else $display();
|               |]

        in Immediate (moduleName pName n ++ "_tb.v") $ fixIndent [qc|
|               `timescale 1 ps / 1 ps
|               {"module"} { moduleName pName n }_tb();
|
|               /*
|               Functions:
|               { S.join "\\n" $ map show $ functions n }
|
|               Steps:
|               { S.join "\\n" $ map show $ reverse $ steps $ process n }
|               */
|
|               reg clk, rst;
|               { if null externalPortNames then "" else "wire " ++ S.join ", " externalPortNames ++ ";" }
|
|               wire cycle;
|
|               // test environment initialization flags
|               reg { S.join ", " envInitFlags };
|               assign envInitFlag = { S.join " && " $ "1'b1" : envInitFlags };
|
|               { moduleName pName n }
|                   #( .DATA_WIDTH( { finiteBitSize (def :: x) } )
|                    , .ATTR_WIDTH( 4 )
|                    ) net
|                   ( .clk( clk )
|                   , .rst( rst )
|                   , .cycle( cycle )
|                   { externalIO }
|                   // if 1 - The process cycle are indipendent from a SPI.
|                   // else - The process cycle are wait for the SPI.
|                   , .is_drop_allow( { maybe "is_drop_allow" bool2verilog bnAllowDrop } )
|                   );
|
|               { testEnv }
|
|               { snippetDumpFile $ moduleName pName n }
|
|               { snippetClkGen }
|
|               initial
|                   begin
|                       // microcode when rst == 1 -> program[0], and must be nop for all PUs
|                       @(negedge rst); // Turn mUnit on.
|                       // Start computational cycle from program[1] to program[n] and repeat.
|                       // Signals effect to mUnit state after first clk posedge.
|                       @(posedge clk);
|                       while (!envInitFlag) @(posedge clk);
|{ assertions }
|                       repeat ( 2000 ) @(posedge clk);
|                       $finish;
|                   end
|
|               endmodule
|               |]
        where
            cntxToTransfer cycleCntx t
                = case extractInstructionAt n t of
                    Transport v _ _ : _ -> Just (v, either error id $ getX cycleCntx v)
                    _                   -> Nothing

            posedgeCycle = fixIndent [qc|
|
|                       //-----------------------------------------------------------------
|                       @(posedge cycle);
|               |]
