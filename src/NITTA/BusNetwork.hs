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
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

{-
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
module NITTA.BusNetwork where

import           Control.Monad.State
import qualified Data.Array                    as A
import           Data.Default
import           Data.List                     (find, nub, partition, sortOn,
                                                (\\))
import qualified Data.Map                      as M
import           Data.Maybe                    (fromMaybe, isJust, mapMaybe)
import           Data.Set                      (elems, fromList, intersection)
import qualified Data.String.Utils             as S
import           Data.Typeable
import           NITTA.Functions               (get', simulateAlgByCycle)
import           NITTA.Project
import           NITTA.Types
import           NITTA.Types.Project
import           NITTA.Utils
import           NITTA.Utils.Lens
import           NITTA.Utils.Process
import           Numeric.Interval              (inf, width, (...))
import           Text.InterpolatedString.Perl6 (qc)


-- | Класс идентификатора вложенного вычислительного блока.
type Title v = ( Typeable v, Ord v, Show v )




data GBusNetwork title spu v x t = BusNetwork
    { -- | Список функциональных блоков привязанных к сети, но ещё не привязанных к конкретным
      -- вычислительным блокам.
      bnRemains        :: [F v x]
    -- | Таблица привязок функциональных блоков ко вложенным вычислительным блокам.
    , bnBinded         :: M.Map title [F v x]
    -- | Описание вычислительного процесса сети, как элемента процессора.
    , bnProcess        :: Process v x t
    -- | Словарь вложенных вычислительных блоков по именам.
    , bnPus            :: M.Map title spu
    -- | Ширина шины управления.
    , bnSignalBusWidth :: Int
    , bnInputPorts     :: [InputPort]
    , bnOutputPorts    :: [OutputPort]
    -- |Why Maybe? If Just : hardcoded parameter; if Nothing - connect to @is_drop_allow@ wire.
    , bnAllowDrop      :: Maybe Bool
    }
type BusNetwork title v x t = GBusNetwork title (PU v x t) v x t


-- TODO: Проверка подключения сигнальных линий.

-- TODO: Вариант функции, где провода будут подключаться автоматически.
busNetwork w allowDrop ips ops pus = BusNetwork
        { bnRemains=[]
        , bnBinded=M.fromList []
        , bnProcess=def
        , bnPus=M.fromList pus'
        , bnSignalBusWidth=w
        , bnInputPorts=ips
        , bnOutputPorts=ops
        , bnAllowDrop=allowDrop
        }
    where
        pus' = map (\(title, f) ->
            ( title
            , f Enviroment
                { signalClk="clk"
                , signalRst="rst"
                , signalCycle="cycle"
                , inputPort= \(InputPort n) -> n
                , outputPort= \(OutputPort n) -> n
                , net=NetEnv
                    { parameterAttrWidth=InlineParam "ATTR_WIDTH"
                    , dataIn="data_bus"
                    , dataOut=title ++ "_data_out"
                    , attrIn="attr_bus"
                    , attrOut=title ++ "_attr_out"
                    , signal= \(Signal i) -> "control_bus[" ++ show i ++ "]"
                    }
                })
            ) pus

instance WithX (BusNetwork title v x t) x

instance ( Title title
         , Time t
         , Var v
         , Typeable x
         ) => WithFunctions (BusNetwork title v x t) (F v x) where
    functions BusNetwork{..} = sortFBs binded []
        where
            binded = bnRemains ++ concat (M.elems bnBinded)
            sortFBs [] _ = []
            sortFBs fbs cntx
                = let (ready, notReady) = partition (\fb -> insideOut fb || all (`elem` cntx) (inputs fb)) fbs
                in case ready of
                    [] -> error "Cycle in algorithm!"
                    _  -> ready ++ sortFBs notReady (elems (unionsMap outputs ready) ++ cntx)


instance ( Title title, Var v, Time t
         , Typeable x
         ) => DecisionProblem (DataFlowDT title v t)
                   DataFlowDT (BusNetwork title v x t)
    where
    options _proxy n@BusNetwork{..}
        = notEmptyDestination $ concat
            [
                [ DataFlowO (srcTitle, fixPullConstrain pullAt) $ M.fromList pushs
                | pushs <- mapM pushOptionsFor $ elems pullVars
                , let pushTo = mapMaybe (fmap fst . snd) pushs
                , length (nub pushTo) == length pushTo
                ]
            | (srcTitle, opts) <- puOptions
            , EndpointO (Source pullVars) pullAt <- opts
            ]
        where
            notEmptyDestination = filter $ \DataFlowO{ dfoTargets } -> any isJust $ M.elems dfoTargets
            now = nextTick bnProcess
            fixPullConstrain constrain
                = let
                    a = max now $ constrain^.avail.infimum
                    b = constrain^.avail.supremum
                in constrain & avail .~ (a ... b)

            pushOptionsFor v | v `notElem` availableVars = [(v, Nothing)]
            pushOptionsFor v = (v, Nothing) : pushOptionsFor' v

            pushOptionsFor' v = [ (v, Just (pushTo, pushAt))
                                --   | (pushTo, vars) <- trace ("\n==========" ++ S.join "\n" (map show puOptions)) puOptions
                                | (pushTo, vars) <- puOptions
                                , EndpointO (Target pushVar) pushAt <- vars
                                , pushVar == v
                                ]
            bnForwardedVariables = transfered n
            availableVars
                = let
                    fbs = bnRemains ++ concat (M.elems bnBinded)
                    alg = foldl
                        (\dict Lock{ locked=a, lockBy=b } -> M.adjust ((:) b) a dict)
                        (M.fromList [(v, []) | v <- elems $ unionsMap variables fbs])
                        $ filter (\Lock{ lockBy } -> lockBy `notElem` bnForwardedVariables)
                        $ concatMap locks fbs ++ concatMap locks (M.elems bnPus)
                    notBlockedVariables = map fst $ filter (null . snd) $ M.assocs alg
                in notBlockedVariables \\ bnForwardedVariables

            puOptions = M.assocs $ M.map (options endpointDT) bnPus

    decision _proxy n@BusNetwork{ bnProcess, bnPus } d@DataFlowD{ dfdSource=( srcTitle, pullAt ), dfdTargets }
        | nextTick bnProcess > d^.at.infimum
        = error $ "BusNetwork wraping time! Time: " ++ show (nextTick bnProcess) ++ " Act start at: " ++ show (d^.at)
        | otherwise
        = let
            pushs = M.map (fromMaybe undefined) $ M.filter isJust dfdTargets
            transportStartAt = d^.at.infimum
            transportDuration = maximum $ map (\(_trg, time) -> (inf time - transportStartAt) + width time) $ M.elems pushs
            transportEndAt = transportStartAt + transportDuration

            subDecisions = ( srcTitle, EndpointD (Source $ fromList $ M.keys pushs) pullAt )
                        :   [ ( trgTitle, EndpointD (Target v) pushAt )
                            | (v, (trgTitle, pushAt)) <- M.assocs pushs
                            ]
        in n
            { bnPus=foldl applyDecision bnPus subDecisions
            , bnProcess=snd $ modifyProcess bnProcess $ do
                mapM_
                    (\(pushedValue, (targetTitle, _tc)) -> addStep
                        (Activity $ transportStartAt ... transportEndAt)
                        $ InstructionStep (Transport pushedValue srcTitle targetTitle :: Instruction (BusNetwork title v x t))
                    )
                    $ M.assocs pushs
                addStep_ (Activity $ transportStartAt ... transportEndAt) $ CADStep $ show d
                setProcessTime $ d^.at.supremum + 1
            }
        where
            applyDecision pus (trgTitle, d') = M.adjust (\pu -> decision endpointDT pu d') trgTitle pus



instance ( Title title, Time t, Var v, Typeable x
         ) => ProcessUnit (BusNetwork title v x t) v x t where

    tryBind f net@BusNetwork{ bnRemains, bnPus }
        | any (allowToProcess f) $ M.elems bnPus
        = Right net{ bnRemains=f : bnRemains }
    tryBind f BusNetwork{ bnPus }
        = Left $ "All sub process units reject the functional block: " ++ show f ++ "\n" ++ rejects
        where
            rejects = S.join "\n" $ map showReject $ M.assocs bnPus
            showReject (title, pu) | Left err <- tryBind f pu = "    [" ++ show title ++ "]: " ++ err
            showReject (title, _) = "    [" ++ show title ++ "]: undefined"


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
                        NestedStep{ nStep=Step{ sDesc=EndpointRoleStep role } } -> [ (sKey, v) | v <- elems $ variables role ]
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
            addNestedProcess (title, pu) = do
                let Process{ steps, relations } = process pu
                uidDict <- M.fromList <$> mapM
                    ( \step@Step{ sKey } -> do
                        sKey' <- scheduleNestedStep title step
                        return (sKey, sKey') )
                    steps
                mapM_ (\(Vertical h l) -> establishVerticalRelation (uidDict M.! h) (uidDict M.! l)) relations

    setTime t net@BusNetwork{..} = net
        { bnProcess=bnProcess{ nextTick=t }
        , bnPus=M.map (setTime t) bnPus
        }



instance Controllable (BusNetwork title v x t) where
    data Instruction (BusNetwork title v x t)
        = Transport v title title
        deriving (Typeable, Show)

    data Microcode (BusNetwork title v x t)
        = BusNetworkMC (A.Array Signal SignalValue)


instance {-# OVERLAPS #-}
         ( Time t
         ) => ByTime (BusNetwork title v x t) t where
    microcodeAt BusNetwork{..} t
        = BusNetworkMC $ foldl merge initSt $ M.elems bnPus
        where
            initSt = A.listArray (Signal 0, Signal $ bnSignalBusWidth - 1) $ repeat def
            merge st PU{ unit, links }
                = foldl merge' st $ transmitToLink (microcodeAt unit t) links
            merge' st (s, x) = st A.// [ (s, st A.! s +++ x) ]



instance ( Title title, Var v, Time t ) => Simulatable (BusNetwork title v x t) v x where
    simulateOn cntx BusNetwork{..} fb
        = let
            Just (title, _) = find (\(_, v) -> fb `elem` v) $ M.assocs bnBinded
            pu = bnPus M.! title
        in simulateOn cntx pu fb



----------------------------------------------------------------------


-- | Функция позволяет проанализировать сеть и получить наружу варианты для управления привязками
-- функциональных блоков к вычислительным блокам. В текущем виде место для данной функции не
-- определено, так как:
--
-- 1. В случае если сеть выступает в качестве вычислительного блока, то она должна инкапсулировать
--    в себя эти настройки (но не hardcode-ить).
-- 2. Эти функции должны быть представленны классом типов.
instance ( Var v
         , Typeable x
         ) => DecisionProblem (BindingDT String v x)
                    BindingDT (BusNetwork String v x t)
         where
    options _ BusNetwork{..} = concatMap bindVariants' bnRemains
        where
            bindVariants' fb =
                [ BindingO fb puTitle
                | (puTitle, pu) <- sortOn (length . binded . fst) $ M.assocs bnPus
                , allowToProcess fb pu
                , not $ selfTransport fb puTitle
                ]

            selfTransport fb puTitle =
                not $ null $ variables fb `intersection` unionsMap variables (binded puTitle)

            binded puTitle
                | puTitle `M.member` bnBinded = bnBinded M.! puTitle
                | otherwise = []

    decision _ bn@BusNetwork{ bnProcess=p@Process{..}, ..} (BindingD fb puTitle)
        = bn
            { bnPus=M.adjust (bind fb) puTitle bnPus
            , bnBinded=M.alter
                (\case  Just fbs -> Just $ fb : fbs
                        Nothing  -> Just [fb]
                ) puTitle bnBinded
            , bnProcess=snd $ modifyProcess p $
                addStep (Event nextTick) $ CADStep $ "Bind " ++ show fb ++ " to " ++ puTitle
            , bnRemains=filter (/= fb) bnRemains
        }


--------------------------------------------------------------------------

programTicks BusNetwork{ bnProcess=Process{ nextTick } } = [ -1 .. nextTick ]


instance
        ( Time t
        , Val x
        , Var v
        ) => TargetSystemComponent (BusNetwork String v x t) where
    moduleName title BusNetwork{..} = title ++ "_net"

    hardware title pu@BusNetwork{..}
        = let
            (instances, valuesRegs) = renderInstance [] [] $ M.assocs bnPus
            mn = moduleName title pu
            iml = fixIndent [qc|
|                   {"module"} { mn }
|                       #( parameter DATA_WIDTH = { widthX pu }
|                        , parameter ATTR_WIDTH = 4
|                        )
|                       ( input                     clk
|                       , input                     rst
|                       , output                    cycle
|                   { S.join "\\n" $ map (\\(InputPort p) -> ("    , input " ++ p)) bnInputPorts }
|                   { S.join "\\n" $ map (\\(OutputPort p) -> ("    , output " ++ p)) bnOutputPorts }
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
|                   wire   start, stop;
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
            [ Immidiate (mn ++ ".v") iml
            , FromLibrary "pu_simple_control.v"
            ] ++ map (uncurry hardware) (M.assocs bnPus)
        where
            regInstance (t :: String)
                = fixIndent [qc|
|                   wire [DATA_WIDTH-1:0] {t}_data_out;
|                   wire [ATTR_WIDTH-1:0] {t}_attr_out;
|                   |]

            renderInstance insts regs [] = ( reverse insts, reverse regs )
            renderInstance insts regs ((t, PU{ unit, systemEnv, links }) : xs)
                = let
                    inst = hardwareInstance t unit systemEnv links
                    insts' = inst : regInstance t : insts
                    regs' = (t ++ "_attr_out", t ++ "_data_out") : regs
                in renderInstance insts' regs' xs

    software title pu@BusNetwork{ bnProcess=Process{..}, ..}
        = let
            subSW = map (uncurry software) (M.assocs bnPus)
            sw = [ Immidiate (mn ++ ".dump") memoryDump ]
        in Aggregate (Just mn) $ subSW ++ sw
        where
            mn = moduleName title pu
            -- По нулевоу адресу устанавливается команда Nop (он же def) для всех вычислиетльных блоков.
            -- Именно этот адрес выставляется на сигнальные линии когда поднят сигнал rst.
            memoryDump = unlines $ map ( values2dump . values . microcodeAt pu ) $ programTicks pu
            values (BusNetworkMC arr) = reverse $ A.elems arr

    hardwareInstance = undefined


instance ( Title title, Var v, Time t
         , Show x, Enum x
         , TargetSystemComponent (BusNetwork title v x t)
         , Typeable x, Val x
         ) => TestBench (BusNetwork title v x t) v x where
    testBenchDescription Project{ projectName, processorModel=n@BusNetwork{..}, testCntx }
        = Immidiate (moduleName projectName n ++ "_tb.v") testBenchImp
        where
            ports = map (\(InputPort n') -> n') bnInputPorts ++ map (\(OutputPort n') -> n') bnOutputPorts
            testEnv = S.join "\\n\\n"
                [ tbEnv
                | (t, PU{ unit, systemEnv, links }) <- M.assocs bnPus
                , let t' = filter (/= '"') $ show t
                , let tbEnv = componentTestEnviroment t' unit systemEnv links
                , not $ null tbEnv
                ]
            externalIO = S.join ", " ("" : map (\p -> "." ++ p ++ "( " ++ p ++ " )") ports)
            testBenchImp = fixIndent [qc|
|               `timescale 1 ps / 1 ps
|               {"module"} { moduleName projectName n }_tb();
|
|               /* Functions:
|               { S.join "\\n" $ map show $ functions n }
|               */
|
|               /* Steps:
|               { S.join "\\n" $ map show $ reverse $ steps $ process n }
|               */
|
|               reg clk, rst;
|               { if null ports then "" else "wire " ++ S.join ", " ports ++ ";" }
|
|               wire cycle;
|
|               { moduleName projectName n }
|                   #( .DATA_WIDTH( { widthX n } )
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
|               { snippetDumpFile $ moduleName projectName n }
|
|               { snippetClkGen }
|
|               initial
|                   begin
|                       // microcode when rst == 1 -> program[0], and must be nop for all PUs
|                       @(negedge rst); // Turn processor on.
|                       // Start computational cycle from program[1] to program[n] and repeat.
|                       // Signals effect to processor state after first clk posedge.
|                       @(posedge clk);
|               { concatMap assertion simulationInfo }
|                   repeat ( 2000 ) @(posedge clk);
|                       $finish;
|                   end
|
|               endmodule
|               |]

            -- TODO: Количество циклов для тестирования должно задаваться пользователем.
            cntxs = take 3 $ simulateAlgByCycle (fromMaybe def testCntx) $ functions n
            cycleTicks = tail $ programTicks n  -- because program[0] is skiped
            simulationInfo = -- (trace ("cntxs: \n" ++ concatMap ((++ "\n") . show) cntxs) 0, head cntxs) :
                concatMap (\cntx -> Nothing {- compute loop end -} :  map (Just . (, cntx)) cycleTicks) cntxs
            assertion Nothing = fixIndent [qc|
|
|                       //-----------------------------------------------------------------
|                       @(posedge cycle);
|               |]
            assertion (Just (t, cntx))
                = fixIndentNoLn [qc|
|                       @(posedge clk); $write("tick: { t }; net.data_bus == %h ", net.data_bus);
|               |]
                ++ case extractInstructionAt n t of
                    Transport v _ _ : _ -> fixIndent [qc|
|                                   $write("=== %h (var: %s)", { fromEnum $ get' cntx v }, { v } );
|                                   if ( !( net.data_bus === { fromEnum $ get' cntx v } ) ) $display( " FAIL");
|                                   else $display();
|                       |]
                    [] -> fixIndent [qc|
|                                   $display();
|                       |]
