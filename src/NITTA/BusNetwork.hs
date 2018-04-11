{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
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
import qualified Data.Array           as A
import           Data.Default
import           Data.Either
import           Data.List            (find, nub, partition, sortOn, (\\))
import qualified Data.Map             as M
import           Data.Maybe           (fromMaybe, isJust, mapMaybe)
import           Data.Set             (elems, fromList, intersection)
import qualified Data.String.Utils    as S
import           Data.Typeable
import           NITTA.FunctionBlocks (get', simulateAlgByCycle)
import           NITTA.TestBench
import           NITTA.Types
import           NITTA.Utils
import           NITTA.Utils.Lens
import           Numeric.Interval     (inf, width, (...))


-- | Класс идентификатора вложенного вычислительного блока.
class ( Typeable v, Ord v, Show v ) => Title v
instance ( Typeable v, Ord v, Show v ) => Title v


data GBusNetwork title spu v x t
  = BusNetwork
    { -- | Список функциональных блоков привязанных к сети, но ещё не привязанных к конкретным
      -- вычислительным блокам.
      bnRemains        :: [FB (Parcel v x)]
    -- | Таблица привязок функциональных блоков ко вложенным вычислительным блокам.
    , bnBinded         :: M.Map title [FB (Parcel v x)]
    -- | Описание вычислительного процесса сети, как элемента процессора.
    , bnProcess        :: Process (Parcel v x) t
    -- | Словарь вложенных вычислительных блоков по именам.
    , bnPus            :: M.Map title spu
    -- | Ширина шины управления.
    , bnSignalBusWidth :: Int
    , bnInputPorts     :: [InputPort]
    , bnOutputPorts    :: [OutputPort]
    }
type BusNetwork title v x t = GBusNetwork title (PU v x t) v x t

transfered net@BusNetwork{..}
  = [ v | st <- steps bnProcess
    , let instr = extractInstruction net st
    , isJust instr
    , let (Just (Transport v _ _)) = instr
    ]


-- TODO: Проверка подключения сигнальных линий.
busNetwork w ips ops pus = BusNetwork [] (M.fromList []) def (M.fromList pus') w ips ops
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
          { parameterDataWidth=32
          , parameterAttrWidth=4
          , dataIn="data_bus"
          , dataOut=valueData title
          , attrIn="attr_bus"
          , attrOut=valueAttr title
          , signal= \(Signal i) -> "control_bus[" ++ show i ++ "]"
          }
        })
      ) pus
    valueData t = t ++ "_data_out"
    valueAttr t = t ++ "_attr_out"

instance ( Title title
         , Time t
         , Var v
         , Typeable x
         ) => WithFunctionalBlocks (BusNetwork title v x t) (FB (Parcel v x)) where
  functionalBlocks BusNetwork{..} = sortFBs binded []
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
    = concat [ [ DataFlowO (srcTitle, fixPullConstrain pullAt) $ M.fromList pushs
               | pushs <- mapM pushOptionsFor $ elems pullVars
               , let pushTo = mapMaybe (fmap fst . snd) pushs
               , length (nub pushTo) == length pushTo
               ]
             | (srcTitle, opts) <- puOptions
             , EndpointO (Source pullVars) pullAt <- opts
             ]
    where
      now = nextTick bnProcess
      fixPullConstrain constrain
        = let a = max now $ constrain^.avail.infimum
              b = constrain^.avail.supremum
          in constrain & avail .~ (a ... b)

      pushOptionsFor v | v `notElem` availableVars = [(v, Nothing)]
      pushOptionsFor v = (v, Nothing) : pushOptionsFor' v

      pushOptionsFor' v = [ (v, Just (pushTo, pushAt))
                          | (pushTo, vars) <- puOptions
                          , EndpointO (Target pushVar) pushAt <- vars
                          , pushVar == v
                          ]
      bnForwardedVariables = transfered n
      availableVars =
        let fbs = bnRemains ++ concat (M.elems bnBinded)
            alg = foldl
                  (\dict (a, b) -> M.adjust ((:) b) a dict)
                  (M.fromList [(v, []) | v <- elems $ unionsMap variables fbs])
                  $ filter (\(_a, b) -> b `notElem` bnForwardedVariables)
                  $ concatMap dependency fbs
            notBlockedVariables = map fst $ filter (null . snd) $ M.assocs alg
        in notBlockedVariables \\ bnForwardedVariables

      puOptions = M.assocs $ M.map (options endpointDT) bnPus

  decision _proxy n@BusNetwork{ bnProcess, bnPus } d@DataFlowD{ dfdSource=( srcTitle, pullAt ), dfdTargets }
    | nextTick bnProcess > d^.at.infimum
    = error $ "BusNetwork wraping time! Time: " ++ show (nextTick bnProcess) ++ " Act start at: " ++ show (d^.at)
    | otherwise
    = let pushs = M.map (fromMaybe undefined) $ M.filter isJust dfdTargets
          transportStartAt = d^.at.infimum
          transportDuration = maximum $ map (\(_trg, time) -> (inf time - transportStartAt) + width time) $ M.elems pushs
          transportEndAt = transportStartAt + transportDuration

          subDecisions = ( srcTitle, EndpointD (Source $ fromList $ M.keys pushs) pullAt )
                       : [ ( trgTitle, EndpointD (Target v) pushAt )
                         | (v, (trgTitle, pushAt)) <- M.assocs pushs
                         ]
      in n{ bnPus=foldl applyDecision bnPus subDecisions
          , bnProcess=snd $ modifyProcess bnProcess $ do
              mapM_ (\(pushedValue, (targetTitle, _tc)) -> addStep
                      (Activity $ transportStartAt ... transportEndAt)
                      $ InstructionStep (Transport pushedValue srcTitle targetTitle :: Instruction (BusNetwork title v x t))
                    ) $ M.assocs pushs
              addStep_ (Activity $ transportStartAt ... transportEndAt) $ CADStep $ show d
              setProcessTime $ d^.at.supremum + 1
          }
    where
      applyDecision pus (trgTitle, d') = M.adjust (\pu -> decision endpointDT pu d') trgTitle pus



instance ( Title title, Time t, Var v, Typeable x
         ) => ProcessUnit (BusNetwork title v x t) (Parcel v x) t where

  bind fb bn@BusNetwork{..}
    | any (isRight . bind fb) $ M.elems bnPus
    = Right bn{ bnRemains=fb : bnRemains }
  bind fb BusNetwork{..} = Left $ "All sub process units reject the functional block: " ++ show fb ++ "\n"
                                ++ rejects
    where
      rejects = S.join "\n" $ map showReject $ M.assocs bnPus
      showReject (title, pu) | Left e <- bind fb pu = "    [" ++ show title ++ "]: " ++ e
      showReject (title, _) = "    [" ++ show title ++ "]: undefined"

  process pu@BusNetwork{..} = let
    transportKey = M.fromList
      [ (v, sKey st)
      | st <- steps bnProcess
      , let instr = extractInstruction pu st
      , isJust instr
      , let (Just (Transport v _ _)) = instr
      ]
    p'@Process{ steps=steps' } = snd $ modifyProcess bnProcess $ do
      let pus = sortOn fst $ M.assocs bnPus
      mapM (addSubProcess transportKey) pus

    in p'{ steps=reverse steps' }
    where
      addSubProcess transportKey (puTitle, pu') = do
        let subSteps = steps $ process pu'
        uids' <- foldM (\dict Step{..} -> do
                           k <- addStep sTime $ NestedStep puTitle sDesc
                           when (isFB sDesc) $ do
                             let FBStep fb = sDesc
                             mapM_ (\v -> when (v `M.member` transportKey)
                                          $ relation $ Vertical (transportKey M.! v) k
                                   ) $ variables fb
                           return $ M.insert sKey k dict
                       ) M.empty subSteps
        let subRelations = relations $ process pu'
        mapM (\r -> relation $ case r of
                 Vertical a b -> Vertical (uids' M.! a) (uids' M.! b)
             ) subRelations

  setTime t bn@BusNetwork{..} = bn{ bnProcess=bnProcess{ nextTick=t }
                                  , bnPus=M.map (setTime t) bnPus
                                  }



instance Controllable (BusNetwork title v x t) where

  data Instruction (BusNetwork title v x t)
    = Transport v title title
    deriving (Typeable, Show)

  data Microcode (BusNetwork title v x t)
    = BusNetworkMC (A.Array Signal Value)



instance {-# OVERLAPS #-}
         ( Time t
         ) => ByTime (BusNetwork title v x t) t where
  microcodeAt BusNetwork{..} t
    = BusNetworkMC $ foldl merge initSt $ M.elems bnPus
    where
      initSt = A.listArray (Signal 0, Signal $ bnSignalBusWidth - 1) $ repeat def
      merge st PU{..}
        = foldl merge' st $ transmitToLink (microcodeAt unit t) links
      merge' st (s, x) = st A.// [ (s, st A.! s +++ x) ]



instance ( Title title, Var v, Time t ) => Simulatable (BusNetwork title v x t) v x where
  simulateOn cntx BusNetwork{..} fb
    = let Just (title, _) = find (\(_, v) -> fb `elem` v) $ M.assocs bnBinded
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
         ) => DecisionProblem (BindingDT String (Parcel v x))
                    BindingDT (BusNetwork String v x t)
         where
  options _ BusNetwork{..} = concatMap bindVariants' bnRemains
    where
      bindVariants' fb =
        [ BindingO fb puTitle
        | (puTitle, pu) <- sortOn (length . binded . fst) $ M.assocs bnPus
        , isRight $ bind fb pu
        , not $ selfTransport fb puTitle
        ]

      selfTransport fb puTitle =
        not $ null $ variables fb `intersection` unionsMap variables (binded puTitle)

      binded puTitle | puTitle `M.member` bnBinded = bnBinded M.! puTitle
                     | otherwise = []

  decision _ bn@BusNetwork{ bnProcess=p@Process{..}, ..} (BindingD fb puTitle)
    = bn{ bnPus=M.adjust (fromRight undefined . bind fb) puTitle bnPus
        , bnBinded=M.alter (\case Just fbs -> Just $ fb : fbs
                                  Nothing  -> Just [fb]
                           ) puTitle bnBinded
        , bnProcess=snd $ modifyProcess p $
            addStep (Event nextTick) $ CADStep $ "Bind " ++ show fb ++ " to " ++ puTitle
        , bnRemains=filter (/= fb) bnRemains
        }


--------------------------------------------------------------------------


instance ( Time t
         ) => DefinitionSynthesis (BusNetwork String v x t) where
  moduleName title BusNetwork{..} = title ++ "_net"

  hardware title pu@BusNetwork{..}
    = let pus = map (uncurry hardware) $ M.assocs bnPus
          net = [ Immidiate (mn ++ ".v") iml
                , FromLibrary "pu_simple_control.v"
                ]
      in Project mn (pus ++ net)
    where
      mn = moduleName title pu
      iml = let (instances, valuesRegs) = renderInstance [] [] $ M.assocs bnPus
            in renderMST
              [ "module $moduleName$"
              , "  ( input                     clk"
              , "  , input                     rst"
              , S.join ", " ("  " : map (\(InputPort n) -> "input " ++ n) bnInputPorts)
              , S.join ", " ("  " : map (\(OutputPort n) -> "output " ++ n) bnOutputPorts)
              , "  );"
              , ""
              , "parameter MICROCODE_WIDTH = $microCodeWidth$;"
              , "parameter DATA_WIDTH = 32;"
              , "parameter ATTR_WIDTH = 4;"
              , ""
              , "// Sub module instances"
              , "wire [MICROCODE_WIDTH-1:0] control_bus;"
              , "wire [DATA_WIDTH-1:0] data_bus;"
              , "wire [ATTR_WIDTH-1:0] attr_bus;"
              , ""
              , "wire cycle, start, stop;"
              , ""
              , "pu_simple_control #( .MICROCODE_WIDTH( MICROCODE_WIDTH )"
              , "                   , .PROGRAM_DUMP( \"\\$path\\$$moduleName$.dump\" )"
              , "                   , .MEMORY_SIZE( $ProgramSize$ )"
              , "                   ) control_unit"
              , "  ( .clk( clk )"
              , "  , .rst( rst )"
              , "  , .signals_out( control_bus )"
              , "  , .cycle( cycle )"
              , "  );"
              , ""
              , "$instances$"
              , "", ""
              , "assign { attr_bus, data_bus } = "
              , "$OutputRegs$;"
              , ""
              , "endmodule"
              , ""
              ]
              [ ( "moduleName", mn )
              , ( "microCodeWidth", show bnSignalBusWidth )
              , ( "instances", S.join "\n\n" instances)
              , ( "OutputRegs", S.join "| \n" $ map (\(a, d) -> "  { " ++ a ++ ", " ++ d ++ " } ") valuesRegs )
              , ( "ProgramSize", show $ fromEnum (nextTick bnProcess)
                  + 1 -- 0 адресс программы для простоя процессора
                )
              ]
      valueData t = t ++ "_data_out"
      valueAttr t = t ++ "_attr_out"
      regInstance t = renderMST
        [ "wire [DATA_WIDTH-1:0] $DataOut$;"
        , "wire [ATTR_WIDTH-1:0] $AttrOut$;"
        ]
        [ ( "DataOut", valueData t )
        , ( "AttrOut", valueAttr t )
        ]

      renderInstance insts regs [] = ( reverse insts, reverse regs )
      renderInstance insts regs ((t, PU{ unit, systemEnv, links }) : xs)
        = let inst = hardwareInstance t unit systemEnv links
              insts' = inst : regInstance t : insts
              regs' = (valueAttr t, valueData t) : regs
          in renderInstance insts' regs' xs

  software title pu@BusNetwork{ bnProcess=Process{..}, ..}
    = Project mn $ map (uncurry software) (M.assocs bnPus)
                       ++ [ Immidiate (mn ++ ".dump") memoryDump ]
    where
      mn = moduleName title pu
      memoryDump = unlines $ map ( values2dump . values . microcodeAt pu ) ticks
      -- По нулевоу адресу устанавливается команда Nop (он же def) для всех вычислиетльных блоков.
      -- Именно этот адрес выставляется на сигнальные линии когда поднят сигнал rst.
      ticks = [ -1 .. nextTick - 1 ]
      values (BusNetworkMC arr) = reverse $ A.elems arr



instance ( Title title, Var v, Time t
         , Show x
         , DefinitionSynthesis (BusNetwork title v x t)
         , Typeable x
         ) => TestBench (BusNetwork title v x t) v x where
  testBenchDescription title n@BusNetwork{..} cntx0 = Immidiate (moduleName title n ++ "_tb.v") testBenchImp
    where
      ports = map (\(InputPort n') -> n') bnInputPorts ++ map (\(OutputPort n') -> n') bnOutputPorts
      testBenchImp = renderMST
        [ "`timescale 1 ps / 1 ps"
        , "module $moduleName$_tb();                                                                                 "
        , "                                                                                                          "
        , "reg clk, rst;                                                                                             "
        , if null ports
            then ""
            else "wire " ++ S.join ", " ports ++ ";"
        , ""
        , "$moduleName$ net                                                                                          "
        , "  ( .clk( clk )                                                                                           "
        , "  , .rst( rst )                                                                                           "
        , S.join ", " ("  " : map (\p -> "." ++ p ++ "( " ++ p ++ " )") ports)
        , "  );                                                                                                      "
        , "                                                                                                          "
        , S.join "\n\n"
          [ tbEnv
          | (t, PU{ unit, systemEnv, links }) <- M.assocs bnPus
          , let t' = filter (/= '"') $ show t
          , let tbEnv = testBenchEnviroment t' unit systemEnv links
          , not $ null tbEnv
          ]
        , "                                                                                                          "
        , verilogWorkInitialze
        , "                                                                                                          "
        , verilogClockGenerator
        , "                                                                                                          "
        , "  initial                                                                                                 "
        , "    begin                                                                                                 "
        , "      // microcode when rst == 1 -> program[0], and must be nop for all PUs                               "
        , "      @(negedge rst); // Turn nitta processor on.                                                         "
        , "      // Start computational cycle from program[1] to program[n] and repeat.                              "
        , "      // Signals effect to processor state after first clk posedge.                                       "
        , assertions
        , "      \\$finish;                                                                                          "
        , "    end                                                                                                   "
        , "                                                                                                          "
        , "endmodule                                                                                                 "
        ]
        [ ( "moduleName", moduleName title n )
        ]

      -- FIXME: 15 - must be variable
      cntxs = take 15 $ simulateAlgByCycle cntx0 $ functionalBlocks n
      cycleTicks = [ 0 .. nextTick (process n) - 1 ]
      simulationInfo = (0, def) : concatMap (\cntx -> map (\t -> (t, cntx)) cycleTicks) cntxs
      assertions = concatMap ( ("      @(posedge clk); " ++) . (++ "\n") . assert ) simulationInfo
        where
          assert (t, cntx)
            = "\\$write(\"%s, bus: %h\", " ++ show (show t) ++ ", net.data_bus); "
            ++ case extractInstructionAt n t of
                Transport v _ _ : _
                  -> concat
                    [ "\\$write(\" == %h (%s)\", " ++ show (get' cntx v) ++ ", " ++ show v ++ ");"
                    , "if ( !( net.data_bus === " ++ show (get' cntx v) ++ ") ) "
                    ,   "\\$display(\" FAIL\"); else \\$display();"
                    ]
                [] -> "\\$display();"
