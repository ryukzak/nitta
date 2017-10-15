{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module NITTA.BusNetwork where

import           Control.Monad.State
import           Data.Array
import           Data.Default
import           Data.Either
import           Data.List               (intersect, nub, sortBy, (\\))
import qualified Data.Map                as M
import           Data.Maybe              (fromMaybe, isJust, mapMaybe)
import           Data.Proxy
import qualified Data.String.Utils       as S
import           Data.Typeable
import           NITTA.Lens
import           NITTA.ProcessUnits.Fram
import           NITTA.TestBench
import           NITTA.Types
import           NITTA.Utils
import           Numeric.Interval        (inf, width, (...))



class ( Typeable v, Eq v, Ord v, Show v ) => Title v
instance ( Typeable v, Eq v, Ord v, Show v ) => Title v


data BusNetwork title spu v t =
  BusNetwork
    { bnRemains            :: [FB Parcel v]
    , bnForwardedVariables :: [v]
    , bnBinded             :: M.Map title [FB Parcel v]
    , bnPus                :: M.Map title spu
    , bnProcess            :: Process v t
    , bnWires              :: Array Int [(title, S)]
    }

busNetwork pus wires = BusNetwork [] [] (M.fromList []) (M.fromList pus) def wires



instance ( Title title, Var v, Time t
         , PUClass Passive (PU Passive v t) v t
         ) => PUClass (Network title) (BusNetwork title (PU Passive v t) v t) v t where

  bind fb bn@BusNetwork{..}
    | any (isRight . bind fb) $ M.elems bnPus
    = Right bn{ bnRemains=fb : bnRemains }
  bind _fb _bn = Left "no"

  options BusNetwork{..} =
    let x = concat
          [
            [ TransportOpt fromPu (fixPullConstrain pullAt) $ M.fromList pushs
            | pushs <- mapM pushOptionsFor pullVars
            , let pushTo = mapMaybe (fmap fst . snd) pushs
            , length (nub pushTo) == length pushTo
            ]
          | (fromPu, opts) <-
            -- trace ("puOptions: \n" ++ concatMap ((" " ++) . (++ "\n") . show) puOptions)
            puOptions
          , EffectOpt (Pull pullVars) pullAt <- opts
          ]
    in -- trace ("BusNetwork options: \n" ++ concatMap ((++"\n") . ("  "++) . show) x)
       x
    where
      now = nextTick bnProcess
      fixPullConstrain tc@TimeConstrain{..} = tc{ tcAvailable=max now (tc^.avail.infimum) ... tc^.avail.supremum }
      pushOptionsFor v | v `notElem` availableVars = [(v, Nothing)]
      pushOptionsFor v = (v, Nothing) : pushOptionsFor' v

      pushOptionsFor' v = [ (v, Just (pushTo, pushAt))
                          | (pushTo, vars) <- puOptions
                          , EffectOpt (Push pushVar) pushAt <- vars
                          , pushVar == v
                          ]
      availableVars =
        let fbs = bnRemains ++ concat (M.elems bnBinded)
            alg = foldl
                  (\dict (a, b) -> M.adjust ((:) b) a dict)
                  (M.fromList [(v, []) | v <- concatMap variables fbs])
                  $ filter (\(_a, b) -> b `notElem` bnForwardedVariables)
                  $ concatMap dependency fbs
            notBlockedVariables = map fst $ filter (null . snd) $ M.assocs alg
        in notBlockedVariables \\ bnForwardedVariables

      puOptions = M.assocs $ M.map options bnPus

  select ni@BusNetwork{..} act@TransportAct{..}
    | nextTick bnProcess > act^.at.infimum
    = error $ "BusNetwork wraping time! Time: " ++ show (nextTick bnProcess) ++ " Act start at: " ++ show (inf taPullAt)
    | otherwise = ni
    { bnPus=foldl (\s n -> n s) bnPus steps
    , bnProcess=snd $ modifyProcess bnProcess $ do
        mapM_ (\(v, (title, _)) -> add
                (Activity $ transportStartAt ... transportEndAt)
                $ InstructionStep (Transport v taPullFrom title :: Instruction (BusNetwork title (PU Passive v t) v t))
              ) $ M.assocs push'
        _ <- add (Activity $ transportStartAt ... transportEndAt) $ CADStep $ show act --   $ Pull pullVars
        setProcessTime $ act^.at.supremum + 1
    , bnForwardedVariables=pullVars ++ bnForwardedVariables
    }
    where
      transportStartAt = inf taPullAt
      transportDuration = maximum $
        map ((\event -> (inf event - transportStartAt) + width event) . snd) $ M.elems push'
      transportEndAt = transportStartAt + transportDuration
      -- if puTitle not exist - skip it...
      pullStep = M.adjust (\dpu -> select dpu $ EffectAct (Pull pullVars) taPullAt) taPullFrom
      pushStep (var, (dpuTitle, pushAt)) =
        M.adjust (\dpu -> select dpu $ EffectAct (Push var) pushAt) dpuTitle
      pushSteps = map pushStep $ M.assocs push'
      steps = pullStep : pushSteps

      push' = M.map (fromMaybe undefined) $ M.filter isJust taPush
      pullVars = M.keys push'


  process pu@BusNetwork{..} = let
    transportKey = M.fromList
      [ (v, sKey st)
      | st <- steps bnProcess
      , let instr = extractInstruction pu st
      , isJust instr
      , let (Just (Transport v _ _)) = instr
      ]
    p'@Process{ steps=steps' } = snd $ modifyProcess bnProcess $ do
      let pus = sortBy (\a b -> fst a `compare` fst b) $ M.assocs bnPus
      mapM (addSubProcess transportKey) pus

    in p'{ steps=reverse steps' }
    where
      addSubProcess transportKey (puTitle, pu') = do
        let subSteps = steps $ process pu'
        uids' <- foldM (\dict Step{..} -> do
                           k <- add sTime $ NestedStep puTitle sDesc
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



-- Сигналы - есть у всех управляемых ПУ.




instance ( Title title, Var v, Time t
         ) => Controllable (BusNetwork title (PU Passive v t) v t) where

  data Instruction (BusNetwork title (PU Passive v t) v t)
    = Transport v title title
    deriving (Typeable, Show)

  data Signal (BusNetwork title (PU Passive v t) v t) = Wire Int
    deriving (Show, Eq, Ord)



instance ( Title title, Var v, Time t
         ) => ByTime (BusNetwork title (PU Passive v t) v t) t where
  signalAt BusNetwork{..} t (Wire i) = foldl (+++) X $ map (uncurry subSignal) $ bnWires ! i
    where
      subSignal puTitle s = case (bnPus M.! puTitle, s) of
        (PU pu', S s')
          | Just s'' <- cast s'
          -> signalAt pu' t s''
          | otherwise -> error "Wrong signal!"



instance ( PUClass (Network title) (BusNetwork title (PU Passive v t) v t) v t
         , Simulatable (PU Passive v t) v Int
         , Typeable title, Typeable (PU Passive v t)
         , Ord title, Show title
         , Var v, Time t
         ) => Simulatable (BusNetwork title (PU Passive v t) v t) v Int where
  variableValue _fb bn@BusNetwork{..} cntx vi@(v, _) =
    let [Transport _ src _] =
          filter (\(Transport v' _ _) -> v == v')
          $ mapMaybe (extractInstruction bn)
          $ steps bnProcess
    in variableValueWithoutFB (bnPus M.! src) cntx vi



----------------------------------------------------------------------



bindingOptions BusNetwork{..} =
  concatMap bindVariants' bnRemains
  where
    bindVariants' fb =
      [ (fb, puTitle) -- , newVariants pu fb)
      | (puTitle, pu) <- sortByLoad $ M.assocs bnPus
      , isRight $ bind fb pu
      , not $ selfTransport fb puTitle
      ]

    sortByLoad = sortBy (\(a, _) (b, _) -> load a `compare` load b)
    load = length . binded

    selfTransport fb puTitle =
      not $ null $ variables fb `intersect` concatMap variables (binded puTitle)

    binded puTitle | puTitle `M.member` bnBinded = bnBinded M.! puTitle
                   | otherwise = []



subBind fb puTitle bn@BusNetwork{ bnProcess=p@Process{..}, ..} = bn
  { bnPus=M.adjust (fromRight undefined . bind fb) puTitle bnPus
  , bnBinded=M.alter (\v -> case v of
                         Just fbs -> Just $ fb : fbs
                         Nothing  -> Just [fb]
                     ) puTitle bnBinded
  , bnProcess=snd $ modifyProcess p $
      add (Event nextTick) $ CADStep $ "Bind " ++ show fb ++ " to " ++ puTitle
  , bnRemains=filter (/= fb) bnRemains
  }






--------------------------------------------------------------------------

instance ( Time t, Var v
         , Ord (Signal (BusNetwork String (PU Passive v t) v t))
         ) => Synthesis (BusNetwork String (PU Passive v t) v t) where
  moduleName BusNetwork{..} = S.join "_" (M.keys bnPus) ++ "_net"

  moduleInstance _ _ _ = undefined

  moduleDefinition pu@BusNetwork{..}
    = let (instances, valuesRegs) = renderInstance [] [] $ M.assocs bnPus
      in renderST [ "module $moduleName$("
                  , "    clk,"
                  , "    rst"
                  , "    );"
                  , ""
                  , "parameter MICROCODE_WIDTH = $microCodeWidth$;"
                  , "parameter DATA_WIDTH = 32;"
                  , "parameter ATTR_WIDTH = 4;"
                  , ""
                  , "input clk;"
                  , "input rst;"
                  , ""
                  , "// Sub module instances"
                  , "wire [MICROCODE_WIDTH-1:0] signals_out;"
                  , "wire [DATA_WIDTH-1:0] data_bus;"
                  , "wire [ATTR_WIDTH-1:0] attr_bus;"
                  , "", ""
                  , "pu_simple_control"
                  , "    #( .MICROCODE_WIDTH( MICROCODE_WIDTH )"
                  , "     , .PROGRAM_DUMP( \"hdl/gen/$moduleName$.dump\" )"
                  , "     , .PROGRAM_SIZE( $ProgramSize$ )"
                  , "     ) control_unit"
                  , "    ( .clk( clk ), .rst( rst ), .signals_out( signals_out ) );"
                  , ""
                  , "", ""
                  , "$instances$"
                  , "", ""
                  , "assign { attr_bus, data_bus } = "
                  , "$OutputRegs$;"
                  , ""
                  , "endmodule"
                  , ""
                  ]
                  [ ( "moduleName", moduleName pu )
                  , ( "microCodeWidth", show $ snd (bounds bnWires) + 1 )
                  , ( "instances", S.join "\n\n" instances)
                  , ( "OutputRegs", S.join "| \n" $ map (\(a, d) -> "    { " ++ a ++ ", " ++ d ++ " } ") valuesRegs )
                  , ( "ProgramSize", show $ fromEnum (nextTick bnProcess)
                      + 1 -- 0 адресс программы для простоя процессора
                      + 1 -- На последнем такте для BusNetwork можно подготовить следующую
                          -- пересылку, но сама шина может быть занята работой.
                    )
                  ]
    where
      valueData t = t ++ "_data_out"
      valueAttr t = t ++ "_attr_out"
      regInstance title = renderST [ "wire [DATA_WIDTH-1:0] $DataOut$;"
                                   , "wire [ATTR_WIDTH-1:0] $AttrOut$;"
                                   ]
                                   [ ("DataOut", valueData title)
                                   , ("AttrOut", valueAttr title)
                                   ]

      renderInstance insts regs [] = ( reverse insts, reverse regs )
      renderInstance insts regs ((title, PU spu) : xs)
        = let inst = moduleInstance spu title (cntx title spu Proxy)
              insts' = inst : regInstance title : insts
              regs' = (valueAttr title, valueData title) : regs
          in renderInstance insts' regs' xs
      cntx :: ( Typeable pu, Show (Signal pu)
              ) => String -> pu -> Proxy (Signal pu) -> [(String, String)]
      cntx title _spu p
        = [ ( "Clk", "clk" )
          , ( "DataIn", "data_bus" )
          , ( "AttrIn", "attr_bus" )
          , ( "DataOut", valueData title )
          , ( "AttrOut", valueAttr title )
          ] ++ mapMaybe foo [ (i, s)
                            | (i, ds) <- assocs bnWires
                            , (title', s) <- ds
                            , title' == title
                            ]
        where
          foo (i, S s)
            | Just s' <- cast s
            = Just ( S.replace " " "_" $ show (s' `asProxyTypeOf` p)
                   , "signals_out[ " ++ show i ++ " ]"
                   )
          foo _ = Nothing


instance ( Synthesis (BusNetwork title (PU Passive v t) v t)
         ) => TestBenchRun (BusNetwork title (PU Passive v t) v t) where
  buildArgs net
    = map (("hdl/" ++) . (++ ".v") . (\(PU pu) -> moduleName pu)) (M.elems $ bnPus net)
    ++ [ "hdl/pu_simple_control.v"
       , "hdl/gen/" ++ moduleName net ++ ".v"
       , "hdl/gen/" ++ moduleName net ++ "_tb.v"
       ]



instance ( Typeable title, Ord title, Show title, Var v, Time t
         , Typeable (PU Passive v t)
         , PUClass Passive (PU Passive v t) v t
         , Simulatable (PU Passive v t) v Int
         , Synthesis (BusNetwork title (PU Passive v t) v t)
         ) => TestBench (BusNetwork title (PU Passive v t) v t) v Int where

  components pu =
    [ ( "hdl/gen/" ++ moduleName pu ++ "_assertions.v", assertions )
    , ( "hdl/gen/" ++ moduleName pu ++ ".dump", dump )
    , ( "hdl/gen/" ++ moduleName pu ++ ".v", const . moduleDefinition )
    , ( "hdl/gen/" ++ moduleName pu ++ "_tb.v", const . testBenchDefinition )
    ]
    where
      puProcess = process pu
      testBenchDefinition net
        = renderST
          [ "module $moduleName$_tb();                                                                                 "
          , "                                                                                                          "
          , "reg clk, rst;                                                                                             "
          , "$moduleName$ net                                                                                          "
          , "  ( .clk(clk)                                                                                             "
          , "  , .rst(rst)                                                                                             "
          , "  );                                                                                                      "
          , "                                                                                                          "
          , "initial begin                                                                                             "
          , "  clk = 1'b0;                                                                                             "
          , "  rst = 1'b1;                                                                                             "
          , "  repeat(2) #10 clk = ~clk;                                                                               "
          , "  rst = 1'b0;                                                                                             "
          , "  forever #10 clk = ~clk;                                                                                 "
          , "end                                                                                                       "
          , "                                                                                                          "
          , "initial                                                                                                   "
          , "  begin                                                                                                   "
          , "    \\$dumpfile(\"$moduleName$_tb.vcd\");                                                                 "
          , "    \\$dumpvars(0, $moduleName$_tb);                                                                      "
          , "    @(negedge rst);                                                                                       "
          , "    forever @(posedge clk);                                                                               "
          , "  end                                                                                                     "
          , "                                                                                                          "
          , "  initial                                                                                                 "
          , "    begin                                                                                                 "
          , "      // program_counter == 1                                                                             "
          , "      // на шину управление выставлены значения соответсвующие адресу 0 в памяти пока не снят rst         "
          , "      @(negedge rst); // Влючение процессора.                                                             "
          , "      // Сразу после снятия сигнала rst на шину управления выставляются сигналы соответствующие адресу 1. "
          , "      // После следующего положительного фронта будет получен результат.                                  "
          , "      `include \"hdl/gen/accum_fram1_fram2_net_assertions.v\"                                             "
          , "      \\$finish;                                                                                          "
          , "    end                                                                                                   "
          , "                                                                                                          "
          , "endmodule                                                                                                 "
          ]
          [ ("moduleName", moduleName net)
          ]

      dump bn@BusNetwork{ bnProcess=Process{..}, ..} _cntx
        = unlines $ map ( values2dump . signalsAt ) ticks
        where
          -- первый элемент - Nop (он же def) по всем линиям. Устанавливается по умолчанию и
          -- говорит о том, что процессор не работает.
          ticks = [ -1 .. nextTick ]
          wires = map Wire $ reverse $ range $ bounds bnWires
          signalsAt t = map (signalAt bn t) wires

      assertions BusNetwork{ bnProcess=Process{..}, ..} cntx
        = concatMap ( ("@(posedge clk); #1; " ++) . (++ "\n") . assert ) [ 0 .. nextTick ]
        where
          p = puProcess
          assert time
            = let pulls = filter (\e -> case e of (Pull _) -> True; _ -> False) $ effectsAt time p
              in case pulls of
                Pull (v:_) : _ -> concat
                    [ "if ( !( net.data_bus === " ++ show (fromMaybe 0 $ M.lookup (v, 0) cntx) ++ ") ) "
                    ,   "$display("
                    ,     "\""
                    ,       "FAIL wrong value of " ++ show' pulls ++ " the bus failed "
                    ,       "(got: %h expect: %h)!"
                    ,     "\", "
                    , "net.data_bus, " ++ show (cntx M.! (v, 0)) ++ "); else $display(\"%d Correct value: %h\", net.control_unit.program_counter, net.data_bus);"
                    ]
                [] -> "$display(\"%d\", net.control_unit.program_counter); /* nothing to check */"
                x -> "$display(\"%d\", net.control_unit.program_counter); /* don't have expected datafor: " ++ show x ++ "*/"
          show' s = filter (/= '\"') $ show s

  simulateContext bn@BusNetwork{..} cntx =
    let transports = extractInstructions bn

    in foldl ( \cntx' (Transport v src _dst) -> M.insert
                (v, 0)
                (variableValueWithoutFB (bnPus M.! src) cntx' (v, 0))
                cntx'
             ) cntx
             transports
