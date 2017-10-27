{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

{-|
TODO: Добавить функциональный блок Const.
-}
module NITTA.ProcessUnits.Fram
  ( Fram(..)
  , Signal(..)
  , framSize
  )
where

-- Как при ветвящемся алгоритме сделать локальный для if statementa Loop? Вероятно, некоторые FB
-- необходимо запретить к ращмещению в тегированном времени. Это не повредит производительности,
-- так как переиспользования вычислительного блока в таком случае быть не может!
import           Data.Array
import           Data.Bits
import           Data.Default
import           Data.Either
import           Data.Generics.Aliases (orElse)
import qualified Data.Graph            as G
import           Data.List             (find, minimumBy, sortBy)
import qualified Data.Map              as M
import           Data.Maybe
import           Data.Typeable
import           NITTA.FunctionBlocks
import           NITTA.Lens
import           NITTA.TestBench
import           NITTA.Types
import           NITTA.Utils
import           Numeric.Interval      ((...))
import           Prelude               hiding (last)


data Fram v t = Fram
  { frMemory             :: Array Int (Cell v t)
  , frRemains            :: [MicroCode v t]
  , frProcess            :: Process v t
  , frAllowBlockingInput :: Bool
  }
deriving instance ( Var v, Time t ) => Show (Fram v t)


remains get Fram{..} = filter (isJust . get) frRemains
remainLoops fr = remains getLoop fr
remainRegs fr = remains getReg fr


getReg :: Var v => MicroCode v t -> Maybe (Reg Parcel v)
getReg MicroCode{..} = castFB fb

getLoop :: Var v => MicroCode v t -> Maybe (Loop Parcel v)
getLoop MicroCode{..} = castFB fb


ioUses fr@Fram{..} =
  length (filter (\Cell{..} -> input /= Undef || output /= Undef) $ elems frMemory)
  + length (remainLoops fr)


placeForRegExist Fram{ frAllowBlockingInput=True, ..} =
  any (\Cell{..} -> output /= UsedOrBlocked) $ elems frMemory
placeForRegExist fr@Fram{ frAllowBlockingInput=False, ..} =
  any (\Cell{..} -> input /= Undef && output /= UsedOrBlocked) (elems frMemory)
  || not (null $ remainLoops fr)


data IOState v t = Undef
                 | Def (MicroCode v t)
                 | UsedOrBlocked
deriving instance ( Var v, Time t ) => Show (IOState v t)
deriving instance ( Var v, Time t ) => Eq (IOState v t)



data Cell v t = Cell
  { input     :: IOState v t -- Pull
  , current   :: Maybe (MicroCode v t)
  , output    :: IOState v t -- Push
  , lastWrite :: Maybe t
  }
deriving instance ( Var v, Time t ) => Show (Cell v t)


data MicroCode v t where
  MicroCode ::
    { compiler, effect, instruction :: [ProcessUid]
    , workBegin                     :: Maybe t
    , fb                            :: FB Parcel v
    , actions                       :: [Effect v]
    , bindTo                        :: MicroCode v t -> Cell v t -> Either String (Cell v t)
    } -> MicroCode v t

microcode = MicroCode [] [] [] Nothing
-- instance HasEffect (MicroCode v t) (Effect v) where
--   effect = lens _effect $ \a b -> a{ _effect=b }

instance ( Var v, Time t ) => Default (Fram v t) where
  def = Fram { frMemory=listArray (0, framSize - 1) $ repeat def
             , frRemains=[]
             , frProcess=def
             , frAllowBlockingInput=True
             }

instance Default (Cell v t) where
  def = Cell Undef Nothing Undef Nothing

instance ( Var v ) => Show (MicroCode v t) where
  show MicroCode{..} = show actions ++ " " ++ show fb

instance ( Var v ) => Eq (MicroCode v t) where
  MicroCode{ actions=a } == MicroCode{ actions=b } = a == b


framSize = 16 :: Int



instance ( Var v, Time t
         ) => Decision EndpointDT (EndpointDT v t)
                      (Fram v t)
         where

  options_ _proxy fr@Fram{ frProcess=Process{..}, ..} = fromCells ++ fromRemain
    where
      fromCells = [ EndpointO (effect2endpoint v) $ constrain cell v
                  | (_addr, cell@Cell{..}) <- assocs frMemory
                  , v <- cell2acts allowOutput cell
                  ]
      fromRemain = [ EndpointO (effect2endpoint act) $ constrain cell act
                   | mc@MicroCode { actions=act:_ } <- frRemains
                   , let aCell = availableCell fr mc
                   , isJust aCell
                   , let Just (_addr, cell) = aCell
                   ]
      allowOutput = ( frAllowBlockingInput || null fromRemain )
        && ( null (remainRegs fr) || framSize - numberOfCellForReg > 1 )
      numberOfCellForReg = length $ filter (\Cell{..} -> output == UsedOrBlocked) $ elems frMemory
      constrain Cell{..} (Pull _)
        | lastWrite == Just nextTick = TimeConstrain (nextTick + 1 ... maxBound) (1 ... maxBound)
        | otherwise              = TimeConstrain (nextTick ... maxBound) (1 ... maxBound)
      constrain _cell (Push _) = TimeConstrain (nextTick ... maxBound) (1 ... maxBound)

  decision_ proxy fr@Fram{ frProcess=p0@Process{ nextTick=tick0 }, .. } act@EndpointD{..}
    | tick0 > act^.at.infimum
    = error $ "You can't start work yesterday :) fram time: " ++ show tick0 ++ " action start at: " ++ show (act^.at.infimum)

    | Just mc@MicroCode{ bindTo=bindTo, .. } <- find ((<< (endpoint2effect epdType)) . head . actions) frRemains
    = case availableCell fr mc of
        Just (addr, cell) ->
          let (key, p') = modifyProcess p0 $ bindFB2Cell addr fb tick0
              Right cell' = bindTo mc{ compiler=key : compiler } cell
              fr' = fr{ frRemains=filter (/= mc) frRemains
                      , frMemory=frMemory // [(addr, cell')]
                      , frProcess=p'
                      }
         in decision_ proxy fr' act
        Nothing -> error $ "Can't find available cell for: " ++ show fb ++ "!"

    | Just (addr, cell) <- find (any (<< (endpoint2effect epdType)) . cell2acts True . snd) $ assocs frMemory
    = case cell of
        Cell{ input=Def mc@MicroCode{ actions=act1 : _ } } | act1 << (endpoint2effect epdType) ->
            let (p', mc') = doAction addr p0 mc
                cell' = updateLastWrite (nextTick p') cell
                cell'' = case mc' of
                  Nothing -> cell'{ input=UsedOrBlocked }
                  Just mc''@MicroCode{ actions=Pull _ : _ } -> cell'{ input=Def mc'' }
                  Just mc''@MicroCode{ actions=Push _ : _ }
                    | output cell' == UsedOrBlocked ->
                        cell'{ input=UsedOrBlocked, output=Def mc'' }
                  _ -> error "Fram internal error after input process."
            in fr{ frMemory=frMemory // [(addr, cell'')]
                 , frProcess=p'
                 }
        Cell{ current=Just mc@MicroCode{ actions=act1 : _ } } | act1 << (endpoint2effect epdType) ->
            let (p', mc') = doAction addr p0 mc
                cell' = updateLastWrite (nextTick p') cell
                cell'' = cell'{ input=UsedOrBlocked
                              , current=mc'
                              }
            in fr{ frMemory=frMemory // [(addr, cell'')]
                 , frProcess=p'
                 }
        Cell{ output=Def mc@MicroCode{ actions=act1 : _ } } | act1 << (endpoint2effect epdType) ->
            let (p', _mc') = doAction addr p0 mc
                -- Вот тут есть потенциальная проблема которую не совсем ясно как можно решить,
                -- а именно, если output происходит в последний такт вычислительного цикла
                -- а input с него происходит в первый такт вычислительного цикла.
                cell' = cell{ input=UsedOrBlocked
                            , output=UsedOrBlocked
                            }
            in fr{ frMemory=frMemory // [(addr, cell')]
                 , frProcess=p'
                 }
        _ -> error "Internal Fram error, step"

    | otherwise = error $ "Can't found selected action: " ++ show act
                  ++ " tick: " ++ show (nextTick p0) ++ "\n"
                  ++ "available options: \n" ++ concatMap ((++ "\n") . show) (options_ endpointDT fr)
                  ++ "cells:\n" ++ concatMap ((++ "\n") . show) (assocs frMemory)
                  ++ "remains:\n" ++ concatMap ((++ "\n") . show) frRemains
    where
      updateLastWrite t cell | Push _ <- (endpoint2effect epdType) = cell{ lastWrite=Just t }
                             | otherwise = cell{ lastWrite=Nothing }

      doAction addr p mc@MicroCode{..} =
        let (p', mc'@MicroCode{ actions=acts' }) = mkWork addr p mc
            result = if null acts'
              then (finish p' mc', Nothing)
              else (p', Just mc')
        in result

      mkWork _addr _p MicroCode{ actions=[] } = error "Fram internal error, mkWork"
      mkWork addr p mc@MicroCode{ actions=x:xs, ..} =
        let ((ef, instrs), p') = modifyProcess p $ do
              e <- add (Activity $ act^.at) $ EffectStep (endpoint2effect epdType)
              i1 <- add (Activity $ act^.at)
                $ InstructionStep (act2Instruction addr (endpoint2effect epdType) :: Instruction (Fram v t))
              is <- if tick0 < act^.at.infimum
                then do
                  i2 <- add (Activity $ tick0 ... act^.at.infimum - 1)
                    $ InstructionStep (Nop :: Instruction (Fram v t))
                  return [ i1, i2 ]
                else return [ i1 ]
              mapM_ (relation . Vertical ef) instrs
              setProcessTime $ act^.at.supremum + 1
              return (e, is)
        in (p', mc{ effect=ef : effect
                  , instruction=instrs ++ instruction
                  , workBegin=workBegin `orElse` Just (act^.at.infimum)
                  , actions=if x == (endpoint2effect epdType) then xs else (x \\\ (endpoint2effect epdType)) : xs
                  })

      finish p MicroCode{..} = snd $ modifyProcess p $ do
        let start = fromMaybe (error "workBegin field is empty!") workBegin
        h <- add (Activity $ start ... act^.at.supremum) $ FBStep fb
        mapM_ (relation . Vertical h) compiler
        mapM_ (relation . Vertical h) effect
        mapM_ (relation . Vertical h) instruction

      act2Instruction addr (Pull _) = Load addr
      act2Instruction addr (Push _) = Save addr










instance ( IOType Parcel v, Time t ) => PUClass Passive (Fram v t) v t where

  bind fb fr@Fram{ frProcess=p@Process{..}, .. }
    | Just (Reg (I a) (O b)) <- castFB fb =
        if placeForRegExist fr
        then let bindToCell _ Cell{ output=UsedOrBlocked } = Left "Can't bind Reg to Fram"
                 bindToCell _ Cell{ current=Just _ } = Left "Can't bind Reg to Fram"
                 bindToCell mc cell@Cell{ input=Undef
                                        , current=Nothing
                                        }
                   | frAllowBlockingInput = Right $ cell{ current=Just mc }
                 bindToCell mc cell@Cell{ input=UsedOrBlocked
                                        , current=Nothing
                                        } = Right $ cell{ current=Just mc }
                 bindToCell _ _ = Left "Can't bind Reg to Fram"
                 ( mc', fr' ) = bind' $ microcode fb [ Push a, Pull b ] bindToCell
             in Right fr'{ frRemains=mc' : frRemains }
        else Left "Can't bind Reg to Fram, place for Reg don't exist."

    | Just (Loop (O bs) (I a)) <- castFB fb =
        if ioUses fr < framSize
        then let bindToCell mc cell@Cell{ input=Undef, output=Undef } =
                   Right cell{ input=Def mc
                             , output=UsedOrBlocked
                             }
                 bindToCell _ cell = Left $ "Can't bind Loop to Fram Cell: " ++ show cell
                 ( mc', fr' ) = bind' $ microcode fb [ Pull bs, Push a ] bindToCell
             in Right fr'{ frRemains=mc' : frRemains }
        else Left "Can't bind Loop to Fram, all IO cell already busy."

    | Just (FramInput addr (O v)) <- castFB fb =
        let bindToCell mc cell@Cell{ input=Undef, .. }
              | ioUses fr < framSize || (ioUses fr == framSize && output /= Undef)
              = Right cell{ input=Def mc }

            bindToCell _ cell = Left $ "Can't bind FramInput (" ++ show fb
                                       ++ ") to Fram Cell: " ++ show cell
            ( mc', fr' ) = bind' $ microcode fb [ Pull v ] bindToCell
        in fmap (\cell' -> fr'{ frMemory=frMemory // [(addr, cell')]
                              }) $ bindToCell mc' $ frMemory ! addr

    | Just (FramOutput addr (I v)) <- castFB fb =
        let bindToCell mc cell@Cell{ output=Undef, .. }
              | ioUses fr < framSize || (ioUses fr == framSize && input /= Undef)
              = Right cell{ output=Def mc }
            bindToCell _ cell = Left $ "Can't bind FramOutput (" ++ show fb
                                       ++ ") to Fram Cell: " ++ show cell
            ( mc', fr' ) = bind' $ microcode fb [ Push v ] bindToCell
        in fmap (\cell' -> fr'{ frMemory=frMemory // [(addr, cell')]
                              }) $ bindToCell mc' $ frMemory ! addr

    | otherwise = Left $ "Unknown functional block: " ++ show fb
      where
        bind' mc =
          let (key, p') = modifyProcess p $ bindFB fb nextTick
              mc' = mc{ compiler=key : compiler mc }
          in ( mc', fr{ frProcess=p' } )

  process = sortPuSteps . frProcess
  setTime t fr@Fram{..} = fr{ frProcess=frProcess{ nextTick=t } }


instance Default (Instruction (Fram v t)) where
  def = Nop

instance ( Var v, Time t ) => Controllable (Fram v t) where

  data Signal (Fram v t)
    = OE
    | WR
    | ADDR Int
    deriving (Show, Eq, Ord)

  data Instruction (Fram v t)
    = Nop
    | Load Int
    | Save Int
    deriving (Show)



instance ( Var v, Time t
         ) => ByTime (Fram v t) t where
  signalAt pu@Fram{..} time sig =
    let instruction = case mapMaybe (extractInstruction pu) $ whatsHappen time frProcess of
          []  -> Nop
          [i] -> i
          is  -> error $ "Ambiguously instruction at "
                       ++ show time ++ ": " ++ show is
    in decodeInstruction instruction sig


instance UnambiguouslyDecode (Fram v t) where
  decodeInstruction  Nop        (ADDR _) = X
  decodeInstruction  Nop         _       = B False
  decodeInstruction (Load addr) (ADDR b) = B $ testBit addr b
  decodeInstruction (Load    _)  OE      = B True
  decodeInstruction (Load    _)  WR      = B False
  decodeInstruction (Save addr) (ADDR b) = B $ testBit addr b
  decodeInstruction (Save    _)  OE      = B False
  decodeInstruction (Save    _)  WR      = B True





instance ( PUClass Passive (Fram v t) v t
         , Time t
         , Var v
         ) => Simulatable (Fram v t) v Int where
  variableValue (FB fb) pu@Fram{..} cntx (v, i)
    | Just (Loop _bs (I a)) <- cast fb, a == v = cntx M.! (v, i)

    | Just (Loop (O bs) _a) <- cast fb, v `elem` bs, i == 0
    = addr2value $ findAddress v pu

    | Just (Reg (I a) _bs) <- cast fb, a == v = cntx M.! (v, i)
    | Just (Reg (I a) (O bs)) <- cast fb, v `elem` bs = cntx M.! (a, i)

    | Just (FramInput addr (O bs)) <- cast fb, i == 0, v `elem` bs = addr2value addr
    | Just (FramOutput _addr (I a)) <- cast fb, v == a = cntx M.! (v, i)

    | otherwise = error $ "Can't simulate " ++ show fb
    where
      addr2value addr = 0x1000 + addr -- must be coordinated with test bench initialization


sortPuSteps p@Process{..} =
  let hierarchy = foldl (\m (Vertical a b) -> M.adjust (b :) a m)
                      (M.fromList [(k, []) | k <- map sKey steps])
                      [x | x@(Vertical _ _) <- relations]
      (graph, _v2k, k2v) = G.graphFromEdges $ map (\(a, b) -> ((), a, b)) $ M.assocs hierarchy
      steps' = sortBy (\Step{ sKey=a } Step{ sKey=b } ->
                       case (k2v a, k2v b) of
                         (Just a', Just b') ->
                           let ab = G.path graph a' b'
                               ba = G.path graph b' a'
                           in if ab || ba then if ab then LT
                                                     else GT
                              else compare a b
                         _ -> compare a b
                   ) steps
  in p{ steps=steps' }


bindFB2Cell addr fb t = add (Event t) $ CADStep $ "Bind " ++ show fb ++ " to cell " ++ show addr


cell2acts _allowOutput Cell{ input=Def MicroCode{ actions=x:_ } }    = [x]
cell2acts _allowOutput Cell{ current=Just MicroCode{ actions=x:_ } } = [x]
cell2acts True         Cell{ output=Def MicroCode{actions=x:_ } }    = [x]
cell2acts _ _                                                        = []


availableCell fr@Fram{..} mc@MicroCode{..} =
  case filter (\(_addr, cell@Cell{..}) ->
                 isRight (bindTo mc cell)
                 && ( frAllowBlockingInput
                      || isNothing (getReg mc)
                      || input == UsedOrBlocked )
                 && ( isNothing (getReg mc)
                      || null (remainLoops fr)
                      || framSize - ioUses fr > 1 )
              ) $ assocs frMemory of
    []    -> Nothing
    cells -> Just $ minimumBy (\a b -> load a `compare` load b) cells
  where
    load (_addr, Cell{..}) = sum [ if input == UsedOrBlocked then -2 else 0
                                 , if output == Undef then -1 else 0
                                 ] :: Int



---------------------------------------------------

instance TestBenchRun (Fram v t) where
  buildArgs _ = [ "hdl/pu_fram.v"
                , "hdl/pu_fram_tb.v"
                ]

instance ( Var v, Time t ) => TestBench (Fram v t) v Int where

  components _ =
    [ ( "hdl/gen/pu_fram_inputs.v", testInputs )
    , ( "hdl/gen/pu_fram_signals.v", testSignals )
    , ( "hdl/gen/pu_fram_outputs.v", testOutputs )
    ]

  simulateContext fr@Fram{ frProcess=p@Process{..}, .. } cntx =
    let vs = [ v | eff <- getEffects p
                 , v <- variables eff
                 ]
    in foldl ( \cntx' v ->
                 M.insert (v, 0)
                          (variableValueWithoutFB fr cntx' (v, 0))
                          cntx'
             ) cntx vs



testSignals fram@Fram{ frProcess=Process{..}, ..} _cntx
  = concatMap ( (++ " @(negedge clk)\n") . showSignals . signalsAt ) [ 0 .. nextTick + 1 ]
  where
    signalsAt time = map (signalAt fram time)
                     [ OE, WR, ADDR 3, ADDR 2, ADDR 1, ADDR 0 ]
    showSignals = (\[oe, wr, a3, a2, a1, a0] ->
                      "oe <= 'b" ++ oe
                      ++ "; wr <= 'b" ++ wr
                      ++ "; addr[3] <= 'b" ++ a3
                      ++ "; addr[2] <= 'b" ++ a2
                      ++ "; addr[1] <= 'b" ++ a1
                      ++ "; addr[0] <= 'b" ++ a0 ++ ";"
                  ) . map show



testInputs Fram{ frProcess=p@Process{..}, ..} cntx
  = concatMap ( (++ " @(negedge clk);\n") . busState ) [ 0 .. nextTick + 1 ]
  where
    busState t
      | Just (Push v) <- effectAt t p = "value_i <= " ++ show (cntx M.! (v, 0)) ++ ";"
      | otherwise = "/* NO INPUT */"

testOutputs pu@Fram{ frProcess=p@Process{..}, ..} cntx
  = concatMap ( ("@(posedge clk); #1; " ++) . (++ "\n") . busState ) [ 0 .. nextTick + 1 ] ++ bankCheck
  where
    busState t
      | Just (Pull (v : _)) <- effectAt t p
      = checkBus v $ cntx M.! (v, 0)
      | otherwise
      = "/* NO OUTPUT */"

    checkBus v value = concat
      [ "if ( !( value_o == " ++ show value ++ " ) ) "
      ,   "$display("
      ,     "\""
      ,       "FAIL wrong value of " ++ show' v ++ " on the bus! "
      ,       "(got: %h expect: %h)"
      ,     "\","
      ,     "value_o, " ++ show value
      ,   ");"
      ]

    bankCheck = "\n\n@(posedge clk);\n"
      ++ concat [ checkBank addr v (cntx M.! (v, 0))
                | Step{ sDesc=FBStep fb, .. } <- filter (isFB . sDesc) steps
                , let addr_v = outputStep fb
                , isJust addr_v
                , let Just (addr, v) = addr_v
                ]

    outputStep fb
      | Just (Loop _bs (I a)) <- castFB fb = Just (findAddress a pu, a)
      | Just (FramOutput addr (I a)) <- castFB fb = Just (addr, a)
      | otherwise = Nothing

    checkBank addr v value = concat
      [ "if ( !( fram.bank[" ++ show addr ++ "] == " ++ show value ++ " ) ) "
      ,   "$display("
      ,     "\""
      ,       "FAIL wrong value of " ++ show' v ++ " in fram bank[" ++ show' addr ++ "]! "
      ,       "(got: %h expect: %h)"
      ,     "\","
      ,     "value_o, " ++ show value
      ,   ");"
      ]
    show' s = filter (/= '\"') $ show s



findAddress v pu@Fram{ frProcess=p@Process{..} }
  | [ Step{ sTime=Activity timePlace }
    ] <- filter ( \st -> isEffect (sDesc st)
                         && (\Step{ sDesc=EffectStep eff } -> v `elem` variables eff) st
                ) steps
  , instructions <- mapMaybe (extractInstruction pu) $ whatsHappen (timePlace^.infimum) p
  , is <- mapMaybe (\i -> case i of
                  Load addr -> Just addr
                  Save addr -> Just addr
                  _         -> Nothing
        ) instructions
  = if length is == 1 then head is
                      else err
  | otherwise = err
    where err = error $ "Can't find instruction for effect of variable: " ++ show v


instance ( Time t, Var v ) => Synthesis (Fram v t) where
  moduleInstance _pu name cntx
    = renderST
      [ "pu_fram $name$ ("
      , "    .clk( $Clk$ ),"
      , "    .signal_addr( { $ADDR_3$, $ADDR_2$, $ADDR_1$, $ADDR_0$ } ),"
      , ""
      , "    .signal_wr( $WR$ ),"
      , "    .data_in( $DataIn$ ),"
      , "    .attr_in( $AttrIn$ ),"
      , ""
      , "    .signal_oe( $OE$ ),"
      , "    .data_out( $DataOut$ ),"
      , "    .attr_out( $AttrOut$ ) "
      , ");"
      , "integer $name$_i;"
      , "initial for ( $name$_i = 0; $name$_i < 16; $name$_i = $name$_i + 1) $name$.bank[$name$_i] <= 32'h1000 + $name$_i;"
      ] $ ("name", name) : cntx
  moduleName _ = "pu_fram"
  moduleDefinition = undefined
