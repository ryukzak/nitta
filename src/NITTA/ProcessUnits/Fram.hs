{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module NITTA.ProcessUnits.Fram
  ( Fram(..)
  , Signals(..)
  , framSize
  ) where

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
import           NITTA.TestBench
import           NITTA.Types
import           NITTA.Utils
import           Prelude               hiding (last)

-- import           Debug.Trace




data Fram ty v t = Fram
  { frMemory             :: Array Int (Cell v t)
  , frRemains            :: [MicroCode v t]
  , frProcess            :: Process v t
  , frAllowBlockingInput :: Bool
  } deriving (Show)


remainLoops fr = remains getLoop fr

remainRegs fr = remains getReg fr


remains get Fram{..} = filter (isJust . get) frRemains

getReg :: Typeable v => MicroCode v t -> Maybe (Reg v)
getReg MicroCode{ fb=FB fb' } = cast fb'

getLoop :: Typeable v => MicroCode v t -> Maybe (Loop v)
getLoop MicroCode{ fb=FB fb } = cast fb


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
  deriving (Show, Eq)


data Cell v t = Cell
  { input     :: IOState v t -- Pull
  , current   :: Maybe (MicroCode v t)
  , output    :: IOState v t -- Push
  , lastWrite :: Maybe t
  } deriving (Show)


data MicroCode v t where
  MicroCode :: --(FBClass fb v) =>
    { compiler, effect, instruction :: [ProcessUid]
    , workBegin                     :: Maybe t
    , fb                            :: FB v
    , actions                       :: [Effect v]
    , bindTo                        :: MicroCode v t -> Cell v t -> Either String (Cell v t)
    } -> MicroCode v t

microcode = MicroCode [] [] [] Nothing


instance ( Var v, Time t ) => Default (Fram ty v t) where
  def = Fram { frMemory=listArray (0, framSize - 1) $ repeat def
             , frRemains=[]
             , frProcess=def
             , frAllowBlockingInput=True
             }

instance Default (Cell v t) where
  def = Cell Undef Nothing Undef Nothing

instance ( Show v ) => Show (MicroCode v t) where
  show MicroCode{..} = show actions ++ " " ++ show fb

instance ( Eq v ) => Eq (MicroCode v t) where
  MicroCode{ actions=a } == MicroCode{ actions=b } = a == b


framSize = 16 :: Int

type I v t = Instruction Fram v t

instance ( Var v, Time t ) => PUClass Fram Passive v t where

  data Signals Fram = OE | WR | ADDR Int

  data Instruction Fram v t
    = Nop
    | Load Int
    | Save Int
    deriving (Show)

  signal' Fram{..} sig time =
    let instruction = case infoAt time $ steps frProcess of
          ([] :: [I v t]) -> Nop
          [i]             -> i
          is              -> error $ "Ambiguously instruction at "
                             ++ show time ++ ": " ++ show is
    in value instruction sig
    where
      value  Nop        (ADDR _) = X
      value  Nop         _       = B False
      value (Load addr) (ADDR b) = B $ testBit addr b
      value (Load    _)  OE      = B True
      value (Load    _)  WR      = B False
      value (Save addr) (ADDR b) = B $ testBit addr b
      value (Save    _)  OE      = B False
      value (Save    _)  WR      = B True

  bind fb fr@Fram{ frProcess=p@Process{..}, .. }
    | Just (Reg a b) <- unbox fb =
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

    | Just (Loop bs a) <- unbox fb =
        if -- trace ("ioUses: " ++ show (ioUses fr)) $
           ioUses fr < framSize
        then let bindToCell mc cell@Cell{ input=Undef, output=Undef } =
                   Right cell{ input=Def mc
                             , output=UsedOrBlocked
                             }
                 bindToCell _ cell = Left $ "Can't bind Loop to Fram Cell: " ++ show cell
                 ( mc', fr' ) = bind' $ microcode fb [ Pull bs, Push a ] bindToCell
             in Right fr'{ frRemains=mc' : frRemains }
        else Left "Can't bind Loop to Fram, all IO cell already busy."

    | Just (FramInput addr v) <- unbox fb =
        let bindToCell mc cell@Cell{ input=Undef, .. }
              | ioUses fr < framSize || (ioUses fr == framSize && output /= Undef)
              = Right cell{ input=Def mc }

            bindToCell _ cell = Left $ "Can't bind FramInput (" ++ show fb
                                       ++ ") to Fram Cell: " ++ show cell
            ( mc', fr' ) = bind' $ microcode fb [ Pull v ] bindToCell
        in fmap (\cell' -> fr'{ frMemory=frMemory // [(addr, cell')]
                              }) $ bindToCell mc' $ frMemory ! addr

    | Just (FramOutput addr v) <- unbox fb =
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
          let (key, p') = modifyProcess p $ bindFB fb tick
              mc' = mc{ compiler=key : compiler mc }
          in ( mc', fr{ frProcess=p' } )

  options fr@Fram{ frProcess=Process{..}, ..} = fromCells ++ fromRemain
    where
      fromCells = [ EffectOpt v $ constrain cell v
                  | (_addr, cell@Cell{..}) <- assocs frMemory
                  , v <- cell2acts allowOutput cell
                  ]
      fromRemain = [ EffectOpt act $ constrain cell act
                   | mc@MicroCode { actions=act:_ } <- frRemains
                   , let aCell = availableCell fr mc
                   , isJust aCell
                   , let Just (_addr, cell) = aCell
                   ]
      allowOutput = ( frAllowBlockingInput || null fromRemain )
        && ( null (remainRegs fr) || framSize - numberOfCellForReg > 1 )
      numberOfCellForReg = length $ filter (\Cell{..} -> output == UsedOrBlocked) $ elems frMemory
      constrain Cell{..} (Pull _)
        | lastWrite == Just tick = TimeConstrain 1 (tick + 1) maxBound
        | otherwise              = TimeConstrain 1 tick maxBound
      constrain _cell (Push _) = TimeConstrain 1 tick maxBound

  step fr@Fram{ frProcess=p0@Process{ tick=tick0 }, .. } act0@EffectAct{ eaAt=at@Event{..}, .. }
    | tick0 > eStart = error "You can't start work yesterday:)"

    | Just mc@MicroCode{ bindTo=bindTo, .. } <- find ((<< eaEffect) . head . actions) frRemains
    = case availableCell fr mc of
        Just (addr, cell) ->
          let (key, p') = modifyProcess p0 $ bindFB2Cell addr fb tick0
              Right cell' = bindTo mc{ compiler=key : compiler } cell
              fr' = fr{ frRemains=filter (/= mc) frRemains
                      , frMemory=frMemory // [(addr, cell')]
                      , frProcess=p'
                      }
         in --trace (show fb ++ " --> " ++ show addr) $
            step fr' act0
        Nothing -> error $ "Can't find available cell for: " ++ show fb ++ "!"

    | Just (addr, cell) <- find (any (<< eaEffect) . cell2acts True . snd) $ assocs frMemory
    = case cell of
        Cell{ input=Def mc@MicroCode{ actions=act : _ } } | act << eaEffect ->
            let (p', mc') = doAction addr p0 mc
                cell' = updateLastWrite (tick p') cell
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
        Cell{ current=Just mc@MicroCode{ actions=act : _ } } | act << eaEffect ->
            let (p', mc') = doAction addr p0 mc
                cell' = updateLastWrite (tick p') cell
                cell'' = cell'{ input=UsedOrBlocked
                              , current=
                                  --trace (show mc ++  " --> " ++ show mc' ++ " @ " ++ show addr)
                                  mc'
                              }
            in fr{ frMemory=frMemory // [(addr, cell'')]
                 , frProcess=p'
                 }
        Cell{ output=Def mc@MicroCode{ actions=act : _ } } | act << eaEffect ->
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

    | otherwise = error $ "Can't found selected action: " ++ show act0
                  ++ " tick: " ++ show (tick p0) ++ "\n"
                  ++ "available options: \n" ++ concatMap ((++ "\n") . show) (options fr)
                  ++ "cells:\n" ++ concatMap ((++ "\n") . show) (assocs frMemory)
                  ++ "remains:\n" ++ concatMap ((++ "\n") . show) frRemains
    where
      updateLastWrite t cell | Push _ <- eaEffect = cell{ lastWrite=Just t }
                             | otherwise = cell{ lastWrite=Nothing }

      doAction addr p mc@MicroCode{..} =
        let (p', mc'@MicroCode{ actions=acts' }) = mkWork addr p mc
        in if null acts'
           then (finish p' mc', Nothing)
           else (p', Just mc')

      mkWork _addr _p MicroCode{ actions=[] } = error "Fram internal error, mkWork"
      mkWork addr p mc@MicroCode{ actions=x:xs, ..} =
        let ((ef, instrs), p') = modifyProcess p $ do
              e <- add at eaEffect
              i1 <- add at $ act2Signal addr eaEffect
              is <- if tick0 < eStart
                then do
                  i2 <- add (Event tick0 (eStart - tick0)) nop
                  return [ i1, i2 ]
                else return [ i1 ]
              mapM_ (relation . Vertical ef) instrs
              setTime (eStart + eDuration)
              return (e, is)
        in (p', mc{ effect=ef : effect
                  , instruction=instrs ++ instruction
                  , workBegin=workBegin `orElse` Just eStart
                  , actions=if x == eaEffect then xs else (x \\\ eaEffect) : xs
                  })

      finish p MicroCode{..} = snd $ modifyProcess p $ do
        let start = fromMaybe (error "workBegin field is empty!") workBegin
        let duration = (eStart + eDuration) - start
        h <- add (Event start duration) (fb :: FB v) -- fb
        mapM_ (relation . Vertical h) compiler
        mapM_ (relation . Vertical h) effect
        mapM_ (relation . Vertical h) instruction

      act2Signal addr (Pull _) = Load addr :: I v t
      act2Signal addr (Push _) = Save addr
      nop = Nop :: I v t

  process = sortPuSteps . frProcess





sortPuSteps p@Process{..} =
  let hierarchy = foldl (\m (Vertical a b) -> M.adjust (b :) a m)
                      (M.fromList [(k, []) | k <- map uid steps])
                      [x | x@(Vertical _ _) <- relations]
      (graph, _v2k, k2v) = G.graphFromEdges $ map (\(a, b) -> ((), a, b)) $ M.assocs hierarchy
      steps' = sortBy (\Step{ uid=a } Step{ uid=b } ->
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


bindFB2Cell addr fb t = add (Event t 0) $ "Bind " ++ show fb ++ " to cell " ++ show addr
bindFB fb t = add (Event t 0) $ "Bind " ++ show fb


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


instance ( Var v, Time t ) => TestBench Fram Passive v t where
  fileName _ = "hdl/dpu_fram"

  testControl fram@Fram{ frProcess=Process{..}, ..} values =
    concatMap (\t -> passiveInputValue t steps values ++ "\n"
                     ++ showSignals (signalsAt t) ++ " @(negedge clk)\n"
              ) [ 0 .. tick + 1 ]
    where
      signalsAt time = map (\sig -> signal' fram sig time)
                       [ OE, WR, ADDR 3, ADDR 2, ADDR 1, ADDR 0 ]
      showSignals = (\[oe, wr, a3, a2, a1, a0] ->
                         "oe <= 'b" ++ oe
                         ++ "; wr <= 'b" ++ wr
                         ++ "; addr[3] <= 'b" ++ a3
                         ++ "; addr[2] <= 'b" ++ a2
                         ++ "; addr[1] <= 'b" ++ a1
                         ++ "; addr[0] <= 'b" ++ a0 ++ ";"
                    ) . map show

  testAsserts Fram{ frProcess=Process{..}, ..} values =
    concatMap (\t -> "@(posedge clk); #1; " ++ assert t ++ "\n"
              ) [ 0 .. tick + 1 ]
    ++ outputAssert
    where
      assert time = case infoAt time steps of
        [Pull (v:_)]
          | v `M.member` values -> checkBus v $ values M.! v
          | [FB fb :: FB v] <- infoAt time steps
          , Just (Loop _ _ :: Loop v) <- cast fb
          , [Load addr :: I v t] <- infoAt time steps
          -> checkBus v (0x0A00 + addr)
        (_ :: [Effect v]) -> "/* assert placeholder */"
      outputAssert = "\n\n@(posedge clk);\n" ++ loopsOutput ++ outputs
      loopsOutput = concat
        [ checkBank addr (values M.! a)
        | (Step{ time=Event{..}, .. }, FB fb :: FB v) <- filterSteps steps
        , let fb' = cast fb :: Maybe (Loop v)
        , isJust fb'
        , let Just (Loop _bs a) = fb'
        , a `M.member` values
        , let Save addr : _ = infoAt (eStart + eDuration - 1) steps :: [I v t]
        ]
      outputs = concat
        [ checkBank addr (values M.! v)
        | (Step{ time=Event{..}, .. }, FB fb :: FB v) <- filterSteps steps
        , let fb' = cast fb :: Maybe (FramOutput v)
        , isJust fb'
        , let Just (FramOutput addr v) = fb'
        , v `M.member` values
        ]

      checkBus var value = "if ( !(value_o == " ++ show value ++ ") ) "
        ++ "$display(\"FAIL Bus assertion failed (" ++ (filter ('\"' /=) $ show var)
        ++ ")! got: %h expect: %h\", value_o, "
        ++ show value ++ ");"
      checkBank addr v = "if ( !(fram.bank[" ++ show addr ++ "] == " ++ show v ++ ") ) "
        ++ "$display(\"FAIL Bank[%d] assertion failed! %h %h\", "
        ++ show addr ++ ", value_o, " ++ show v ++ ");\n"
