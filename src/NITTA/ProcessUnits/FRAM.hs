-- {-# OPTIONS -Wall -fno-warn-missing-signatures #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module NITTA.ProcessUnits.FRAM where

import           Data.Array
import           Data.Bits
import           Data.Default
import           Data.Generics.Aliases (orElse)
import qualified Data.Graph            as G
import           Data.List             (find, minimumBy, sortBy)
import qualified Data.Map              as M
import           Data.Maybe
import           NITTA.Base
import           NITTA.FunctionBlocks
import           NITTA.Types
import           Prelude               hiding (last)


data FRAM ty v t = FRAM
  { frMemory  :: Array Int (Cell v t)
  , frRemains :: [MicroCode v t]
  , frProcess :: Process v t
  } deriving (Show)


data IOState v t = Undef
                 | Def (MicroCode v t)
                 | UsedOrBlocked
  deriving (Show, Eq)


data Cell v t = Cell
  { input     :: IOState v t -- Pull
  , current   :: Maybe (MicroCode v t)
  , output    :: IOState v t -- Push
  , queue     :: [MicroCode v t] -- [Push, Pull]
  , lastWrite :: Maybe t
  } deriving (Show)


data MicroCode v t = MicroCode
  { compiler, effect, instruction :: [ProcessUid]
  , workBegin                     :: Maybe t
  , fb                            :: FB v
  , actions                       :: [Effect v]
  , bindTo                        :: MicroCode v t -> Cell v t -> Maybe (Cell v t)
  }

microcode = MicroCode [] [] [] Nothing


instance ( Var v, Time t ) => Default (FRAM ty v t) where
  def = FRAM { frMemory=listArray (0, 35) $ repeat def
             , frRemains=[]
             , frProcess=def
             }

instance Default (Cell v t) where
  def = Cell Undef Nothing Undef [] Nothing

instance ( Show v ) => Show (MicroCode v t) where
  show MicroCode{..} = show actions

instance ( Eq v ) => Eq (MicroCode v t) where
  MicroCode{ actions=a } == MicroCode{ actions=b } = a == b





type I v t = Instruction FRAM v t

instance ( Var v, Time t ) => PUClass FRAM Passive v t where

  data Signals FRAM = OE | WR | ADDR Int

  data Instruction FRAM v t
    = Nop
    | Load Int
    | Save Int
    deriving (Show)

  signal' FRAM{..} sig time =
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


  bind fr@FRAM{ frProcess=p@Process{..}, .. } fb
    | Just (Reg a b) <- unbox fb =
        let bindToCell _ Cell{ input=UsedOrBlocked, output=UsedOrBlocked} = Nothing
            bindToCell mc cell@Cell{ queue=q } = Just cell{ queue=mc : q }
            ( mc', fr' ) = bind' $ microcode fb [ Push a, Pull b ] bindToCell
        in Just fr'{ frRemains=mc' : frRemains }
    | Just (Loop a b) <- unbox fb =
        let bindToCell mc cell@Cell{ input=Undef, output=Undef } = Just cell{ input=Def mc
                                                                            , output=UsedOrBlocked
                                                                            }
            bindToCell _ _                                       = Nothing
            ( mc', fr' ) = bind' $ microcode fb [ Pull b, Push a ] bindToCell
        in Just fr'{ frRemains=mc' : frRemains }
    | Just (FRAMInput addr v) <- unbox fb =
        let bindToCell mc cell@Cell{ input=Undef } = Just cell{ input=Def mc }
            bindToCell _ _                         = Nothing
            ( mc', fr' ) = bind' $ microcode fb [ Pull v ] bindToCell
            Just cell' = bindToCell mc' $ frMemory ! addr
        in Just fr'{ frMemory=frMemory // [(addr, cell')] }
    | Just (FRAMOutput addr v) <- unbox fb =
        let bindToCell mc cell@Cell{ output=Undef } = Just cell{ output=Def mc }
            bindToCell _ _                          = Nothing
            ( mc', fr' ) = bind' $ microcode fb [ Push v ] bindToCell
            Just cell' = bindToCell mc' $ frMemory ! addr
        in Just fr'{ frMemory=frMemory // [(addr, cell')] }
    | otherwise = Nothing
      where
        bind' mc =
          let (key, p') = modifyProcess p $ bindFB fb tick
              mc' = mc{ compiler=key : compiler mc }
          in ( mc', fr{ frProcess=p' } )

  variants FRAM{ frProcess=Process{..}, ..} = fromCells ++ fromRemain
    where
      fromCells = [ PUVar v $ constrain cell v
                  | (_addr, cell) <- assocs frMemory
                  , v <- cell2acts cell
                  ]
      fromRemain = [ PUVar act $ constrain cell act
                   | mc@MicroCode { actions=act:_, bindTo=bindTo } <- frRemains
                   , let Just (_addr, cell) = availableCell frMemory (isJust . bindTo mc)
                   ]
      constrain Cell{..} (Pull _)
        | lastWrite == Just tick = TimeConstrain 1 (tick + 1) maxBound
        | otherwise              = TimeConstrain 1 tick maxBound
      constrain _cell (Push _) = TimeConstrain 1 tick maxBound

  step fr@FRAM{ frProcess=p0@Process{ tick=tick0 }, .. } act0@PUAct{ aAt=at@Event{..}, .. }
    | tick0 > eStart = error "You can't start work yesterday:)"
    | Just mc@MicroCode{ bindTo=bindTo } <- find ((<< aEffect) . head . actions) frRemains
    = case availableCell frMemory (isJust . bindTo mc) of
        Just (addr, cell) ->
          let (key, p') = modifyProcess p0 $ bindFB2Cell addr (fb mc) tick0
              Just cell' = bindTo mc{ compiler=key : (compiler mc) } cell
              fr' = fr{ frRemains=filter (/= mc) frRemains
                      , frMemory=frMemory // [(addr, cell')]
                      , frProcess=p'
                      }
          in step fr' act0
        Nothing -> error "Can't find available cell!"

    | Just (addr, cell) <- find (any (<< aEffect) . cell2acts . snd) $ assocs frMemory
    = case cell of
        Cell{ input=Def mc@MicroCode{ actions=act : _ } } | act << aEffect ->
            let (p', mc') = doAction addr p0 mc
                cell' = updateLastWrite (tick p') cell
                cell'' = case mc' of
                  Nothing -> cell'{ input=UsedOrBlocked }
                  Just mc''@MicroCode{ actions=Pull _ : _ } -> cell'{ input=Def mc'' }
                  Just mc''@MicroCode{ actions=Push _ : _ }
                    | (output cell') == UsedOrBlocked ->
                        cell'{ input=UsedOrBlocked, output=Def mc'' }
            in fr{ frMemory=frMemory // [(addr, cell'')]
                 , frProcess=p'
                 }
        Cell{ current=Just mc@MicroCode{ actions=act : _ } } | act << aEffect ->
            let (p', mc') = doAction addr p0 mc
                cell' = updateLastWrite (tick p') cell
                cell'' = cell'{ input=UsedOrBlocked
                              , current=mc'
                              }
            in fr{ frMemory=frMemory // [(addr, cell'')]
                 , frProcess=p'
                 }
        Cell{ queue=mcs }
          | not $ null mcs
          , Just mc@MicroCode{..} <- find ((<< aEffect) . head . actions) mcs ->
              let (p', mc') = doAction addr p0 mc
                  cell' = updateLastWrite (tick p') cell
                  cell'' = cell'{ input=UsedOrBlocked
                                , current=mc'
                                , queue=filter (/= mc) mcs
                                }
              in fr{ frMemory=frMemory // [(addr, cell'')]
                   , frProcess=p'
                   }
        Cell{ output=Def mc@MicroCode{ actions=act : _ } } | act << aEffect ->
            let (p', mc') = doAction addr p0 mc
                -- Вот тут есть потенциальная проблема которую не совсем ясно как можно решить,
                -- а именно, если output происходит в последний такт вычислительного цикла
                -- а input с него происходит в первый такт вычислительного цикла.
                cell' = cell{ input=UsedOrBlocked
                            , output=UsedOrBlocked
                            }
            in fr{ frMemory=frMemory // [(addr, cell')]
                 , frProcess=p'
                 }
        _ -> error "Internal FRAM error, step"

    | otherwise = error $ "Can't found selected action: " ++ show act0 ++ show (variants fr)
    where
      updateLastWrite t cell | Push _ <- aEffect = cell{ lastWrite=Just t }
                             | otherwise = cell{ lastWrite=Nothing }

      doAction addr p mc@MicroCode{..} =
        let (p', mc'@MicroCode{ actions=acts' }) = mkWork addr p mc
        in if null acts'
           then (finish p' mc', Nothing)
           else (p', Just mc')

      mkWork _addr _p MicroCode{ actions=[] } = error "FRAM internal error, mkWork"
      mkWork addr p mc@MicroCode{ actions=x:xs, ..} =
        let ((ef, instrs), p') = modifyProcess p $ do
              e <- add at aEffect
              i1 <- add at $ act2Signal addr aEffect
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
                  , actions=if x == aEffect then xs else (x \\\ aEffect) : xs
                  })

      finish p MicroCode{..} = snd $ modifyProcess p $ do
        let start = fromMaybe (error "workBegin field is empty!") workBegin
        let duration = (eStart + eDuration) - start
        h <- add (Event start duration) (fb :: FB v) -- fb
        mapM_ (relation . Vertical h) compiler
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


bindFB2Cell addr fb t = add (Event t 0) ("Bind " ++ show fb ++ " to cell " ++ show addr)
bindFB fb t = add (Event t 0) ("Bind " ++ show fb)


cell2acts Cell{ input=Def MicroCode{ actions=x:_ } } = [x]
cell2acts Cell{ current=Just MicroCode{ actions=x:_ } } = [x]
cell2acts Cell{ queue=mcs@(_:_) } = map (head . actions) mcs
cell2acts Cell{ output=Def MicroCode{actions=x:_ } } = [x]
cell2acts _ = []


availableCell frMemory pred =
  case filter (\(addr, cell) -> isNothing (current cell) && pred cell) $ assocs frMemory of
    []    -> Nothing
    cells -> Just $ minimumBy (\a b -> (load a) `compare` (load b)) cells
  where
    load (_addr, Cell{..}) = sum [ 2 * length queue
                                 , if input == Undef then -1 else 0
                                 , if output == Undef then -1 else 0
                                 ]


---------------------------------------------------


instance ( Var v, Time t ) => TestBench FRAM Passive v t where
  fileName _ = "hdl/dpu_fram_tb"

  testControl fram@FRAM{ frProcess=Process{..}, ..} values =
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

  testAsserts fram@FRAM{ frProcess=Process{..}, ..} values =
    concatMap (\t -> "@(posedge clk); #1; " ++ assert t ++ "\n"
                -- ++ "$display(\"%h  %h  %h\", fram.bank[0], fram.bank[1], fram.bank[2]);\n"
              ) [ 0 .. tick + 1 ]
    where
      assert time = case infoAt time steps of
        [Pull (v:_)]
          | v `M.member` values ->
            "if ( !(value_o == " ++ show (values M.! v) ++ ") ) $display(\"Assertion failed!\", value_o, " ++ show (values M.! v) ++ ");"
        (_ :: [Effect v]) -> "/* assert placeholder */"
