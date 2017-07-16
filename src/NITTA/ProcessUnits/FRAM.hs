{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
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
import           Data.List             (find, sortBy)
import qualified Data.Map              as M
import           Data.Maybe
import           NITTA.Base
import           NITTA.FunctionBlocks
import           NITTA.Types


data FRAM ty v t = FRAM
  { frMemory  :: Array Int (Cell v t)
  , frRemains :: [MicroCode v t]
  , frProcess :: Process v t
  } deriving (Show)

data Cell v t = Cell
  { next  :: Maybe (MicroCode v t)
  , queue :: [MicroCode v t]
  , final :: Maybe (MicroCode v t)
  } deriving (Show)

data MicroCode v t = MicroCode
  { actions                       :: [Effect v]
  , isFirst                       :: Bool
  , isLast                        :: Bool
  , fb                            :: FB v
  , compiler, effect, instruction :: [ProcessUid]
  , workBegin                     :: Maybe t
  }

microcode fb isFirst isLast acts  = MicroCode acts isFirst isLast fb [] [] [] Nothing
withCompiler mc@MicroCode{..} key = mc{ compiler=key : compiler }


instance ( Var v, Time t ) => Default (FRAM ty v t) where
  def = FRAM { frMemory=listArray (0, 35) $ repeat def
             , frRemains=[]
             , frProcess=def
             }

instance Default (Cell v t) where
  def = Cell Nothing [] Nothing


instance ( Show v ) => Show (MicroCode v t) where
  show MicroCode{..} = show actions


instance ( Eq v ) => Eq (MicroCode v t) where
  MicroCode{ actions=a } == MicroCode{ actions=b } = a == b




instance ( Var v, Time t
         ) => TestBench FRAM Passive v t where

  fileName _ = "hdl/dpu_fram_tb."
  processFileName _ = "hdl/dpu_fram_tb.process.v"

  testBench fram@FRAM{ frProcess=Process{..}, ..} =
    let signalValues time = map (\sig -> signal' fram sig time)
                           [ OE, WR, ADDR 3, ADDR 2, ADDR 1, ADDR 0 ]
        values = map ((\[oe, wr, a3, a2, a1, a0] ->
                         "oe <= b'" ++ oe
                         ++ "; wr <= b'" ++ wr
                         ++ "; addr[3] <= b'" ++ a3
                         ++ "; addr[2] <= b'" ++ a2
                         ++ "; addr[1] <= b'" ++ a1
                         ++ "; addr[0] <= b'" ++ a0 ++ ";"
                     ) . map show . signalValues) [ 0 .. tick + 1 ]
    in concatMap (++ " @(negedge clk);\n") values



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
        Just $ snd $ bind' $ microcode fb False False [ Push a, Pull b ]
    | Just (FRAMInput addr v) <- unbox fb =
        let (mc, fr') = bind' $ microcode fb True False [ Pull v ]
        in Just $ bindToCell fr' addr mc
    | Just (FRAMOutput addr v) <- unbox fb =
        let (mc, fr') = bind' $ microcode fb False True [ Push v ]
        in Just $ bindToCell fr' addr mc
    | Just (Loop a b) <- unbox fb =
        Just $ snd $ bind' $ microcode fb True True [ Pull b, Push a ]
    | otherwise = Nothing
      where
        bind' mc =
          let (key, p') = modifyProcess p $ bindFB fb tick
              mc' = mc{ compiler=key : compiler mc }
          in (mc', fr{ frRemains=mc' : frRemains
                     , frProcess=p'
                     })

  variants FRAM{ frProcess=Process{..}, ..} = fromCells ++ fromRemain
    where
      fromCells = [ PUVar v $ constrain v
                  | (_addr, cell) <- assocs frMemory
                  , v <- cell2acts cell
                  ]
      fromRemain
        | Nothing <- freeCell frMemory = []
        | otherwise = map ((\x -> PUVar x $ constrain x) . head . actions) frRemains
      constrain (Pull _) = TimeConstrain 1 tick maxBound
      constrain (Push _) = TimeConstrain 1 tick maxBound

  step fr@FRAM{ frProcess=p0@Process{..}, .. } act@PUAct{ aAt=at@Event{..}, .. }
    | tick > eStart = error "You can't start work yesterday:)"
    | Just mc <- find ((<< aEffect) . head . actions) frRemains
    = case freeCell frMemory of
        Just addr ->
          let fr' = (bindToCell fr addr mc)
                { frRemains=filter (/= mc) frRemains
                }
          in step fr' act
        Nothing -> error "Can't find free cell!"

    | Just (addr, cell) <- find (any (<< aEffect) . cell2acts . snd) $ assocs frMemory
    = case cell of
        Cell{ next=Just mc@MicroCode{ actions=x : _, .. } } | x << aEffect ->
            let (p', mc') = doAction addr p0 mc
            in fr{ frMemory=frMemory // [(addr,
                                          if isFirst && isLast
                                          then cell{ next=Nothing, final=mc' }
                                          else cell{ next=mc' }
                                         )]
                 , frProcess=p'
                 }
        Cell{ queue=mcs }
          | not $ null mcs
          , Just mc@MicroCode{..} <- find ((<< aEffect) . head . actions) mcs ->
              let (p', mc') = doAction addr p0 mc
              in fr{ frMemory=frMemory // [(addr, cell{ next=mc'
                                                      , queue=filter (/= mc) mcs
                                                      })]
                   , frProcess=p'
                   }
        Cell{ final=Just mc@MicroCode{ actions=(x : _) } } | x << aEffect ->
            let (p', mc') = doAction addr p0 mc
            in fr{ frMemory=frMemory // [(addr, cell { final=mc' })]
                 , frProcess=p'
                 }
        _ -> error "Internal FRAM error, step"

    | otherwise = error $ "Can't found selected action: " ++ show act ++ show (variants fr)
    where
      doAction addr p mc@MicroCode{..} =
        let (p', mc'@MicroCode{ actions=acts'}) = mkWork addr p mc
        in if null acts'
           then (finish p' mc', Nothing)
           else (p', Just mc')


      mkWork _addr _p MicroCode{ actions=[] } = error "FRAM internal error, mkWork"
      mkWork addr p mc@MicroCode{ actions=x:xs, ..} =
        let ((ef, instrs), p') = modifyProcess p $ do
              e <- add at aEffect
              i1 <- add at $ act2Signal addr aEffect
              is <- if tick < eStart
                then do
                  i2 <- add (Event tick (eStart - tick)) nop
                  return [ i1, i2 ]
                else return [ i1 ]
              mapM_ (relation . Vertical ef) instrs
              setTime (eStart + eDuration)
              return (e, is)
        in (p', mc{ effect=ef : effect
                  , instruction=instrs ++ instruction
                  , workBegin=workBegin `orElse` Just eStart
                  , actions=if x == aEffect then xs else (x \\ aEffect) : xs
                  })

      finish p MicroCode{..} = snd $ modifyProcess p $ do
        let start = fromMaybe (error "workBegin field is empty!") workBegin
        let duration = (eStart + eDuration) - start
        h <- add (Event start duration) fb
        mapM_ (relation . Vertical h) compiler
        mapM_ (relation . Vertical h) instruction

      act2Signal addr (Pull _) = Load addr :: I v t
      act2Signal addr (Push _) = Save addr
      nop = Nop :: I v t

  process = sortPuSteps . frProcess




bindToCell fr@FRAM{ frProcess=p@Process{..}, ..} addr mc =
  let (key, p') = modifyProcess p $ bindFB2Cell addr fb tick
      mc'@MicroCode{ fb=fb } = mc{ compiler=key : compiler mc }
      c' = case (frMemory ! addr, mc') of
        -- Reg
        (c@Cell{ final=Nothing, queue=q }, MicroCode{ isFirst=False, isLast=False }) ->
          c{ queue=mc' : q }
        -- Input
        (c@Cell{ next=Nothing }, MicroCode{ isFirst=True, isLast=False }) ->
          c{ next=Just mc' }
        -- Output
        (c@Cell{ final=Nothing }, MicroCode{ isFirst=False, isLast=True }) ->
          c{ final=Just mc' }
        -- Loop
        (c@Cell{ next=Nothing, final=Nothing }, MicroCode{ isFirst=True, isLast=True
                                                         -- , actions=[ f, l ]
                                                         }) ->
          c{ next=Just mc' -- { actions=[f, l] }
           , final=Just undefined -- mc'{ actions=[] }
           }
        _ -> error "Strange MicroCode and cell composition in FRAM!"
  in fr{ frMemory=frMemory // [(addr, c')]
       , frRemains=filter (/= mc') frRemains
       , frProcess=p'
       }


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


cell2acts Cell { next=Just MicroCode{ actions=x:_ } } = [x]
cell2acts Cell { queue=mcs } | not $ null mcs = map (head . actions) mcs
cell2acts Cell { final=Just MicroCode{actions=x:_ } } = [x]
cell2acts _ = []


freeCell memory =
  let cells = filter (isNothing . next . snd) $ assocs memory
      cells' = sortBy (\a b -> load a `compare` load b) cells
  in case cells' of []  -> Nothing
                    x:_ -> Just $ fst x
  where
    load (_, Cell _ mcs mc) = sum [ sum $ map (length . actions) mcs
                                  , maybe 0 (length . actions) mc
                                  ]
