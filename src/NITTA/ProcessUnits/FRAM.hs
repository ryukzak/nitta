{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

module NITTA.ProcessUnits.FRAM where

import           Control.Monad.State
import           Data.Bits
import           Data.Default
import           Data.Dynamic          (Dynamic, Typeable, fromDynamic, toDyn)
import           Data.Generics.Aliases (orElse)
import qualified Data.Graph            as G
import           Data.List             (find, intersect, partition, sortBy)
import qualified Data.List             as L
import           Data.Map              (Map, fromList, lookup, (!))
import qualified Data.Map              as M
import           Data.Maybe            (catMaybes, fromMaybe, isNothing)
import           Data.Typeable         (cast)
import           NITTA.Base
import           NITTA.FunctionBlocks
import qualified NITTA.FunctionBlocks  as FB
import           NITTA.ProcessUnits



data FRAM a ty v t = FRAM
  { frLastSave :: Maybe a
  , frMemory   :: Map a (Cell v t)
  , frRemains  :: [MC v t]
  , frProcess  :: Process v t
  } deriving (Show)

data Cell v t = Cell
  { next  :: Maybe (MC v t)
  , queue :: [MC v t]
  , final :: Maybe (MC v t)
  } deriving (Show)

instance Default (Cell v t) where
  def = Cell Nothing [] Nothing

data MC v t = MC
  { actions :: [Effect v]
  , cntx    :: Context v t
  }

instance (Show v) => Show (MC v t) where
  show MC {..} = show actions

instance (Eq v) => Eq (MC v t) where
  MC { actions=a } == MC { actions=b } = a == b

data Context v t = Cntx
  { fb                                      :: FB v
  , compilerLevel, middleLevel, signalLevel :: [ProcessUid]
  , workBegin                               :: Maybe t
  }

instance Default (Context var time) where
  def = Cntx undefined [] [] [] Nothing





instance ( Addr a, Var v, Time t
         , Default t
         ) => Default (FRAM a ty v t) where
  def = FRAM { frLastSave=Nothing
             , frMemory=fromList [ (addr, def) | addr <- [0..36] ]
             , frRemains=[]
             , frProcess=def
             }



instance ( Addr a, Var v, Time t, Enum t
         ) => TestBench (FRAM a) Passive v t where
  fileName _ = "hdl/dpu_fram_tb."
  processFileName _ = "hdl/dpu_fram_tb.process.v"

  testBench fram@FRAM{ frProcess=Process{..}, ..} =
    let signalValues t = map (\signal -> signal' fram signal t)
                           [ OE, WR, ADDR 3, ADDR 2, ADDR 1, ADDR 0 ]
        values = map ((\[oe, wr, a3, a2, a1, a0] ->
                         "oe <= b'" ++ oe
                         ++ "; wr <= b'" ++ wr
                         ++ "; addr[3] <= b'" ++ a3
                         ++ "; addr[2] <= b'" ++ a2
                         ++ "; addr[1] <= b'" ++ a1
                         ++ "; addr[0] <= b'" ++ a0 ++ ";"
                     ) . (map show) . signalValues) [ 0 .. tick + 1 ]
    in concatMap (++ " @(negedge clk);\n") values







instance ( Addr a, Var v, Time t
         ) => PUClass (FRAM a) Passive v t where
  data Signals (FRAM a) = OE | WR | ADDR Int
    deriving (Typeable)

  data Instruction (FRAM a) v t
    = Nop
    | Load a
    | Save a
    deriving (Show)

  signal' fr@FRAM{..} s t =
    let i = case infoAt t $ steps frProcess of
          []                                     -> Nop
          ((i:[]) :: [Instruction (FRAM a) v t]) -> i
          x                                      -> error $ show x
    in value i s
    where
      value  Nop        (ADDR _) = X
      value  Nop         _       = B False
      value (Load addr) (ADDR b) = B $ testBit addr b
      value (Load addr)  OE      = B True
      value (Load addr)  WR      = B False
      value (Save addr) (ADDR b) = B $ testBit addr b
      value (Save addr)  OE      = B False
      value (Save addr)  WR      = B True

  evaluate fr@FRAM{ frProcess=p@Process{..}, .. } fb
    | Just (FB.Reg a b) <- unbox fb =
        let (key, p') = modifyProcess p $ evaluateFB fb tick
            mc = MC [Push a, Pull b] def { fb=fb, compilerLevel=[key] }
        in Just $ fr
           { frRemains=mc : frRemains
           , frProcess=p'
           }

    | Just (FB.FRAMInput addr v) <- unbox fb
    = case M.lookup addr frMemory of
        Just cell@Cell { next=Nothing } ->
          let (keys, p') = modifyProcess p $ do
                k1 <- evaluateFB fb tick
                k2 <- mapFB addr fb tick
                return [k1, k2]
              mc = MC [Pull v] def { fb=fb
                                   , compilerLevel=keys
                                   }
          in Just fr
             { frMemory=M.insert addr cell{ next=Just mc } frMemory
             , frProcess=p'
             }
        Nothing -> error "Memory cell already locked or not exist!"

    | Just (FB.FRAMOutput addr v) <- unbox fb
    = case M.lookup addr frMemory of
        Just cell@Cell { final=Nothing } ->
          let (keys, p') = modifyProcess p $ do
                k1 <- evaluateFB fb tick
                k2 <- mapFB addr fb tick
                return [k1, k2]
              mc = MC [Push v] def { fb=fb
                                   , compilerLevel=keys
                                   }
          in Just fr
             { frMemory=M.insert addr cell{ final=Just mc } frMemory
             , frProcess=p'
             }
        Nothing -> error "Memory cell already locked or not exist!"

    | otherwise = Nothing

  variants dpu@FRAM{ frProcess=Process{..}, ..} = fromCells ++ fromRemain
    where
      fromCells = [ PUVar v $ constrain v $ Just addr
                  | (addr, cell) <- M.assocs frMemory
                  , v <- cell2acts cell
                  ]
      fromRemain
        | Nothing <- freeCell frMemory = []
        | otherwise = map ((\x -> PUVar x $ constrain x Nothing) . head . actions) frRemains
      constrain (Pull _) addr | addr == frLastSave = TimeConstrain 1 tick maxBound
      constrain (Pull _) _    = TimeConstrain 1 tick maxBound
      constrain (Push _) _    = TimeConstrain 1 tick maxBound

  step dpu@FRAM{ frProcess=p@Process{..}, .. } (act@PUAct{ aAt=at@Event{..}, .. })
    | tick > eStart = error "You can't start work yesterday:)"
    | Just (addr, cell) <- find (any (<< aEffect) . cell2acts . snd) $ M.assocs frMemory
    = case cell of
        Cell { next=Just mc@MC{ cntx=Cntx{..}, actions=(x:_) } } | x << aEffect ->
            let (p', mc') = doAction p mc addr
            in dpu{ frMemory=M.insert addr (cell { next=mc' }) frMemory
                  , frProcess=p'
                  }

        Cell { queue=lst@(_:_) }
          | Just mc@MC{ cntx=Cntx{..}, .. } <- find ((<< aEffect) . head . actions) lst ->
            let queue' = filter (/= mc) lst
                (p', mc') = doAction p mc addr
            in dpu{ frMemory=M.insert addr (cell { next=mc', queue=queue' }) frMemory
                  , frProcess=p'
                  }

        Cell { final=Just mc@MC{ cntx=Cntx{..}, actions=(x:_) } } | x << aEffect ->
            let (p', mc') = doAction p mc addr
            in dpu{ frMemory=M.insert addr (cell { final=mc' }) frMemory
                  , frProcess=p'
                  }

    | Just mc@MC { cntx=rd@Cntx{..}, .. } <- find ((<< aEffect) . head . actions) frRemains
    = case freeCell frMemory of
        Just (addr, cell) ->
          let (p', mc') = addMapToCell p mc addr
              (p'', mc'') = doAction p' mc' addr
          in dpu { frMemory=M.insert addr (cell { next=mc'' }) frMemory
                 , frProcess=p''
                 , frRemains=filter (/= mc) frRemains
                 }
        Nothing -> error "Can't find free cell!"

    | otherwise = error ("Can't found selected action: " ++ show act ++ (show $ variants dpu))
    where
      addMapToCell p mc@MC{ cntx=cntx@Cntx{..} } addr =
        let (c, p') = modifyProcess p $ mapFB addr fb tick
        in (p', mc{ cntx=cntx{ compilerLevel=c : compilerLevel } })

      doAction p mc@MC{ cntx=rd@Cntx{ .. }, .. } addr  =
        let (process', rd', actions') = makeStepWork p rd actions addr
            (process'', mc') = if null actions'
              then ( finish process' rd', Nothing )
              else ( process'           , Just $ mc{ actions=actions', cntx=rd' } )
        in (process'', mc')

      makeStepWork p rd@Cntx{..} (x:xs) addr =
        let ((m, ls), p') = modifyProcess p $ do
              m <- add at aEffect
              l1 <- add at $ act2Signal addr aEffect
              ls <- if tick < eStart
                then do
                  l2 <- add (Event tick (eStart - tick)) nop
                  return [l1, l2]
                else return [l1]
              mapM_ (relation . Vertical m) ls
              setTime (eStart + eDuration)
              return (m, ls)
            rd' = rd { middleLevel=m : middleLevel
                     , workBegin=workBegin `orElse` Just eStart
                     , signalLevel=ls ++ signalLevel
                     }
        in (p', rd', if x == aEffect then xs else (x \\ aEffect) : xs)

      finish p Cntx{..} = snd $ modifyProcess p $ do
        let start = (fromMaybe undefined workBegin)
        let duration = (eStart + eDuration) - start
        h <- add (Event start duration) fb
        mapM_ (relation . Vertical h) compilerLevel
        mapM_ (relation . Vertical h) middleLevel

      act2Signal :: a -> Effect v -> Instruction (FRAM a) v t
      act2Signal addr (Pull _) = Load addr
      act2Signal addr (Push _) = Save addr
      nop :: Instruction (FRAM a) v t
      nop = Nop


  process = sortPuSteps . frProcess







sortPuSteps p@Process{..} =
  let hierarchy = foldl (\m (Vertical a b) -> M.adjust (b :) a m)
                      (M.fromList [(k, []) | k <- map uid steps])
                      [x | x@(Vertical _ _) <- relations]
      (graph, v2k, k2v) = G.graphFromEdges $ map (\(a, b) -> ((), a, b)) $ M.assocs hierarchy
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






mapFB addr fb t = add (Event t 0) ("Bind " ++ show fb ++ " to cell " ++ show addr)
evaluateFB fb t = add (Event t 0) ("Bind " ++ show fb)


cell2acts Cell { next=Just MC { actions=x:_ } } = [x]
cell2acts Cell { queue=mcs } | not $ null mcs = map (head . actions) mcs
cell2acts Cell { final=Just MC {actions=x:_ } } = [x]
cell2acts _ = []

freeCell memory =
  let cells = filter (isNothing . next . snd) $ M.assocs memory
      cells' = sortBy (\a b -> load a `compare` load b) cells
  in case cells' of []    -> Nothing
                    (x:_) -> Just x
  where
    load (_, Cell _ mcs mc) = sum [ sum $ map (length . actions) mcs
                                  , maybe 0 (length . actions) mc
                                  ]

