{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE UndecidableInstances   #-}

module NITTA.ProcessUnits.FRAM where

import           Control.Monad.State
import           Data.Default
import           Data.Dynamic          (Dynamic, Typeable, fromDynamic, toDyn)
import           Data.Generics.Aliases (orElse)
import qualified Data.Graph            as G
import           Data.List             (find, intersect, partition, sortBy)
import qualified Data.List             as L
import           Data.Map              (Map, fromList, lookup, (!))
import qualified Data.Map              as M
import           Data.Maybe            (catMaybes, fromMaybe, isNothing)
import           NITTA.Base
import           NITTA.FunctionBlocks
import qualified NITTA.FunctionBlocks  as FB
import           NITTA.ProcessUnits



data FRAM a (variant :: * -> * -> *) (action :: * -> * -> *) (step :: * -> * -> * -> *) v t k = FRAM
  { frLastSave :: Maybe a
  , frMemory   :: Map a (Cell v t k)
  , frRemains  :: [MC v t k]
  , frProcess  :: Process step v t k
  } deriving Show



instance ( Addr a, Key k, Enum k, Var v, Time t
         , Default t, Default k
         ) => Default (FRAM a variant action step v t k) where
  def = FRAM { frLastSave=Nothing
             , frMemory=fromList [ (addr, def) | addr <- [0..19] ]
             , frRemains=[]
             , frProcess=def
             }

instance ( Addr a, Key k, Var v, Time t
         ) => PUClass (FRAM a) PuVariant PuAction PuStep v t k where
  evaluate dpu@FRAM{ frProcess=p@Process{..}, .. } fb
    | Just (FB.Reg a b) <- unbox fb =
        let (key, p') = modifyProcess p $ addEval fb tick
            mc = MC [Push a, Pull b] def { fb=fb, compilerLevel=[key] }
        in Just $ dpu
           { frRemains=mc : frRemains
           , frProcess=p'
           }

    | Just (FB.FRAMInput addr v) <- unbox fb
    = case M.lookup addr frMemory of
        Just cell@Cell { next=Nothing } ->
          let (keys, p') = modifyProcess p $ do
                k1 <- addEval fb tick
                k2 <- addMap addr fb tick
                return [k1, k2]
              mc = MC [Pull v] def { fb=fb
                                   , compilerLevel=keys
                                   }
          in Just dpu
             { frMemory=M.insert addr (cell { next=Just mc }) frMemory
             , frProcess=p'
             }
        Nothing -> error "Memory cell already locked or not exist!"

    | Just (FB.FRAMOutput addr v) <- unbox fb
    = case M.lookup addr frMemory of
        Just cell@Cell { final=Nothing } ->
          let (keys, p') = modifyProcess p $ do
                k1 <- addEval fb tick
                k2 <- addMap addr fb tick
                return [k1, k2]
              mc = MC [Push v] def { fb=fb
                                   , compilerLevel=keys
                                   }
          in Just dpu
             { frMemory=M.insert addr (cell { final=Just mc }) frMemory
             , frProcess=p'
             }
        Nothing -> error "Memory cell already locked or not exist!"

    | otherwise = Nothing

  variants dpu@FRAM{..} = fromCells ++ fromRemain
    where
      fromCells = [ Interaction v (constrain v $ Just addr)
                  | (addr, cell) <- M.assocs frMemory
                  , v <- cell2acts cell
                  ]
      fromRemain
        | Nothing <- freeCell frMemory = []
        | otherwise = map ((\x -> Interaction x (constrain x Nothing)) . head . actions) frRemains
      constrain (Pull _) addr | addr == frLastSave = TimeConstrain 1 1 maxBound
      constrain (Pull _) _    = TimeConstrain 1 0 maxBound
      constrain (Push _) _    = TimeConstrain 1 1 maxBound

  step dpu@FRAM{ frProcess=p@Process{..}, .. } Interaction{ at=at@Event{..}, .. }
    | tick > eStart = error "You can't start work yesterday:)"
    | Just (addr, cell) <- find (any (<< effect) . cell2acts . snd) $ M.assocs frMemory
    = case cell of
        Cell { next=Just mc@MC{ cntx=Cntx{..}, actions=(x:_) } } | x << effect ->
            let (p', mc') = doAction p mc addr
            in dpu{ frMemory=M.insert addr (cell { next=mc' }) frMemory
                  , frProcess=p'
                  }

        Cell { queue=lst@(_:_) }
          | Just mc@MC{ cntx=Cntx{..}, .. } <- find ((<< effect) . head . actions) lst ->
            let queue' = filter (/= mc) lst
                (p', mc') = doAction p mc addr
            in dpu{ frMemory=M.insert addr (cell { next=mc', queue=queue' }) frMemory
                  , frProcess=p'
                  }

        Cell { final=Just mc@MC{ cntx=Cntx{..}, actions=(x:_) } } | x << effect ->
            let (p', mc') = doAction p mc addr
            in dpu{ frMemory=M.insert addr (cell { final=mc' }) frMemory
                  , frProcess=p'
                  }

    | Just mc@MC { cntx=rd@Cntx{..}, .. } <- find ((<< effect) . head . actions) frRemains
    = case freeCell frMemory of
        Just (addr, cell) ->
          let (p', mc') = addMapToCell p mc addr
              (p'', mc'') = doAction p' mc' addr
          in dpu { frMemory=M.insert addr (cell { next=mc'' }) frMemory
                 , frProcess=p''
                 , frRemains=filter (/= mc) frRemains
                 }
        Nothing -> error "Can't find free cell!"

    | otherwise = error "Can't found selected action!"
    where
      addMapToCell p mc@MC{ cntx=cntx@Cntx{..} } addr =
        let (c, p') = modifyProcess p $ addMap addr fb tick
        in (p', mc{ cntx=cntx{ compilerLevel=c : compilerLevel } })

      doAction p mc@MC{ cntx=rd@Cntx{ .. }, .. } addr  =
        let (process', rd', actions') = makeStepWork p rd actions addr
            (process'', mc') = if null actions'
              then ( finish process' rd', Nothing )
              else ( process'           , Just $ mc{ actions=actions', cntx=rd' } )
        in (process'', mc')

      makeStepWork p rd@Cntx{..} (x:xs) addr =
        let ((m, ls), p') = modifyProcess p $ do
              m <- add' (Effect effect) at
              l1 <- add' (Signal $ act2Signal addr effect) at
              ls <- if tick < eStart
                then do
                  l2 <- add' (Signal nop) $ Event tick (eStart - tick)
                  -- relation $ Seq [l1, l2]
                  relation $ Vertical m l1
                  relation $ Vertical m l2
                  return [l1, l2]
                else do
                  relation $ Vertical m l1
                  return [l1]
              setTime (eStart + eDuration)
              return (m, ls)
            rd' = rd { middleLevel=m : middleLevel
                     , workBegin=workBegin `orElse` Just eStart
                     , signalLevel=ls ++ signalLevel
                     }
        in (p', rd', if x == effect then xs else (x \\ effect) : xs)

      finish p Cntx{..} = snd $ modifyProcess p $ do
        let start = (fromMaybe undefined workBegin)
        let duration = (eStart + eDuration) - start
        h <- add' (FunctionBlock fb) (Event start duration)
        mapM_ (relation . Vertical h) compilerLevel
        mapM_ (relation . Vertical h) middleLevel
        relation $ Seq middleLevel
        relation $ Seq signalLevel

      act2Signal addr (Pull _) = Save addr
      act2Signal addr (Push _) = Load addr

  process = sortPuSteps . frProcess







sortPuSteps p@Process{..} =
  let hierarchy = foldl (\m (Vertical a b) -> M.adjust (b :) a m)
                      (M.fromList [(k, []) | k <- map key steps])
                      [x | x@(Vertical _ _) <- relations]
      (graph, v2k, k2v) = G.graphFromEdges $ map (\(a, b) -> ((), a, b)) $ M.assocs hierarchy
      steps' = sortBy (\Step{ key=a } Step{ key=b } ->
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




data Cell v t k = Cell
  { next  :: Maybe (MC v t k)
  , queue :: [MC v t k]
  , final :: Maybe (MC v t k)
  } deriving (Show)

instance Default (Cell v t k) where
  def = Cell Nothing [] Nothing



data MC v t k = MC
  { actions :: [Effect v]
  , cntx    :: Context v t k
  }

instance (Show v) => Show (MC v t k) where
  show MC {..} = show actions

instance (Eq v) => Eq (MC v t k) where
  MC { actions=a } == MC { actions=b } = a == b




data Context v t k = Cntx
  { fb                                      :: FB v
  , compilerLevel, middleLevel, signalLevel :: [k]
  , workBegin                               :: Maybe t
  }

instance Default (Context key var time) where
  def = Cntx undefined [] [] [] Nothing






addMap addr fb tick = add' (Compiler ("FRAM: map " ++ show fb ++ " on " ++ show addr)) (Event tick 0)

addEval fb tick = add' (Compiler ("FRAM: evaluate " ++ show fb)) (Event tick 0)

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

data Signal addr
  = Nop
  | Load addr
  | Save addr
  deriving (Show, Eq)

nop :: Signal Int
nop = Nop
