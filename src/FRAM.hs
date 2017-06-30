{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE UndecidableInstances   #-}


module FRAM where

import           Base
import           Control.Monad.State
import           Data.Default
import           Data.Dynamic          (Dynamic, Typeable, fromDynamic, toDyn)
import           Data.Generics.Aliases (orElse)
import           Data.List             (find, sortBy)
import qualified Data.List             as L
import           Data.Map              (Map, fromList, lookup, (!))
import qualified Data.Map              as M
import           Data.Maybe            (catMaybes, fromMaybe, isNothing)
import           FB                    (FB (..), unbox)
import qualified FB




class ( Typeable a, Num a, Eq a, Ord a, Enum a, Show a ) => Addr a
instance ( Typeable a, Num a, Eq a, Ord a, Enum a, Show a ) => Addr a


data FRAM a variant action v t key = FRAM
  { frLastSave :: Maybe a
  , frMemory   :: Map a (Cell v key t)
  , frRemains  :: [MC v key t]
  , frProcess  :: Process v key t
  } deriving Show

instance ( Addr a, Enum key, Var v, Time t
         , Default t, Default key
         ) => Default (FRAM a variant action v t key) where
  def = FRAM { frLastSave=Nothing
             , frMemory=fromList [ (addr, def) | addr <- [0..19] ]
             , frRemains=[]
             , frProcess=def
             }

instance ( Addr a, Enum key, Var v, Time t
         ) => PUClass (FRAM a
                        (Variant v t) (Action v t) v t key
                      ) (Variant v t) (Action v t) v t key where
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
      fromCells = [ (v, time v $ Just addr)
                  | (addr, cell) <- M.assocs frMemory
                  , v <- cell2acts cell
                  ]
      fromRemain
        | Nothing <- freeCell frMemory = []
        | otherwise = map ((\x -> (x, time x Nothing)) . head . actions) frRemains
      time (Pull _) addr | addr == frLastSave = TimeConstrain 1 1 maxBound
      time (Pull _) _    = TimeConstrain 1 0 maxBound
      time (Push _) _    = TimeConstrain 1 1 maxBound

  step dpu@FRAM{ frProcess=p@Process{..}, .. } (act, Moment{..})
    | Just (addr, cell) <- find (any (<< act) . cell2acts . snd) $ M.assocs frMemory
    = case cell of
        Cell { next=Just mc@MC{ cntx=Cntx{..}, actions=(x:_) } } | x << act ->
            let (p', mc') = doAction p mc addr
            in dpu{ frMemory=M.insert addr (cell { next=mc' }) frMemory
                  , frProcess=p'
                  }

        Cell { queue=lst@(_:_) }
          | Just mc@MC{ cntx=Cntx{..}, .. } <- find ((<< act) . head . actions) lst ->
            let queue' = filter (/= mc) lst
                (p', mc') = doAction p mc addr
            in dpu{ frMemory=M.insert addr (cell { next=mc', queue=queue' }) frMemory
                  , frProcess=p'
                  }

        Cell { final=Just mc@MC{ cntx=Cntx{..}, actions=(x:_) } } | x << act ->
            let (p', mc') = doAction p mc addr
            in dpu{ frMemory=M.insert addr (cell { final=mc' }) frMemory
                  , frProcess=p'
                  }

    | Just mc@MC { cntx=rd@Cntx{..}, .. } <- find ((<< act) . head . actions) frRemains
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
              m <- add (Interaction act) (Interval eStart eDuration)
              l1 <- add (Signal $ act2Signal addr act) (Interval eStart eDuration)
              ls <- if tick < eStart
                then do
                  l2 <- add (Signal nop) (Interval tick (eStart - 1))
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
                     , workBegin=workBegin `orElse` Just tick
                     , signalLevel=ls ++ signalLevel
                     }
        in (p', rd', if x == act then xs else (x \\ act) : xs)

      finish p Cntx{..} = snd $ modifyProcess p $ do
        h <- add (FunctionBlock fb) (Interval (fromMaybe undefined workBegin) (eStart + eDuration))
        mapM_ (relation . Vertical h) compilerLevel
        mapM_ (relation . Vertical h) middleLevel
        -- relation $ Seq compilerLevel
        relation $ Seq middleLevel
        relation $ Seq signalLevel

      act2Signal addr (Pull _) = Save addr
      act2Signal addr (Push _) = Load addr

  process = frProcess





















data Cell var key time = Cell
  { next  :: Maybe (MC var key time)
  , queue :: [MC var key time]
  , final :: Maybe (MC var key time)
  } deriving (Show)

instance Default (Cell v i t) where
  def = Cell Nothing [] Nothing



data MC var key time = MC
  { actions :: [Interaction var]
  , cntx    :: Context key var time
  }

instance (Show var) => Show (MC var key time) where
  show MC {..} = show actions

instance (Eq var) => Eq (MC var key time) where
  MC { actions=a } == MC { actions=b } = a == b




data Context key var time = Cntx
  { fb                                      :: FB var
  , compilerLevel, middleLevel, signalLevel :: [key]
  , workBegin                               :: Maybe time
  }

instance Default (Context key var time) where
  def = Cntx undefined [] [] [] Nothing






addMap addr fb tick = add (Compiler ("FRAM: map " ++ show fb ++ " on " ++ show addr)) (Event tick)

addEval fb tick = add (Compiler ("FRAM: evaluate " ++ show fb)) (Event tick)



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



