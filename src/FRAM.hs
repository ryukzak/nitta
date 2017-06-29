-- {-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards        #-}
-- {-# LANGUAGE UndecidableInstances   #-}

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



data FRAM addr var key time = FRAM
  { lastSave    :: Maybe addr
  , memoryState :: Map addr (Cell var key time)
  , remains     :: [MC var key time]
  , process     :: Process var key time
  } deriving Show


instance ( Typeable var, Eq var
         , Typeable addr, Ord addr, Show addr
         , Bounded time, Num time, Ord time
         , Enum key
         ) => DPUClass (FRAM addr var key time) key var time where
  proc = process
  evaluate dpu@FRAM { process=process@Process {..}, .. } fb
    | Just (FB.Reg a b) <- unbox fb =
        let (key, process') = modifyProcess process $ addEval fb tick
            mc = MC [Push a, Pull b] def { fb=fb, compilerLevel=[key] }
        in Just $ dpu
           { remains=mc : remains
           , process=process'
           }

    | Just (FB.FRAMInput addr v) <- unbox fb
    = case M.lookup addr memoryState of
        Just cell@Cell { next=Nothing } ->
          let (keys, process') = modifyProcess process $ do
                k1 <- addEval fb tick
                k2 <- addMap addr fb tick
                return [k1, k2]
              mc = MC [Pull v] def { fb=fb
                                   , compilerLevel=keys
                                   }
          in Just dpu
             { memoryState=M.insert addr (cell { next=Just mc }) memoryState
             , process=process'
             }
        Nothing -> error "Memory cell already locked or not exist!"

    | Just (FB.FRAMOutput addr v) <- unbox fb
    = case M.lookup addr memoryState of
        Just cell@Cell { final=Nothing } ->
          let (keys, process') = modifyProcess process $ do
                k1 <- addEval fb tick
                k2 <- addMap addr fb tick
                return [k1, k2]
              mc = MC [Push v] def { fb=fb
                                   , compilerLevel=keys
                                   }
          in Just dpu
             { memoryState=M.insert addr (cell { final=Just mc }) memoryState
             , process=process'
             }
        Nothing -> error "Memory cell already locked or not exist!"

    | otherwise = Nothing



  variants dpu@FRAM {..} = fromCells ++ fromRemain
    where
      fromCells = [ (v, time v $ Just addr)
                  | (addr, cell) <- M.assocs memoryState
                  , v <- cell2acts cell
                  ]
      fromRemain
        | Nothing <- freeCell memoryState = []
        | otherwise = map ((\x -> (x, time x Nothing)) . head . actions) remains
      time (Pull _) addr | addr == lastSave = TimeConstrain 1 1 maxBound
      time (Pull _) _    = TimeConstrain 1 0 maxBound
      time (Push _) _    = TimeConstrain 1 1 maxBound


  step dpu@FRAM{ process=process@Process{..}, .. } act begin end
    | Just (addr, cell) <- find (any (<< act) . cell2acts . snd) $ M.assocs memoryState
    = case cell of
        Cell { next=Just mc@MC{ cntx=Cntx{..}, actions=(x:_) } } | x << act ->
            let (p', mc') = doAction process mc addr
            in dpu { memoryState=M.insert addr (cell { next=mc' }) memoryState
                   , process=p'
                   }

        Cell { queue=lst@(_:_) }
          | Just mc@MC{ cntx=Cntx{..}, .. } <- find ((<< act) . head . actions) lst ->
            let queue' = filter (/= mc) lst
                (p', mc') = doAction process mc addr
            in dpu { memoryState=M.insert addr (cell { next=mc', queue=queue' }) memoryState
                   , process=p'
                   }

        Cell { final=Just mc@MC{ cntx=Cntx{..}, actions=(x:_) } } | x << act ->
            let (p', mc') = doAction process mc addr
            in dpu { memoryState=M.insert addr (cell { final=mc' }) memoryState
                   , process=p'
                   }

    | Just mc@MC { cntx=rd@Cntx{..}, .. } <- find ((<< act) . head . actions) remains
    = case freeCell memoryState of
        Just (addr, cell) ->
          let (p', mc') = addMapToCell process mc addr
              (p'', mc'') = doAction p' mc' addr
          in dpu { memoryState=M.insert addr (cell { next=mc'' }) memoryState
                 , process=p''
                 , remains=filter (/= mc) remains
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
              m <- add (Interaction act) (Interval begin end)
              l1 <- add (Signal $ act2Signal addr act) (Interval begin end)
              ls <- if tick < begin
                then do
                  l2 <- add (Signal nop) (Interval tick (begin - 1))
                  -- relation $ Seq [l1, l2]
                  relation $ Vertical m l1
                  relation $ Vertical m l2
                  return [l1, l2]
                else do
                  relation $ Vertical m l1
                  return [l1]
              setTime (end + 1)
              return (m, ls)
            rd' = rd { middleLevel=m : middleLevel
                     , workBegin=workBegin `orElse` Just tick
                     , signalLevel=ls ++ signalLevel
                     }
        in (p', rd', if x == act then xs else (x \\ act) : xs)

      finish p Cntx{..} = snd $ modifyProcess p $ do
        h <- add (FunctionBlock fb) (Interval (fromMaybe undefined workBegin) end)
        mapM_ (relation . Vertical h) compilerLevel
        mapM_ (relation . Vertical h) middleLevel
        -- relation $ Seq compilerLevel
        relation $ Seq middleLevel
        relation $ Seq signalLevel

      act2Signal addr (Pull _) = Save addr
      act2Signal addr (Push _) = Load addr









instance (Enum addr, Num addr, Ord addr
         , Default time, Default key
         ) => Default (FRAM addr var key time) where
  def = FRAM { lastSave=Nothing
             , memoryState=fromList [ (addr, def) | addr <- [0..19] ]
             , remains=[]
             , process=def
             }



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
cell2acts Cell { queue=mcs } | length mcs > 0 = map (head . actions) mcs
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



