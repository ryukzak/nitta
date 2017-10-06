{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
-- {-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module NITTA.ProcessUnits.Accum where


import           Control.Lens         hiding (at, from, (...))
import           Control.Monad.State
import           Data.Default
import           Data.Either
import           Data.List            (find, intersect, (\\))
import qualified Data.Map             as M
import           Data.Maybe
import           Data.Typeable
import           NITTA.FunctionBlocks
import           NITTA.Lens
import           NITTA.TestBench
import           NITTA.Types
import           NITTA.Utils
import           Numeric.Interval     (Interval, inf, singleton, sup, width,
                                       (...))

import           Debug.Trace


-- Wrapper -------------------------------------------------------------------

data SerialPU st io v t
  = SerialPU
  { spuRemain  :: [(FB io v, ProcessUid)]
  , spuCurrent :: Maybe (CurrentJob io v t)
  , spuProcess :: Process v t
  , spuState   :: st
  } deriving ( Show )

instance ( Time t, Var v, Default st ) => Default (SerialPU st Parcel v t) where
  def = SerialPU
    { spuRemain  = def
    , spuCurrent = def
    , spuProcess = def
    , spuState   = def
    }

data CurrentJob io v t
  = CurrentJob
  { cFB    :: FB io v
  , cStart :: t
  , cSteps :: [ProcessUid]
  } deriving ( Show )

class ( Typeable st
      , Default st
      , Var v, Time t
      ) => SerialPUState st io v t | st -> io, st -> v, st -> t where
  bindToState :: FB io v -> st -> Either String st
  stateOptions :: st -> t -> [Option Passive v t]
  schedule :: st -> Action Passive v t -> (st, State (Process v t) [ProcessUid])



instance ( SerialPUState st Parcel v t, Show st
         ) => PUClass Passive (SerialPU st Parcel v t) v t where

  bind fb pu@SerialPU{..}
    -- Используется def, так как выбор функции выполняется на уровне SerialPU, а не SerialPUState.
    = case fb `bindToState` (def :: st) of
        Right _ -> let (key, spuProcess') = modifyProcess spuProcess $ bindFB fb (spuProcess^.time)
                   in Right pu{ spuRemain=(fb, key) : spuRemain
                              , spuProcess=spuProcess'
                              }
        Left reason -> Left reason

  options SerialPU{ spuCurrent=Nothing, .. }
    = concatMap ((\f -> f $ spuProcess^.time) . stateOptions)
      $ rights $ map (\(fb, _) -> bindToState fb spuState) spuRemain
  options SerialPU{ spuCurrent=Just _, .. }
    = stateOptions spuState $ spuProcess^.time

  select pu@SerialPU{ spuCurrent=Nothing, .. } act
    | Just (fb, compilerKey) <- find (not . null . (variables act `intersect`) . variables . fst) spuRemain
    , Right spuState' <- bindToState fb spuState
    = select pu{ spuState=spuState'
               , spuCurrent=Just CurrentJob
                             { cFB=fb
                             , cStart=inf $ eaAt act
                             , cSteps=[ compilerKey ]
                             }
              } act
  select pu@SerialPU{ spuCurrent=Just cur, .. } act
   | not $ nextTick spuProcess <= act^.start
   = error $ "Time wrap! Time: " ++ show (nextTick spuProcess) ++ " Act start at: " ++ show (act ^. start)
   | otherwise
    = let (spuState', work) = schedule spuState act
          (steps, spuProcess') = modifyProcess spuProcess work
          cur' = cur{ cSteps=steps ++ cSteps cur }
          pu' = pu{ spuState=spuState'
                  , spuProcess=spuProcess'
                  , spuCurrent=Just cur
                  }
          nextOptions = stateOptions spuState' (spuProcess' ^. time)
      in -- trace ("SerialPU:" ++ show act ++ " " ++ (show $ tick spuProcess)) $
         case nextOptions of
           [] -> pu'{ spuCurrent=Nothing
                    , spuProcess=finish spuProcess' cur'
                    }
           _  -> pu'
    where
      finish p cur@CurrentJob{..} = snd $ modifyProcess p $ do
        h <- add (Activity $ cStart ... (act^.at.leftBound + act^.at.dur)) $ FBStep cFB
        mapM_ (relation . Vertical h) cSteps

  process = spuProcess
  setTime t pu@SerialPU{..} = pu{ spuProcess=spuProcess{ nextTick=t } }


instance ( Var v, Time t
         , ByInstruction (SerialPU st Parcel v t)
         , Default (Instruction (SerialPU st Parcel v t))
         , Controllable (SerialPU st Parcel v t)
         ) => ByTime (SerialPU st Parcel v t) t where
  signalAt pu@SerialPU{..} sig t
    = let instr = case instructionAt (proxy pu) t spuProcess of
                    Just i  -> i
                    Nothing -> def
      in signalFor instr sig



-- Add -----------------------------------------------------------------------

data AccumState v t = Accum{ acIn :: [v], acOut :: [v] }
  deriving ( Show )

instance Default (AccumState v t) where
  def = Accum def def

type Accum v t = SerialPU (AccumState v t) Parcel v t


instance ( Var v, Time t
         , Controllable (Accum v t)
         ) => SerialPUState (AccumState v t) Parcel v t where
  bindToState fb ac@Accum{ acIn=[], acOut=[] }
    | Just (Add (I a) (I b) (O cs)) <- castFB fb = Right ac{ acIn=[a, b], acOut = cs }
    | otherwise = Left $ "Unknown functional block or : " ++ show fb

  stateOptions Accum{ acIn=vs@(_:_) } now
    | length vs == 2
    = map (\v -> EffectOpt (Push v) $ TimeConstrain (now ... maxBound) (singleton 2)) vs
    | otherwise
    = map (\v -> EffectOpt (Push v) $ TimeConstrain (now ... maxBound) (singleton 1)) vs
  stateOptions Accum{ acOut=vs@(_:_) } now
    = [ EffectOpt (Pull vs) $ TimeConstrain ((now + 1) ... maxBound) (1 ... maxBound) ]
  stateOptions _ _ = []

  schedule st@Accum{ acIn=vs@(_:_) } act
    | not $ null $ vs `intersect` variables act
    = let st' = st{ acIn=vs \\ variables act }
          work = serialSchedule (Proxy :: Proxy (Accum v t)) act
            $ if length vs == 2
              then Init False
              else Load False
      in (st', work)
  schedule st@Accum{ acIn=[], acOut=vs } act
    | not $ null $ vs `intersect` variables act
    = let st' = st{ acOut=vs \\ variables act }
          work = serialSchedule (Proxy :: Proxy (Accum v t)) act Out
      in (st', work)




instance ( Var v, Time t ) => Controllable (Accum v t) where
  data Signals (Accum v t) = OE | INIT | LOAD | NEG deriving ( Show, Eq, Ord )
  data Instruction (Accum v t)
    = Nop
    | Init Bool
    | Load Bool
    | Out
    deriving (Show)

instance ( Controllable (Accum v t) ) => Default (Instruction (Accum v t)) where
  def = Nop

instance ( Var v, Time t ) => ByInstruction (Accum v t) where
  signalFor  Nop     NEG  = X
  signalFor  Nop     _    = B False

  signalFor (Init _) INIT = B True
  signalFor (Init _) LOAD = B True
  signalFor (Init _) OE   = B False
  signalFor (Init n) NEG  = B n

  signalFor (Load _) INIT = B False
  signalFor (Load _) LOAD = B True
  signalFor (Load _) OE   = B False
  signalFor (Load n) NEG  = B n


  signalFor  Out     INIT = B False
  signalFor  Out     LOAD = B False
  signalFor  Out     OE   = B True
  signalFor  Out     NEG  = X

instance ( PUClass Passive (Accum v t) v t
         , Time t
         , Var v
         ) => Simulatable (Accum v t) v Int where

  variableValue (FB fb) pu@SerialPU{..} cntx (v, i)
    | Just (Add (I a) _ _) <- cast fb, a == v = cntx M.! (v, i)
    | Just (Add _ (I b) _) <- cast fb, b == v = cntx M.! (v, i)
    | Just (Add (I a) (I b) (O cs)) <- cast fb, v `elem` cs = (cntx M.! (a, i)) + (cntx M.! (b, i))
    | otherwise = error $ "Can't simulate " ++ show fb


-- Internals ----------------------------------------------------------

serialSchedule puProxy act instr = do
  now <- processTime
  e <- add (Activity $ eaAt act) $ EffectStep (eaEffect act)
  i <- modelInstruction puProxy (eaAt act) instr
  is <- if False && now < act^.start
        then do
            ni <- modelInstruction puProxy (now ... (act^.start)) def
            return [i, ni]
        else return [i]
  mapM_ (relation . Vertical e) is
  setProcessTime $ (sup $ eaAt act) + 1
  return $ e : is

modelInstruction
  :: ( Show (Instruction pu), Typeable pu, Time t
     ) => Proxy pu -> Interval t -> Instruction pu -> State (Process v t) ProcessUid
modelInstruction _pu at instr = add (Activity at) $ InstructionStep instr
