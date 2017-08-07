{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module NITTA.Utils where

import           Control.Monad.State
import           Data.Default
import           Data.Functor.Const
import           Data.List            (find, intersect, isSubsequenceOf,
                                       partition, sortBy)
import qualified Data.List            as L
import qualified Data.Map             as M
import           Data.Maybe           (catMaybes, fromMaybe, isJust)
import           Data.Proxy
import           Data.Typeable        (Typeable, cast, typeOf)
import           NITTA.FunctionBlocks
import qualified NITTA.FunctionBlocks as FB
import           NITTA.Types
import           System.Exit
import           System.Process


isPull (EffectOpt (Pull _) _) = True
isPull _                      = False
isPush (EffectOpt (Push _) _) = True
isPush _                      = False




modifyProcess p state = runState state p

add time info = do
  p@Process{..} <- get
  put p { nextUid=succ nextUid
        , steps=Step nextUid time info : steps
        }
  return nextUid

relation r = do
  p@Process{..} <- get
  put p{ relations=r : relations }

setProcessTime t = do
  p <- get
  put p{ tick=t }

bindFB fb t = add (Event t 0) $ InfoStep $ "Bind " ++ show fb



stepStart Step{ time=Event{..} } = eStart


whatsHappen t Process{..} =
  filter (\Step{ time=Event{..} } -> eStart <= t && t < eStart + eDuration) steps


isFB (FBStep _)                = True
isFB (NestedStep _ (FBStep _)) = True
isFB _                         = False

getFB Step{ info=FBStep fb }                = Just fb
getFB Step{ info=NestedStep _ (FBStep fb) } = Just fb
getFB _                                     = Nothing

getFBs p = catMaybes $ map getFB
           $ sortBy (\a b -> stepStart a `compare` stepStart b)
           $ steps p


isEffect (EffectStep _) = True
isEffect _              = False

getEffect Step{ info=EffectStep eff }                = Just eff
getEffect Step{ info=NestedStep _ (EffectStep eff) } = Just eff
getEffect _                                          = Nothing

getEffects p = catMaybes $ map getEffect
               $ sortBy (\a b -> stepStart a `compare` stepStart b)
               $ steps p

effectAt t p
  | [eff] <- catMaybes $ map getEffect $ whatsHappen t p = Just eff
  |otherwise = Nothing


isInfo (InfoStep _) = True
isInfo _            = False


isInstruction (InstructionStep _) = True
isInstruction _                   = False

getInstruction :: ( Typeable pu ) => Proxy pu -> Step v t -> Maybe (Instruction pu)
getInstruction _ Step{ info=InstructionStep instr } = cast instr
getInstruction _ _                                  = Nothing

getInstructions proxy' p = catMaybes $ map (getInstruction $ proxy')
                           $ sortBy (\a b -> stepStart a `compare` stepStart b)
                           $ steps p



inputsOfFBs fbs
  = let deps0 = (M.fromList [(v, []) | v <- concatMap variables fbs])
        deps = foldl (\dict (a, b) -> M.adjust ((:) b) a dict) deps0 $ concatMap dependency fbs
    in map fst $ filter (null . snd) $ M.assocs deps

-- outputsOfFBs fbs
--   = let deps0 = (M.fromList [(v, []) | v <- concatMap variables fbs])
--         deps = foldl (\dict (a, b) -> M.adjust ((:) b) a dict) deps0 $ concatMap dependency fbs
--     in filter (\a -> all (not . (a `elem`)) deps) $ M.keys deps


fromLeft :: a -> Either a b -> a
fromLeft _ (Left a) = a
fromLeft a _        = a

fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _         = b
