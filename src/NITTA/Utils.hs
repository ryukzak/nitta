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
                                       partition)
import qualified Data.List            as L
import qualified Data.Map             as M
import           Data.Maybe           (catMaybes, fromMaybe, isJust)
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

bindFB fb t = add (Event t 0) $ Const $ "Bind " ++ show fb



whatsHappen t Process{..} =
  filter (\Step{ time=Event{..} } -> eStart <= t && t < eStart + eDuration) steps

infoAt :: ( Time t
          , Typeable (info v)
          ) => t -> Process v t -> [info v]
infoAt t p = catMaybes $ map (\Step{..} -> cast info) $ whatsHappen t p

filterSteps :: ( Typeable (info v)
               ) => Process v t -> [(Step v t, info v)]
filterSteps = catMaybes . map (\step@Step{..} -> fmap (step, ) $ cast info) . steps
findStep v =
  find (\(_, info) -> v `elem` variables info) . filterSteps


stepStart Step{ time=Event{..} } = eStart



fromLeft :: a -> Either a b -> a
fromLeft _ (Left a) = a
fromLeft a _        = a
fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _         = b
