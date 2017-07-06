{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE UndecidableInstances      #-}

module NITTA.ProcessUnits where

import qualified Data.List            as L
import           NITTA.Base
import           NITTA.FunctionBlocks




data Interaction mt v t = Interaction { effect :: Effect v
                                      , at     :: mt t
                                      } deriving (Show)
type PuVariant = Interaction TimeConstrain
type PuAction = Interaction Event




data Effect v
  = Push v
  | Pull [v]
  deriving (Show, Eq)

(Push a) << (Push b) | a == b = True
(Pull a) << (Pull b)          = all (`elem` a) b
_        << _                 = False

(Pull a) \\ (Pull b) = Pull (a L.\\ b)

instance Vars (Effect v) v where
  variables (Push var)  = [var]
  variables (Pull vars) = vars




data PuInfo v where
  Compiler :: String -> PuInfo v
  FunctionBlock :: FB v -> PuInfo v
  Effect :: Effect v -> PuInfo v
  Middle :: (Show mid) => mid -> PuInfo v
  Signal :: (Show sig) => sig -> PuInfo v
type PuStep = Step PuInfo

instance Level (PuInfo v) where
  level (Compiler s)       = "Compiler"
  level (FunctionBlock fb) = "FunctionBlock"
  level (Effect i)         = "Effect"
  level (Middle m)         = "Middle"
  level (Signal sig)       = "Signal"

instance (Var v) => Show (PuInfo v) where
  show (Compiler s)       = s
  show (FunctionBlock fb) = show fb
  show (Effect i)         = show i
  show (Middle m)         = show m
  show (Signal sig)       = "signal: " ++ show sig


signalAt t Process{..} = case L.find (\Step{ time=Event{..}, info=Signal _ } ->
                                    eStart <= t && t <= eStart + eDuration
                                 ) steps of
                       Just Step{ time=Event{..}, info=Signal a } -> show a
                       Nothing -> error "Time not come... We need nop value here..."
