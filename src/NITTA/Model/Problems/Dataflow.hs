{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Model.Problems.Dataflow
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Problems.Dataflow
    ( DataflowOption(..), DataflowDecision(..), DataflowProblem(..)
    , Source, Target
    ) where

import qualified Data.Map          as M
import           GHC.Generics
import           NITTA.Model.Types
import           Numeric.Interval


type Source tag t = (tag, t)
type Target tag v t = M.Map v (Maybe (tag, t))

data DataflowOption tag v t
    = DataFlowO
        { dfoSource  :: Source tag (TimeConstrain t) -- ^Источник пересылки.
        -- |Словарь, описывающий все необходимые пункты назначения для пересылаемого значения.
        -- Допустима ситация, когда пункт назначения не может принять значение, в таком случае для
        -- негоне указываются временные ограничения.
        --
        -- Примечание: почему tag оказался под Maybe? Потому что мы можем, банально, не знать в
        -- каком PU находится требуемый функциональный блок, так как он может быть ещё непривязан к
        -- PU.
        , dfoTargets :: Target tag v (TimeConstrain t)
        }
        deriving ( Show, Generic )

data DataflowDecision tag v t
    = DataFlowD
        { dfdSource  :: Source tag (Interval t) -- ^Источник пересылки.
        -- |Словарь, описывающий пункты назначения для пересылаемого значения.
        , dfdTargets :: Target tag v (Interval t)
        }
        deriving ( Show, Generic )

class DataflowProblem u tag v t | u -> tag v t where
    dataflowOptions :: u -> [ DataflowOption tag v t ]
    dataflowDecision :: u -> DataflowDecision tag v t -> u
