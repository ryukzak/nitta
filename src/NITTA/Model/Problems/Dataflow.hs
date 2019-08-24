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
    ) where

import qualified Data.Map          as M
import           GHC.Generics
import           NITTA.Model.Types
import           Numeric.Interval


data DataflowOption tag v t
    = DataFlowO
        { -- |A source processor unit of data flow transaction, and it's time constrains.
          dfoSource  :: (tag, TimeConstrain t)
        -- |All possible targets of dataflow transaction. If some of targets can
        -- be not available (Nothing).
        , dfoTargets :: M.Map v (Maybe (tag, TimeConstrain t)) -- Target tag v (TimeConstrain t)
        }
        deriving ( Show, Generic )

data DataflowDecision tag v t
    = DataFlowD
        { dfdSource  :: (tag, Interval t) -- ^Источник пересылки.
        -- |Словарь, описывающий пункты назначения для пересылаемого значения.
        , dfdTargets :: M.Map v (Maybe (tag, Interval t))
        }
        deriving ( Show, Generic )

class DataflowProblem u tag v t | u -> tag v t where
    dataflowOptions :: u -> [ DataflowOption tag v t ]
    dataflowDecision :: u -> DataflowDecision tag v t -> u
