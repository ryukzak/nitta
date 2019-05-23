{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Model.Problems.Transport
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Problems.Transport
    ( DataFlowDT, Option(..), Decision(..), dataFlowDT
    , Source, Target
    ) where

import qualified Data.Map                   as M
import           Data.Proxy
import           GHC.Generics
import           NITTA.Model.Problems.Types
import           NITTA.Model.Types
import           Numeric.Interval



-- |Решения относительно пересылки данных между вычислитльными узлами (реализация прикладного
-- Model).
data DataFlowDT tag v t
dataFlowDT = Proxy :: Proxy DataFlowDT

type Source tag t = (tag, t)
type Target tag v t = M.Map v (Maybe (tag, t))

instance DecisionType (DataFlowDT tag v t) where
    data Option (DataFlowDT tag v t)
        = DataFlowO
        { dfoSource     :: Source tag (TimeConstrain t) -- ^Источник пересылки.
        -- |Словарь, описывающий все необходимые пункты назначения для пересылаемого значения.
        -- Допустима ситация, когда пункт назначения не может принять значение, в таком случае для
        -- негоне указываются временные ограничения.
        --
        -- Примечание: почему tag оказался под Maybe? Потому что мы можем, банально, не знать в
        -- каком PU находится требуемый функциональный блок, так как он может быть ещё непривязан к
        -- PU.
        , dfoTargets    :: Target tag v (TimeConstrain t)
        } deriving ( Show, Generic )
    data Decision (DataFlowDT tag v t)
        = DataFlowD
        { dfdSource     :: Source tag (Interval t) -- ^Источник пересылки.
        -- |Словарь, описывающий пункты назначения для пересылаемого значения.
        , dfdTargets    :: Target tag v (Interval t)
        } deriving ( Show, Generic )
