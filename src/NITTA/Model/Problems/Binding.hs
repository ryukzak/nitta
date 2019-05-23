{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Model.Problems.Endpoint
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Problems.Binding
    ( BindingDT, Option(..), Decision(..), binding
    ) where

import           Data.Proxy
import           GHC.Generics
import           NITTA.Intermediate.Types
import           NITTA.Model.Problems.Types

-- |Решение в области привязки функционального блока к вычислительному. Определяется только для
-- вычислительных блоков, организующих работу со множеством вложенных блоков, адресуемым по tag.
data BindingDT tag v x
binding = Proxy :: Proxy BindingDT

instance DecisionType (BindingDT tag v x) where
    data Option (BindingDT tag v x) = BindingO (F v x) tag deriving ( Generic )
    data Decision (BindingDT tag v x) = BindingD (F v x) tag deriving ( Generic )
