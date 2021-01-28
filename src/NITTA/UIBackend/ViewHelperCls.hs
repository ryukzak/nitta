{- |
Module      : NITTA.UIBackend.ViewHelperCls
Description : Type class for marshaling helpers
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.UIBackend.ViewHelperCls (
    Viewable (..),
) where

{- |Type class of helpers required for autogeneration of ToJSON and typescript
 types.
-}
class Viewable t v | t -> v where
    view :: t -> v
