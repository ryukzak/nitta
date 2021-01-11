{-# LANGUAGE FunctionalDependencies #-}

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

-- |Type class of view helper
class Viewable t v | t -> v where
    view :: t -> v
