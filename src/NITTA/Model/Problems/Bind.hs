{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}

{- |
Module      : NITTA.Model.Problems.Bind
Description : Function distribution between processor units
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Problems.Bind (
    Bind (..),
    BindProblem (..),
    binds2bindGroup,
) where

import Data.Map.Strict qualified as M
import Data.String.ToString
import Data.String.Utils qualified as S
import GHC.Generics
import NITTA.Intermediate.Types
import NITTA.Model.ProcessorUnits.Types (UnitTag)
import NITTA.Utils.Base (unionsMap)

data Bind tag v x
    = SingleBind tag (F v x)
    | GroupBind {isObliviousBinds :: Bool, bindGroup :: M.Map tag [F v x]}
    deriving (Generic, Eq)

binds2bindGroup :: UnitTag tag => [(tag, F v x)] -> M.Map tag [F v x]
binds2bindGroup binds =
    foldl
        ( \st (tag, f) ->
            M.alter
                ( \case
                    (Just fs) -> Just $ f : fs
                    Nothing -> Just [f]
                )
                tag
                st
        )
        M.empty
        binds

instance UnitTag tag => Show (Bind tag v x) where
    show (SingleBind uTag f) = "Bind " <> showFAndTag (f, uTag)
    show (GroupBind{isObliviousBinds, bindGroup}) =
        concat
            [ "Binds "
            , if isObliviousBinds then "obliviousBinds " else ""
            , S.join "; " (map showFsAndTag $ M.assocs bindGroup)
            ]

showFAndTag :: UnitTag tag => (F v x, tag) -> String
showFAndTag (f, tag) = toString tag <> " <- " <> show f

showFsAndTag :: (ToString a1, Show a2) => (a1, [a2]) -> String
showFsAndTag (tag, fs) = toString tag <> " <- " <> S.join ", " (map show fs)

class BindProblem u tag v x | u -> tag v x where
    bindOptions :: u -> [Bind tag v x]
    bindDecision :: u -> Bind tag v x -> u

instance Var v => Variables (Bind tab v x) v where
    variables (SingleBind _tag f) = variables f
    variables GroupBind{bindGroup} = unionsMap variables $ concat $ M.elems bindGroup

instance WithFunctions (Bind tag v x) (F v x) where
    functions (SingleBind _tag f) = [f]
    functions GroupBind{bindGroup} = concat $ M.elems bindGroup
