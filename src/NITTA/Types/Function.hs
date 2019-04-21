{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Types.Function
Description : Types and instances, related to function description
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Types.Function
    ( -- *Variables
      Var, Variables(..)
      -- *Function interface description
    , I(..), O(..), X(..)
    , Lock(..), Locks(..)
      -- *Function description
    , F(..), Function(..)
      -- *Application level simulation
    , FunctionSimulation(..), Cntx(..)
      -- *Other
    , Patch(..)
    , Label(..)
    ) where

import           Data.Default
import           Data.List
import qualified Data.Map          as M
import qualified Data.Set          as S
import qualified Data.String.Utils as S
import           Data.Typeable
import           NITTA.Types.VisJS



-----------------------------------------------------------

-- |Variable identifier. Used for simplify type description.
type Var v = ( Typeable v, Ord v, Show v, Label v )

-- |Type class of something, which is related to varibles.
class Variables a v | a -> v where
    -- |Get all related variables.
    variables :: a -> S.Set v



-----------------------------------------------------------

-- |Input variable.
newtype I v = I v
    deriving ( Show, Eq, Ord )

instance ( Eq v ) => Patch (I v) (v, v) where
    patch (v, v') i@(I v0)
        | v0 == v = I v'
        | otherwise = i

instance Variables (I v) v where
    variables (I v) = S.singleton v



-- |Output variables.
newtype O v = O (S.Set v)
    deriving ( Eq, Ord )

instance ( Ord v ) => Patch (O v) (v, v) where
    patch (v, v') o@(O vs)
        | v `S.member` vs = O $ S.fromList (v':(S.elems vs \\ [v]))
        | otherwise = o

instance ( Show v ) => Show (O v) where
    show (O vs) = "O " ++ show (S.elems vs)

instance Variables (O v) v where
    variables (O v) = v



-- |Value of variable (constant or initial value).
newtype X x = X x
    deriving ( Show, Eq )



-- |The type class for a thing, which can defines order of variable transfers.
class Locks x v | x -> v where
    locks :: x -> [Lock v]

-- |Variable casuality.
data Lock v
    = Lock
        { locked :: v
        , lockBy :: v
        }
    deriving ( Show )



-----------------------------------------------------------

-- |Type class for application algorithm functions.
class Function f v | f -> v where
    -- |Get all input variables.
    inputs :: f -> S.Set v
    inputs _ = S.empty
    -- |Get all output variables.
    outputs :: f -> S.Set v
    outputs _ = S.empty
    -- |Is function break eval loop.
    isBreakLoop :: f -> Bool
    isBreakLoop _ = False
    -- |Sometimes, one function can cause internal process unit lock for another function.    
    isInternalLockPossible :: f -> Bool
    isInternalLockPossible _ = False



-- |Box forall functions.
data F v x where
    F ::
        ( Ord v
        , Function f v
        , Patch f (v, v)
        , Locks f v
        , Show f
        , Label f
        , ToVizJS f
        , FunctionSimulation f v x
        , Typeable f, Typeable v, Typeable x
        ) => f -> F v x

instance Eq (F v x) where
    F a == F b = show a == show b

instance Function (F v x) v where
    isBreakLoop (F f) = isBreakLoop f
    isInternalLockPossible (F f) = isInternalLockPossible f
    inputs (F f) = inputs f
    outputs (F f) = outputs f

instance FunctionSimulation (F v x) v x where
    simulate cntx (F f) = simulate cntx f

instance Label (F v x) where
    label (F f) = label f

instance Locks (F v x) v where
    locks (F f) = locks f

instance Ord (F v x) where
    (F a) `compare` (F b) = show a `compare` show b

instance Patch (F v x) (v, v) where
    patch diff (F f) = F $ patch diff f

instance ( Patch b v ) => Patch [b] v where
    patch diff fs = map (patch diff) fs

instance Show (F v x) where
    show (F f) = S.replace "\"" "" $ show f

instance Variables (F v x) v where
    variables (F f) = inputs f `S.union` outputs f



-----------------------------------------------------------
-- FIXME: Think about: implement algSimulation here..

-- |The type class for function simulation.
class FunctionSimulation f v x | f -> v x where
    simulate :: Cntx v x -> f -> Maybe (Cntx v x)


data Cntx v x
    = Cntx
        { cntxVars    :: M.Map v [x]
        , cntxInputs  :: M.Map v [x]
        , cntxOutputs :: M.Map v [x]
        , cntxFram    :: M.Map (Int, v) [x]
        }

instance Default (Cntx v x) where
    def = Cntx M.empty M.empty M.empty M.empty

-- FIXME: Incorrect output if cntxInput has different amount of data.
instance ( Show v, Show x ) => Show (Cntx v x) where
    show Cntx{ cntxVars, cntxInputs, cntxOutputs }
        = let
            dt = concat
                [ map (\(v, xs) -> reverse $ map ( filter (/= '"') . (("q." ++ show v ++ ":") ++) . show ) xs) $ M.assocs cntxInputs
                , map (\(v, xs) -> reverse $ map ( filter (/= '"') . ((show v ++ ":") ++) . show ) xs) $ M.assocs cntxOutputs
                , map (\(v, xs) -> reverse $ map ( filter (/= '"') . ((show v ++ ":") ++) . show ) xs) $ M.assocs cntxVars
                ]
        in S.join "\n" $ map (S.join "\t") $ transpose dt



-----------------------------------------------------------

-- |Patch class allows replacing one variable by another. Especially for algorithm refactor.
class Patch f diff where
    patch :: diff -> f -> f


-- |Type class for making fine label for Functions (firtly for VisJS).
class Label a where
    label :: a -> String

instance ( Show (f v x) ) => Label (f v x) where
    label f = S.replace "\"" "" $ show f

instance Label String where
    label s = s
