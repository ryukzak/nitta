{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Intermediate.Types
Description : Types for an algorithm intermediate representation
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Intermediate.Types
    ( -- *Function interface
      I(..), O(..), X(..)
      -- *Function description
    , F(..), castF, Function(..)
    , Lock(..), Locks(..), inputsLockOutputs
    , WithFunctions(..)
    , Label(..)
      -- *Functional simulation
    , FunctionSimulation(..)
    , CycleCntx(..), Cntx(..)
    , getX, setZipX, cntxReceivedBySlice
      -- *Patch
    , Patch(..), Changeset(..), reverseDiff
    , module NITTA.Intermediate.Value
    , module NITTA.Intermediate.Variable
    ) where

import           Data.Default
import           Data.List
import qualified Data.Map                    as M
import           Data.Maybe
import qualified Data.Set                    as S
import qualified Data.String.Utils           as S
import           Data.Tuple
import           Data.Typeable
import           GHC.Generics
import           NITTA.Intermediate.Value
import           NITTA.Intermediate.Variable


-- |Input variable.
newtype I v = I v
    deriving ( Show, Eq, Ord )

instance ( Eq v ) => Patch (I v) (v, v) where
    patch (v, v') i@(I v0)
        | v0 == v = I v'
        | otherwise = i

instance Variables (I v) v where
    variables (I v) = S.singleton v


-- |Output variable set.
newtype O v = O (S.Set v)
    deriving ( Eq, Ord )

instance ( Ord v ) => Patch (O v) (v, v) where
    patch (v, v') (O vs) = O $ S.fromList $ map (\e -> if e == v then v' else e) $ S.elems vs

instance ( Show v ) => Show (O v) where
    show (O vs) = "(O " ++ show (S.elems vs) ++ ")"

instance Variables (O v) v where
    variables (O v) = v


-- |Value of variable (constant or initial value).
newtype X x = X x
    deriving ( Show, Eq )


-----------------------------------------------------------

-- |Casuality of variable processing sequence in term of locks.
--
-- For example:
-- > c := a + b
-- > [ Lock{ locked=c, lockBy=a }, Lock{ locked=c, lockBy=b } ]
class ( Var v ) => Locks x v | x -> v where
    locks :: x -> [ Lock v ]


-- |Variable casuality.
data Lock v
    = Lock
        { locked :: v
        , lockBy :: v
        }
    deriving ( Show, Ord, Eq, Generic )


-- |All input variables locks all output variables.
inputsLockOutputs f =
    [ Lock{ locked=y, lockBy=x }
    | x <- S.elems $ inputs f
    , y <- S.elems $ outputs f
    ]


-----------------------------------------------------------

-- |Type class for application algorithm functions.
class Function f v | f -> v where
    -- |Get all input variables.
    inputs :: f -> S.Set v
    inputs _ = S.empty
    -- |Get all output variables.
    outputs :: f -> S.Set v
    outputs _ = S.empty
    -- |Sometimes, one function can cause internal process unit lock for another function.
    --
    -- TODO: remove or move, because its depends from PU type
    isInternalLockPossible :: f -> Bool
    isInternalLockPossible _ = False


-- |Type class for making fine label for Functions.
class Label a where
    label :: a -> String

instance Label String where
    label s = s


-- |Type class of something, which is related to functions.
class WithFunctions a f | a -> f where
    -- |Get a list of associated functions.
    functions :: a -> [f]


-- |Box forall functions.
data F v x where
    F ::
        ( Function f v
        , Patch f (v, v)
        , Locks f v
        , Show f
        , Label f
        , FunctionSimulation f v x
        , Typeable f
        ) => f -> F v x

instance Eq (F v x) where
    F a == F b = show a == show b

instance Function (F v x) v where
    isInternalLockPossible (F f) = isInternalLockPossible f
    inputs (F f) = inputs f
    outputs (F f) = outputs f

instance FunctionSimulation (F v x) v x where
    simulate cntx (F f) = simulate cntx f

instance Label (F v x) where
    label (F f) = S.replace "\"" "" $ label f

instance ( Var v ) => Locks (F v x) v where
    locks (F f) = locks f

instance Ord (F v x) where
    (F a) `compare` (F b) = show a `compare` show b

instance Patch (F v x) (v, v) where
    patch diff (F f) = F $ patch diff f

instance ( Ord v ) => Patch (F v x) (Changeset v) where
    patch Changeset{ changeI, changeO } f0 = let
            changeI' = mapMaybe (\v -> case changeI M.!? v of
                    Just v' -> Just (v, v')
                    Nothing -> Nothing
                ) $ S.elems $ inputs f0
            changeO' = concat $ mapMaybe (\v -> case changeO M.!? v of
                    Just vs -> Just [ (v, v') | v' <- S.elems vs ]
                    Nothing -> Nothing
                ) $ S.elems $ outputs f0
        in foldl (\f diff -> patch diff f) f0 $ changeI' ++ changeO'


instance ( Patch b v ) => Patch [b] v where
    patch diff fs = map (patch diff) fs

instance Show (F v x) where
    show (F f) = S.replace "\"" "" $ show f

instance ( Var v ) => Variables (F v x) v where
    variables (F f) = inputs f `S.union` outputs f


-- |Helper for extraction function from existential container 'F'.
castF :: ( Typeable f, Typeable v, Typeable x ) => F v x -> Maybe (f v x)
castF (F f) = cast f


-----------------------------------------------------------

-- |The type class for function simulation.
class FunctionSimulation f v x | f -> v x where
    -- FIXME: CycleCntx - problem, because its prevent Receive simulation with
    -- data drop (how implement that?).
    simulate :: CycleCntx v x -> f -> Either String (CycleCntx v x)


data CycleCntx v x = CycleCntx{ cycleCntx :: M.Map v x }
    deriving ( Show, Generic )

instance Default (CycleCntx v x) where
    def = CycleCntx def


data Cntx v x
    = Cntx
        { cntxProcess     :: [ CycleCntx v x ]
          -- ^all variables on each process cycle
        , cntxReceived    :: M.Map v [x]
          -- ^sequences of all received values, one value per process cycle
        , cntxCycleNumber :: Int
        }
instance {-# OVERLAPS #-} ( Show v, Show x ) => Show (Cntx v x) where
    show Cntx{ cntxProcess, cntxCycleNumber } = let
            header = S.join "\t" $ sort $ map show $ M.keys $ cycleCntx $ head cntxProcess
            body = map (row . cycleCntx) $ take cntxCycleNumber cntxProcess
        in S.join "\n" (header : body)
        where
            row cntx = S.join "\t" $  map (show . snd) $ sortOn (show . fst) $ M.assocs cntx

instance Default (Cntx v x) where
    def = Cntx
        { cntxProcess=def
        , cntxReceived=def
        , cntxCycleNumber=5
        }


-- |Make sequence of received values '[ Map v x ]'
cntxReceivedBySlice :: ( Ord v ) => Cntx v x -> [ M.Map v x ]
cntxReceivedBySlice Cntx{ cntxReceived } = cntxReceivedBySlice' $ M.assocs cntxReceived
cntxReceivedBySlice' received
    | all (not . null . snd) received
    = let
        slice = M.fromList [ (v, x) | ( v, x:_ ) <- received ]
        received' = [ (v, xs) | ( v, _:xs ) <- received ]
    in slice : cntxReceivedBySlice' received'
    | otherwise = repeat M.empty

getX (CycleCntx cntx) v = case cntx M.!? v of
        Just x  -> Right x
        Nothing -> Left $ "variable value not defined: " ++ show v

setX cycleCntx vxs = setX' cycleCntx vxs
setZipX cycleCntx vs x = setX cycleCntx $ zip (S.elems vs) $ repeat x

setX' cycleCntx [] = Right cycleCntx
setX' (CycleCntx cntx) ((v, x):vxs)
    | M.member v cntx = Left $ "variable value already defined: " ++ show v
    | otherwise = setX' (CycleCntx $ M.insert v x cntx) vxs


-----------------------------------------------------------

-- |Patch class allows replacing one variable by another. Especially for algorithm refactor.
class Patch f diff where
    patch :: diff -> f -> f


-- |Change set for patch.
--
-- FIXME: rename to change set
data Changeset v = Changeset
        { changeI :: M.Map v v
          -- ^change set for input variables (one to one)
        , changeO :: M.Map v (S.Set v)
          -- ^change set for output variables. Many to many relations:
          --
          -- > fromList [(a, {x}), (b, {x})] -- several output variables to one
          -- > fromList [(c, {y, z})] -- one output variable to many
        }
    deriving ( Show, Eq )

instance Default (Changeset v) where
    def = Changeset def def


-- |Reverse changeset for patch a process unit options / decision.

-- TODO: move to another module
reverseDiff Changeset{ changeI, changeO } = Changeset
    { changeI=M.fromList $ map swap $ M.assocs changeI
    , changeO=foldl (\st (k, v) -> let
                          box' = case st M.!? k of
                              Just box -> box `S.union` S.singleton v
                              Nothing  -> S.singleton v
                      in M.insert k box' st
                  ) def
            [ ( b, a )
            | ( a, bs ) <- M.assocs changeO
            , b <- S.elems bs
            ]
    }
