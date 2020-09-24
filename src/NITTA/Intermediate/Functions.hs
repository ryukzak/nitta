{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : NITTA.Intermediate.Functions
Description : Library of functions
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

Library of functions for an intermediate algorithm representation. Execution
relations between functions and process units are many-to-many.

[@function (functional block)@] atomic operation in intermediate algorithm
representation. Function has zero or many inputs and zero or many output.
Function can contains state between process cycles.
-}
module NITTA.Intermediate.Functions
    ( -- *Arithmetics
      Add(..), add
    , Division(..), division
    , Multiply(..), multiply
    , ShiftLR(..), shiftL, shiftR
    , Sub(..), sub
    -- *Memory
    , Constant(..), constant
    , Loop(..), loop, isLoop, LoopIn(..), LoopOut(..)
    , Reg(..), reg
    -- *Input/Output
    , Receive(..), receive
    , Send(..), send
    , module NITTA.Intermediate.Functions.Accum
    ) where

import qualified Data.Bits as B
import           Data.Default
import qualified Data.Map as M
import           Data.Set ( elems, fromList, union )
import qualified Data.String.Utils as S
import           Data.Typeable
import           NITTA.Intermediate.Functions.Accum
import           NITTA.Intermediate.Types
import           NITTA.Utils.Base

-- |Loop -- function for transfer data between computational cycles.
-- Let see the simple example with the following implementation of the
-- Fibonacci algorithm.
--
-- Data flow graph:
--
-- @
--     +---------------------------------+
--     |                                 |
--     v                                 |
-- +------+                          b2  |
-- | Loop |      b1_1  +-----+    +------+
-- +------+----+------>|     |    |
--             | a1    | sum +----+
-- +------+----------->|     |
-- | Loop |    |       +-----+      b1_2
-- +------+    +-------------------------+
--     ^                                 |
--     |                                 |
--     +---------------------------------+
-- @
--
-- Lua source code:
--
-- @
-- function fib(a1, b1)
--     b2 = a1 + b1
--     fib(b1, b2)
-- end
-- fib(0, 1)
-- @
--
-- Data flow defines computation for a single computational cycle. But
-- a controller should repeat the algorithm infinite times, and
-- usually, it is required to transfer data between cycles. `Loop`
-- allows doing that. At first cycle, `Loop` function produces an
-- initial value (`X x`), after that on each cycle `Loop` produces a
-- variable value from the previous cycle, and consumes a new value at
-- the end of the cycle.
--
-- Computational process:
--
-- @
--          ][                 Cycle 1                 ][                Cycle 2                  ]
--          ][                                         ][                                         ]
-- initial  ][ ---+                          b2   +--- ][ ---+                          b2   +--- ]
--  value   ][ op |      b1_1  +-----+    +------>| Lo ][ op |      b1_1  +-----+    +------>| Lo ]
--  is a    ][ ---+----+------>|     |    |       +--- ][ ---+----+------>|     |    |       +--- ]
-- part of  ][         |       | sum +----+            ][         |       | sum +----+            ]
-- software ][ ---+----------->|     |            +--- ][ ---+----------->|     |            +--- ]
--          ][ op |    |       +-----+     b1_2   | Lo ][ op |    |       +-----+      b1_2  | Lo ]
--          ][ ---+    +------------------------->+--- ][ ---+    +------------------------->+--- ]
--          ][                                         ][                                         ]
-- @
--
-- Similation data:
--
-- +--------------+----+----+----+
-- | Cycle number | a1 | b1 | b2 |
-- +==============+====+====+====+
-- | 1            | 0  | 1  | 1  |
-- +--------------+----+----+----+
-- | 2            | 1  | 1  | 2  |
-- +--------------+----+----+----+
-- | 3            | 1  | 2  | 3  |
-- +--------------+----+----+----+
-- | 4            | 2  | 3  | 5  |
-- +--------------+----+----+----+
--
-- In practice, Loop function supported by Fram processor unit in the
-- following way: Loop function should be prepared before execution by
-- automatical refactor @BreakLoop@, which replace Loop by @LoopIn@
-- and @LoopOut@.



data Loop v x = Loop (X x) (O v) (I v) deriving ( Typeable, Eq, Show )
instance ( Show x, Show v ) => Label (Loop v x) where
    label (Loop (X x) _ (I b)) = show x ++ "->" ++ show b
loop :: ( Var v, Val x ) => x -> v -> [v] -> F v x
loop x a bs = packF $ Loop (X x) (O $ fromList bs) $ I a
isLoop f
    | Just Loop{} <- castF f = True
    | otherwise = False

instance Function (Loop v x) v where
    isInternalLockPossible _ = True
    inputs  (Loop _ _a b) = variables b
    outputs (Loop _ a _b) = variables a
instance ( Ord v ) => Patch (Loop v x) (v, v) where
    patch diff (Loop x a b) = Loop x (patch diff a) (patch diff b)
instance ( Var v ) => Locks (Loop v x) v where
    locks (Loop _ (O as) (I b)) = [ Lock{ locked=b, lockBy=a } | a <- elems as ]
instance ( Var v ) => FunctionSimulation (Loop v x) v x where
    simulate cntx@CycleCntx{ cycleCntx } (Loop (X x) (O vs) (I _))
        = case cycleCntx M.!? oneOf vs of
            -- if output variables are defined - nothing to do (values thrown on upper level)
            Just _  -> return cntx
            -- if output variables are not defined - set initial value
            Nothing -> setZipX cntx vs x


data LoopOut v x = LoopOut (Loop v x) (O v) deriving ( Typeable, Eq, Show )
instance ( Show v ) => Label (LoopOut v x) where
    label (LoopOut _ (O vs)) = show (oneOf vs) ++ " ->"
instance ( Ord v ) => Function (LoopOut v x) v where
    outputs (LoopOut _ o) = variables o
    isInternalLockPossible _ = True
instance ( Ord v ) => Patch (LoopOut v x) (v, v) where
    patch diff (LoopOut l a) = LoopOut (patch diff l) $ patch diff a
instance ( Var v ) => Locks (LoopOut v x) v where
    locks _ = []
instance ( Var v ) => FunctionSimulation (LoopOut v x) v x where
    simulate cntx (LoopOut l _) = simulate cntx l


data LoopIn v x = LoopIn (Loop v x) (I v) deriving ( Typeable, Eq, Show  )
instance ( Show v ) => Label (LoopIn v x) where
    label (LoopIn (Loop _ (O vs) _) (I v)) = "-> " ++ show v ++ " (" ++ show (oneOf vs) ++ ")"
instance ( Ord v ) => Function (LoopIn v x) v where
    inputs (LoopIn _ o) = variables o
    isInternalLockPossible _ = True
instance ( Ord v ) => Patch (LoopIn v x) (v, v) where
    patch diff (LoopIn l a) = LoopIn (patch diff l) $ patch diff a
instance ( Var v ) => Locks (LoopIn v x) v where locks (LoopIn l _) = locks l
instance ( Var v ) => FunctionSimulation (LoopIn v x) v x where
    simulate cntx (LoopIn l _) = simulate cntx l


data Reg v x = Reg (I v) (O v) deriving ( Typeable, Eq )
instance Label (Reg v x) where label Reg{} = "r"
instance ( Show v ) => Show (Reg v x) where
    show (Reg (I k1) (O k2)) = S.join " = " (map show $ elems k2) ++ " = reg(" ++ show k1 ++ ")"
reg :: ( Var v, Val x ) => v -> [v] -> F v x
reg a b = packF $ Reg (I a) (O $ fromList b)

instance ( Ord v ) => Function (Reg v x) v where
    inputs  (Reg a _b) = variables a
    outputs (Reg _a b) = variables b
instance ( Ord v ) => Patch (Reg v x) (v, v) where
    patch diff (Reg a b) = Reg (patch diff a) (patch diff b)
instance ( Var v ) => Locks (Reg v x) v where
    locks = inputsLockOutputs
instance ( Var v ) => FunctionSimulation (Reg v x) v x where
    simulate cntx (Reg (I v) (O vs)) = do
        x <- cntx `getX` v
        setZipX cntx vs x


data Add v x = Add (I v) (I v) (O v) deriving ( Typeable, Eq)
instance Label (Add v x) where label Add{} = "+"
instance ( Show v ) => Show (Add v x) where
    show (Add (I k1) (I k2) (O k3)) = S.join " = " (map show $ elems k3) ++ " = " ++ show k1 ++ " + " ++ show k2
add :: ( Var v, Val x, Num x ) => v -> v -> [v] -> F v x
add a b c = packF $ Add (I a) (I b) $ O $ fromList c

instance ( Ord v ) => Function (Add v x) v where
    inputs  (Add  a  b _c) = variables a `union` variables b
    outputs (Add _a _b  c) = variables c
instance ( Ord v ) => Patch (Add v x) (v, v) where
    patch diff (Add a b c) = Add (patch diff a) (patch diff b) (patch diff c)
instance ( Var v ) => Locks (Add v x) v where
    locks = inputsLockOutputs
instance ( Var v, Num x ) => FunctionSimulation (Add v x) v x where
    simulate cntx (Add (I v1) (I v2) (O vs)) = do
        x1 <- cntx `getX` v1
        x2 <- cntx `getX` v2
        let x3 = x1 + x2 -- + 1 -- can be used for checking test working
        setZipX cntx vs x3


data Sub v x = Sub (I v) (I v) (O v) deriving ( Typeable, Eq)
instance Label (Sub v x) where label Sub{} = "-"
instance ( Show v ) => Show (Sub v x) where
    show (Sub (I k1) (I k2) (O k3)) = S.join " = " (map show $ elems k3) ++ " = " ++ show k1 ++ " - " ++ show k2
sub :: ( Var v, Val x, Num x ) => v -> v -> [v] -> F v x
sub a b c = packF $ Sub (I a) (I b) $ O $ fromList c

instance ( Ord v ) => Function (Sub v x) v where
    inputs  (Sub  a  b _c) = variables a `union` variables b
    outputs (Sub _a _b  c) = variables c
instance ( Ord v ) => Patch (Sub v x) (v, v) where
    patch diff (Sub a b c) = Sub (patch diff a) (patch diff b) (patch diff c)
instance ( Var v ) => Locks (Sub v x) v where
    locks = inputsLockOutputs
instance ( Var v, Num x ) => FunctionSimulation (Sub v x) v x where
    simulate cntx (Sub (I v1) (I v2) (O vs)) = do
        x1 <- cntx `getX` v1
        x2 <- cntx `getX` v2
        let x3 = x1 - x2
        setZipX cntx vs x3


data Multiply v x = Multiply (I v) (I v) (O v) deriving ( Typeable, Eq )
instance Label (Multiply v x) where label Multiply{} = "*"
instance ( Show v ) => Show (Multiply v x) where
    show (Multiply (I k1) (I k2) (O k3)) = S.join " = " (map show $ elems k3) ++ " = " ++ show k1 ++ " * " ++ show k2
multiply :: ( Var v, Val x, Num x ) => v -> v -> [v] -> F v x
multiply a b c = packF $ Multiply (I a) (I b) $ O $ fromList c

instance ( Ord v ) => Function (Multiply v x) v where
    inputs  (Multiply  a  b _c) = variables a `union` variables b
    outputs (Multiply _a _b  c) = variables c
instance ( Ord v ) => Patch (Multiply v x) (v, v) where
    patch diff (Multiply a b c) = Multiply (patch diff a) (patch diff b) (patch diff c)
instance ( Var v ) => Locks (Multiply v x) v where
    locks = inputsLockOutputs
instance ( Var v, Num x ) => FunctionSimulation (Multiply v x) v x where
    simulate cntx (Multiply (I v1) (I v2) (O vs)) = do
        x1 <- cntx `getX` v1
        x2 <- cntx `getX` v2
        let x3 = x1 * x2
        setZipX cntx vs x3


data Division v x = Division
    { denom, numer     :: I v
    , quotient, remain :: O v
    } deriving ( Typeable, Eq )
instance Label (Division v x) where label Division{} = "/"
instance ( Show v ) => Show (Division v x) where
    show (Division (I k1) (I k2) (O k3) (O k4))
        =  S.join " = " (map show $ elems k3) ++ " = " ++ show k1 ++ " / " ++ show k2 ++ "; "
        ++ S.join " = " (if null k4 then ["_"] else map show $ elems k4) ++ " = " ++ show k1 ++ " `mod` " ++ show k2
division :: ( Var v, Val x, Integral x ) => v -> v -> [v] -> [v] -> F v x
division d n q r = packF $ Division
        { denom=I d
        , numer=I n
        , quotient=O $ fromList q
        , remain=O $ fromList r
        }


instance ( Ord v ) => Function (Division v x) v where
    inputs  Division{ denom, numer } = variables denom `union` variables numer
    outputs Division{ quotient, remain } = variables quotient `union` variables remain
instance ( Ord v ) => Patch (Division v x) (v, v) where
    patch diff (Division a b c d) = Division (patch diff a) (patch diff b) (patch diff c) (patch diff d)
instance ( Var v ) => Locks (Division v x) v where
    locks = inputsLockOutputs
instance ( Var v, Integral x ) => FunctionSimulation (Division v x) v x where
    simulate cntx Division{ denom=I d, numer=I n, quotient=O qs, remain=O rs } = do
        dx <- cntx `getX` d
        nx <- cntx `getX` n
        let (qx, rx) = dx `quotRem` nx
        cntx' <- setZipX cntx qs qx
        setZipX cntx' rs rx


data Constant v x = Constant (X x) (O v) deriving ( Typeable, Eq )
instance ( Show x ) => Label (Constant v x) where label (Constant (X x) _) = show x
instance ( Show v, Show x ) => Show (Constant v x) where
    show (Constant (X x) (O k)) = S.join " = " (map show $ elems k) ++ " = const(" ++ show x ++ ")"
constant :: ( Var v, Val x ) => x -> [v] -> F v x
constant x vs = packF $ Constant (X x) $ O $ fromList vs

instance ( Show x, Eq x, Typeable x ) => Function (Constant v x) v where
    outputs (Constant _ o) = variables o
instance ( Ord v ) => Patch (Constant v x) (v, v) where
    patch diff (Constant x a) = Constant x (patch diff a)
instance ( Var v ) => Locks (Constant v x) v where locks _ = []
instance ( Var v ) => FunctionSimulation (Constant v x) v x where
    simulate cntx (Constant (X x) (O vs))
        = setZipX cntx vs x


-- FIXME: just fixme
data ShiftLR v x = ShiftL (I v) (O v)
                 | ShiftR (I v) (O v)
                deriving ( Typeable, Eq )
instance ( Show v ) => Show (ShiftLR v x) where
    show (ShiftL (I k1) (O k2)) = S.join " = " (map show $ elems k2) ++ " = " ++ show k1 ++ " << 1"
    show (ShiftR (I k1) (O k2)) = S.join " = " (map show $ elems k2) ++ " = " ++ show k1 ++ " >> 1"
instance ( Show v ) => Label (ShiftLR v x) where label = show

shiftL :: ( Var v, Val x ) => v -> [v] -> F v x
shiftL a b = packF $ ShiftL (I a) $ O $ fromList b
shiftR :: ( Var v, Val x ) => v -> [v] -> F v x
shiftR a b = packF $ ShiftR (I a) $ O $ fromList b

instance ( Ord v ) => Function (ShiftLR v x) v where
    outputs (ShiftL i o) = variables i `union` variables o
    outputs (ShiftR i o) = variables i `union` variables o
instance ( Ord v ) => Patch (ShiftLR v x) (v, v) where
    patch diff (ShiftL a b) = ShiftL (patch diff a) (patch diff b)
    patch diff (ShiftR a b) = ShiftR (patch diff a) (patch diff b)
instance ( Var v ) => Locks (ShiftLR v x) v where
    locks = inputsLockOutputs
instance ( Var v, B.Bits x ) => FunctionSimulation (ShiftLR v x) v x where
    simulate cntx (ShiftL (I v1) (O vs)) = do
        x <- cntx `getX` v1
        let x' = x `B.shiftL` 1
        setZipX cntx vs x'
    simulate cntx (ShiftR (I v1) (O vs)) = do
        x <- cntx `getX` v1
        let x' = x `B.shiftR` 1
        setZipX cntx vs x'


newtype Send v x = Send (I v) deriving ( Typeable, Eq, Show )
instance Label (Send v x) where label Send{} = "send"
send :: ( Var v, Val x ) => v -> F v x
send a = packF $ Send $ I a
instance ( Ord v ) => Function (Send v x) v where
    inputs (Send i) = variables i
instance ( Ord v ) => Patch (Send v x) (v, v) where
    patch diff (Send a) = Send (patch diff a)
instance ( Var v ) => Locks (Send v x) v where locks _ = []
instance FunctionSimulation (Send v x) v x where
    simulate cntx Send{} = return cntx


newtype Receive v x = Receive (O v) deriving ( Typeable, Eq, Show )
instance Label (Receive v x) where label Receive{} = "receive"
receive :: ( Var v, Val x ) => [v] -> F v x
receive a = packF $ Receive $ O $ fromList a
instance ( Ord v ) => Function (Receive v x) v where
    outputs (Receive o) = variables o
instance ( Ord v ) => Patch (Receive v x) (v, v) where
    patch diff (Receive a) = Receive (patch diff a)
instance ( Var v ) => Locks (Receive v x) v where locks _ = []
instance ( Var v, Val x ) => FunctionSimulation (Receive v x) v x where
    simulate cntx@CycleCntx{ cycleCntx } (Receive (O vs))
        = case cycleCntx M.!? oneOf vs of
            -- if output variables are defined - nothing to do (values thrown on upper level)
            Just _  -> return cntx
            -- if output variables are not defined - set initial value
            Nothing -> setZipX cntx vs def
