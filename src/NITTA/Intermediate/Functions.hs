{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
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
module NITTA.Intermediate.Functions (
    -- *Arithmetics
    Add (..),
    add,
    Division (..),
    division,
    Multiply (..),
    multiply,
    ShiftLR (..),
    shiftL,
    shiftR,
    Sub (..),
    sub,
    Neg (..),
    neg,
    module NITTA.Intermediate.Functions.Accum,

    -- *Memory
    Constant (..),
    constant,
    Loop (..),
    loop,
    isLoop,
    LoopEnd (..),
    LoopBegin (..),
    Buffer (..),
    buffer,

    -- *Input/Output
    Receive (..),
    receive,
    Send (..),
    send,

    -- *Internal
    BrokenBuffer (..),
    brokenBuffer,
) where

import qualified Data.Bits as B
import Data.Default
import qualified Data.HashMap.Strict as HM
import Data.Set (elems, fromList, union)
import Data.Typeable
import NITTA.Intermediate.Functions.Accum
import NITTA.Intermediate.Types
import NITTA.Utils.Base

{- |Loop -- function for transfer data between computational cycles.
Let see the simple example with the following implementation of the
Fibonacci algorithm.

Data flow graph:

@
    +---------------------------------+
    |                                 |
    v                                 |
+------+                          b2  |
| Loop |      b1_1  +-----+    +------+
+------+----+------>|     |    |
            | a1    | sum +----+
+------+----------->|     |
| Loop |    |       +-----+      b1_2
+------+    +-------------------------+
    ^                                 |
    |                                 |
    +---------------------------------+
@

Lua source code:

@
function fib(a1, b1)
    b2 = a1 + b1
    fib(b1, b2)
end
fib(0, 1)
@

Data flow defines computation for a single computational cycle. But
a controller should repeat the algorithm infinite times, and
usually, it is required to transfer data between cycles. `Loop`
allows doing that. At first cycle, `Loop` function produces an
initial value (`X x`), after that on each cycle `Loop` produces a
variable value from the previous cycle, and consumes a new value at
the end of the cycle.

Computational process:

@
         ][                 Cycle 1                 ][                Cycle 2                  ]
         ][                                         ][                                         ]
initial  ][ ---+                          b2   +--- ][ ---+                          b2   +--- ]
 value   ][ op |      b1_1  +-----+    +------>| Lo ][ op |      b1_1  +-----+    +------>| Lo ]
 is a    ][ ---+----+------>|     |    |       +--- ][ ---+----+------>|     |    |       +--- ]
part of  ][         |       | sum +----+            ][         |       | sum +----+            ]
software ][ ---+----------->|     |            +--- ][ ---+----------->|     |            +--- ]
         ][ op |    |       +-----+     b1_2   | Lo ][ op |    |       +-----+      b1_2  | Lo ]
         ][ ---+    +------------------------->+--- ][ ---+    +------------------------->+--- ]
         ][                                         ][                                         ]
@

Similation data:

+--------------+----+----+----+
| Cycle number | a1 | b1 | b2 |
+==============+====+====+====+
| 1            | 0  | 1  | 1  |
+--------------+----+----+----+
| 2            | 1  | 1  | 2  |
+--------------+----+----+----+
| 3            | 1  | 2  | 3  |
+--------------+----+----+----+
| 4            | 2  | 3  | 5  |
+--------------+----+----+----+

In practice, Loop function supported by Fram processor unit in the
following way: Loop function should be prepared before execution by
automatical refactor @BreakLoop@, which replace Loop by @LoopEnd@
and @LoopBegin@.
-}
data Loop v x = Loop (X x) (O v) (I v) deriving (Typeable, Eq)

instance (Var v, Show x) => Show (Loop v x) where show = label
instance (Var v, Show x) => Label (Loop v x) where
    label (Loop (X x) os i) =
        "loop(" <> show x <> ", " <> show i <> ") = " <> show os
loop :: (Var v, Val x) => x -> v -> [v] -> F v x
loop x a bs = packF $ Loop (X x) (O $ fromList bs) $ I a
isLoop f
    | Just Loop{} <- castF f = True
    | otherwise = False

instance Function (Loop v x) v where
    isInternalLockPossible _ = True
    inputs (Loop _ _a b) = variables b
    outputs (Loop _ a _b) = variables a
instance (Var v) => Patch (Loop v x) (v, v) where
    patch diff (Loop x a b) = Loop x (patch diff a) (patch diff b)
instance (Var v) => Locks (Loop v x) v where
    locks (Loop _ (O as) (I b)) = [Lock{locked = b, lockBy = a} | a <- elems as]
instance (Var v) => FunctionSimulation (Loop v x) v x where
    simulate CycleCntx{cycleCntx} (Loop (X x) (O vs) (I _)) =
        case oneOf vs `HM.lookup` cycleCntx of
            -- if output variables are defined - nothing to do (values thrown on upper level)
            Just _ -> []
            -- if output variables are not defined - set initial value
            Nothing -> [(v, x) | v <- elems vs]

data LoopBegin v x = LoopBegin (Loop v x) (O v) deriving (Typeable, Eq, Show)
instance (Var v) => Label (LoopBegin v x) where
    label (LoopBegin _ os) = "LoopBegin() = " <> show os
instance (Var v) => Function (LoopBegin v x) v where
    outputs (LoopBegin _ o) = variables o
    isInternalLockPossible _ = True
instance (Var v) => Patch (LoopBegin v x) (v, v) where
    patch diff (LoopBegin l a) = LoopBegin (patch diff l) $ patch diff a
instance (Var v) => Locks (LoopBegin v x) v where
    locks _ = []
instance (Var v) => FunctionSimulation (LoopBegin v x) v x where
    simulate cntx (LoopBegin l _) = simulate cntx l

data LoopEnd v x = LoopEnd (Loop v x) (I v) deriving (Typeable, Eq, Show)
instance (Var v) => Label (LoopEnd v x) where
    label (LoopEnd (Loop _ os _) i) = "LoopEnd(" <> show i <> ") pair out: " <> show os
instance (Var v) => Function (LoopEnd v x) v where
    inputs (LoopEnd _ o) = variables o
    isInternalLockPossible _ = True
instance (Var v) => Patch (LoopEnd v x) (v, v) where
    patch diff (LoopEnd l a) = LoopEnd (patch diff l) $ patch diff a
instance (Var v) => Locks (LoopEnd v x) v where locks (LoopEnd l _) = locks l
instance (Var v) => FunctionSimulation (LoopEnd v x) v x where
    simulate cntx (LoopEnd l _) = simulate cntx l

data Buffer v x = Buffer (I v) (O v) deriving (Typeable, Eq)
instance Label (Buffer v x) where label Buffer{} = "buf"
instance (Var v) => Show (Buffer v x) where
    show (Buffer i os) = "buffer(" <> show i <> ")" <> " = " <> show os
buffer :: (Var v, Val x) => v -> [v] -> F v x
buffer a b = packF $ Buffer (I a) (O $ fromList b)

instance (Var v) => Function (Buffer v x) v where
    inputs (Buffer a _b) = variables a
    outputs (Buffer _a b) = variables b
instance (Var v) => Patch (Buffer v x) (v, v) where
    patch diff (Buffer a b) = Buffer (patch diff a) (patch diff b)
instance (Var v) => Locks (Buffer v x) v where
    locks = inputsLockOutputs
instance (Var v) => FunctionSimulation (Buffer v x) v x where
    simulate cntx (Buffer (I a) (O vs)) =
        [(v, cntx `getCntx` a) | v <- elems vs]

data Add v x = Add (I v) (I v) (O v) deriving (Typeable, Eq)
instance Label (Add v x) where label Add{} = "+"
instance (Var v) => Show (Add v x) where
    show (Add a b c) =
        let lexp = show a <> " + " <> show b
            rexp = show c
         in lexp <> " = " <> rexp
add :: (Var v, Val x) => v -> v -> [v] -> F v x
add a b c = packF $ Add (I a) (I b) $ O $ fromList c

instance (Var v) => Function (Add v x) v where
    inputs (Add a b _c) = variables a `union` variables b
    outputs (Add _a _b c) = variables c
instance (Var v) => Patch (Add v x) (v, v) where
    patch diff (Add a b c) = Add (patch diff a) (patch diff b) (patch diff c)
instance (Var v) => Locks (Add v x) v where
    locks = inputsLockOutputs
instance (Var v, Num x) => FunctionSimulation (Add v x) v x where
    simulate cntx (Add (I v1) (I v2) (O vs)) =
        let x1 = cntx `getCntx` v1
            x2 = cntx `getCntx` v2
            y = x1 + x2
         in [(v, y) | v <- elems vs]

data Sub v x = Sub (I v) (I v) (O v) deriving (Typeable, Eq)
instance Label (Sub v x) where label Sub{} = "-"
instance (Var v) => Show (Sub v x) where
    show (Sub a b c) =
        let lexp = show a <> " - " <> show b
            rexp = show c
         in lexp <> " = " <> rexp
sub :: (Var v, Val x) => v -> v -> [v] -> F v x
sub a b c = packF $ Sub (I a) (I b) $ O $ fromList c

instance (Var v) => Function (Sub v x) v where
    inputs (Sub a b _c) = variables a `union` variables b
    outputs (Sub _a _b c) = variables c
instance (Var v) => Patch (Sub v x) (v, v) where
    patch diff (Sub a b c) = Sub (patch diff a) (patch diff b) (patch diff c)
instance (Var v) => Locks (Sub v x) v where
    locks = inputsLockOutputs
instance (Var v, Num x) => FunctionSimulation (Sub v x) v x where
    simulate cntx (Sub (I v1) (I v2) (O vs)) =
        let x1 = cntx `getCntx` v1
            x2 = cntx `getCntx` v2
            y = x1 - x2
         in [(v, y) | v <- elems vs]

data Multiply v x = Multiply (I v) (I v) (O v) deriving (Typeable, Eq)
instance Label (Multiply v x) where label Multiply{} = "*"
instance (Var v) => Show (Multiply v x) where
    show (Multiply a b c) =
        show a <> " * " <> show b <> " = " <> show c
multiply :: (Var v, Val x) => v -> v -> [v] -> F v x
multiply a b c = packF $ Multiply (I a) (I b) $ O $ fromList c

instance (Var v) => Function (Multiply v x) v where
    inputs (Multiply a b _c) = variables a `union` variables b
    outputs (Multiply _a _b c) = variables c
instance (Var v) => Patch (Multiply v x) (v, v) where
    patch diff (Multiply a b c) = Multiply (patch diff a) (patch diff b) (patch diff c)
instance (Var v) => Locks (Multiply v x) v where
    locks = inputsLockOutputs
instance (Var v, Num x) => FunctionSimulation (Multiply v x) v x where
    simulate cntx (Multiply (I v1) (I v2) (O vs)) =
        let x1 = cntx `getCntx` v1
            x2 = cntx `getCntx` v2
            y = x1 * x2
         in [(v, y) | v <- elems vs]

data Division v x = Division
    { denom, numer :: I v
    , quotient, remain :: O v
    }
    deriving (Typeable, Eq)
instance Label (Division v x) where label Division{} = "/"
instance (Var v) => Show (Division v x) where
    show Division{denom, numer, quotient, remain} =
        let q = show numer <> " / " <> show denom <> " = " <> show quotient
            r = show numer <> " mod " <> show denom <> " = " <> show remain
         in q <> "; " <> r
division :: (Var v, Val x) => v -> v -> [v] -> [v] -> F v x
division d n q r =
    packF $
        Division
            { denom = I d
            , numer = I n
            , quotient = O $ fromList q
            , remain = O $ fromList r
            }

instance (Var v) => Function (Division v x) v where
    inputs Division{denom, numer} = variables denom `union` variables numer
    outputs Division{quotient, remain} = variables quotient `union` variables remain
instance (Var v) => Patch (Division v x) (v, v) where
    patch diff (Division a b c d) = Division (patch diff a) (patch diff b) (patch diff c) (patch diff d)
instance (Var v) => Locks (Division v x) v where
    locks = inputsLockOutputs
instance (Var v, Integral x) => FunctionSimulation (Division v x) v x where
    simulate cntx Division{denom = I d, numer = I n, quotient = O qs, remain = O rs} =
        let dx = cntx `getCntx` d
            nx = cntx `getCntx` n
            (qx, rx) = dx `quotRem` nx
         in [(v, qx) | v <- elems qs] ++ [(v, rx) | v <- elems rs]

data Neg v x = Neg (I v) (O v) deriving (Typeable, Eq)
instance Label (Neg v x) where label Neg{} = "-"
instance (Show v) => Show (Neg v x) where
    show (Neg (I i) (O o)) =
        let lexp = "-" <> show i
            rexp = showOut o
         in lexp <> " = " <> rexp
neg :: (Var v, Val x) => v -> [v] -> F v x
neg i o = packF $ Neg (I i) $ O $ fromList o

instance (Ord v) => Function (Neg v x) v where
    inputs (Neg i _) = variables i
    outputs (Neg _ o) = variables o
instance (Ord v) => Patch (Neg v x) (v, v) where
    patch diff (Neg i o) = Neg (patch diff i) (patch diff o)
instance (Var v) => Locks (Neg v x) v where
    locks = inputsLockOutputs
instance (Var v, Num x) => FunctionSimulation (Neg v x) v x where
    simulate cntx (Neg (I i) (O o)) =
        let x1 = cntx `getCntx` i
            y = - x1
         in [(v, y) | v <- elems o]

data Constant v x = Constant (X x) (O v) deriving (Typeable, Eq)
instance (Show x) => Label (Constant v x) where label (Constant (X x) _) = show x
instance (Var v, Show x) => Show (Constant v x) where
    show (Constant (X x) os) = "const(" <> show x <> ") = " <> show os
constant :: (Var v, Val x) => x -> [v] -> F v x
constant x vs = packF $ Constant (X x) $ O $ fromList vs

instance (Show x, Eq x, Typeable x) => Function (Constant v x) v where
    outputs (Constant _ o) = variables o
instance (Var v) => Patch (Constant v x) (v, v) where
    patch diff (Constant x a) = Constant x (patch diff a)
instance (Var v) => Locks (Constant v x) v where locks _ = []
instance FunctionSimulation (Constant v x) v x where
    simulate _cntx (Constant (X x) (O vs)) = [(v, x) | v <- elems vs]

-- TODO: separete into two different functions

-- |Functional unit that implements logic shift operations
data ShiftLR v x
    = ShiftL Int (I v) (O v)
    | ShiftR Int (I v) (O v)
    deriving (Typeable, Eq)

instance (Var v) => Show (ShiftLR v x) where
    show (ShiftL s i os) = show i <> " << " <> show s <> " = " <> show os
    show (ShiftR s i os) = show i <> " >> " <> show s <> " = " <> show os
instance (Var v) => Label (ShiftLR v x) where label = show

shiftL :: (Var v, Val x) => Int -> v -> [v] -> F v x
shiftL s i o = packF $ ShiftL s (I i) $ O $ fromList o
shiftR :: (Var v, Val x) => Int -> v -> [v] -> F v x
shiftR s i o = packF $ ShiftR s (I i) $ O $ fromList o

instance (Var v) => Function (ShiftLR v x) v where
    inputs (ShiftL _ i _) = variables i
    inputs (ShiftR _ i _) = variables i
    outputs (ShiftL _ _ o) = variables o
    outputs (ShiftR _ _ o) = variables o
instance (Var v) => Patch (ShiftLR v x) (v, v) where
    patch diff (ShiftL s i o) = ShiftL s (patch diff i) (patch diff o)
    patch diff (ShiftR s i o) = ShiftR s (patch diff i) (patch diff o)
instance (Var v) => Locks (ShiftLR v x) v where
    locks = inputsLockOutputs
instance (Var v, B.Bits x) => FunctionSimulation (ShiftLR v x) v x where
    simulate cntx (ShiftL s (I i) (O os)) = do
        [(o, getCntx cntx i `B.shiftL` s) | o <- elems os]
    simulate cntx (ShiftR s (I i) (O os)) = do
        [(o, getCntx cntx i `B.shiftR` s) | o <- elems os]

newtype Send v x = Send (I v) deriving (Typeable, Eq)
instance (Var v) => Show (Send v x) where
    show (Send i) = "send(" <> show i <> ")"
instance Label (Send v x) where label Send{} = "send"
send :: (Var v, Val x) => v -> F v x
send a = packF $ Send $ I a
instance (Var v) => Function (Send v x) v where
    inputs (Send i) = variables i
instance (Var v) => Patch (Send v x) (v, v) where
    patch diff (Send a) = Send (patch diff a)
instance (Var v) => Locks (Send v x) v where locks _ = []
instance FunctionSimulation (Send v x) v x where
    simulate _cntx Send{} = []

newtype Receive v x = Receive (O v) deriving (Typeable, Eq)
instance (Var v) => Show (Receive v x) where
    show (Receive os) = "receive() = " <> show os
instance Label (Receive v x) where label Receive{} = "receive"
receive :: (Var v, Val x) => [v] -> F v x
receive a = packF $ Receive $ O $ fromList a
instance (Var v) => Function (Receive v x) v where
    outputs (Receive o) = variables o
instance (Var v) => Patch (Receive v x) (v, v) where
    patch diff (Receive a) = Receive (patch diff a)
instance (Var v) => Locks (Receive v x) v where locks _ = []
instance (Var v, Val x) => FunctionSimulation (Receive v x) v x where
    simulate CycleCntx{cycleCntx} (Receive (O vs)) =
        case oneOf vs `HM.lookup` cycleCntx of
            -- if output variables are defined - nothing to do (values thrown on upper level)
            Just _ -> []
            -- if output variables are not defined - set initial value
            Nothing -> [(v, def) | v <- elems vs]

-- |Special function for negative tests only.
data BrokenBuffer v x = BrokenBuffer (I v) (O v) deriving (Typeable, Eq)

instance Label (BrokenBuffer v x) where label BrokenBuffer{} = "broken"
instance (Var v) => Show (BrokenBuffer v x) where
    show (BrokenBuffer i os) = "brokenBuffer(" <> show i <> ")" <> " = " <> show os
brokenBuffer :: (Var v, Val x) => v -> [v] -> F v x
brokenBuffer a b = packF $ BrokenBuffer (I a) (O $ fromList b)

instance (Var v) => Function (BrokenBuffer v x) v where
    inputs (BrokenBuffer a _b) = variables a
    outputs (BrokenBuffer _a b) = variables b
instance (Var v) => Patch (BrokenBuffer v x) (v, v) where
    patch diff (BrokenBuffer a b) = BrokenBuffer (patch diff a) (patch diff b)
instance (Var v) => Locks (BrokenBuffer v x) v where
    locks = inputsLockOutputs
instance (Var v) => FunctionSimulation (BrokenBuffer v x) v x where
    simulate cntx (BrokenBuffer (I a) (O vs)) = [(v, cntx `getCntx` a) | v <- elems vs]
