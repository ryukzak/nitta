{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

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
    -- * Arithmetics
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

    -- * Memory
    Constant (..),
    constant,
    isConst,
    Loop (..),
    loop,
    isLoop,
    LoopEnd (..),
    LoopBegin (..),
    Buffer (..),
    buffer,

    -- * Input/Output
    Receive (..),
    receive,
    Send (..),
    send,

    -- * Internal
    BrokenBuffer (..),
    brokenBuffer,
    Lut (..),

    -- * Logic
    LogicFunction (..),
    logicAnd,
    logicOr,
    logicNot,
    LogicCompare (..),
    Op (..),
    logicCompare,
    Mux (..),
    mux,
) where

import Data.Bits qualified as B
import Data.Data (Data)
import Data.Default
import Data.HashMap.Strict qualified as HM
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (elems, fromList, union)
import Data.Set qualified as S
import Data.Typeable
import NITTA.Intermediate.Functions.Accum
import NITTA.Intermediate.Types
import NITTA.Utils.Base

{- | Loop -- function for transfer data between computational cycles.
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
instance Var v => Patch (Loop v x) (v, v) where
    patch diff (Loop x a b) = Loop x (patch diff a) (patch diff b)
instance Var v => Locks (Loop v x) v where
    locks (Loop _ (O as) (I b)) = [Lock{locked = b, lockBy = a} | a <- elems as]
instance Var v => FunctionSimulation (Loop v x) v x where
    simulate CycleCntx{cycleCntx} (Loop (X x) (O vs) (I _)) =
        case oneOf vs `HM.lookup` cycleCntx of
            -- if output variables are defined - nothing to do (values thrown on upper level)
            Just _ -> []
            -- if output variables are not defined - set initial value
            Nothing -> [(v, x) | v <- elems vs]

data LoopBegin v x = LoopBegin (Loop v x) (O v) deriving (Typeable, Eq)
instance (Var v, Show x) => Show (LoopBegin v x) where show = label
instance Var v => Label (LoopBegin v x) where
    label (LoopBegin _ os) = "LoopBegin() = " <> show os
instance Var v => Function (LoopBegin v x) v where
    outputs (LoopBegin _ o) = variables o
    isInternalLockPossible _ = True
instance Var v => Patch (LoopBegin v x) (v, v) where
    patch diff (LoopBegin l a) = LoopBegin (patch diff l) $ patch diff a
instance Var v => Locks (LoopBegin v x) v where
    locks _ = []
instance Var v => FunctionSimulation (LoopBegin v x) v x where
    simulate cntx (LoopBegin l _) = simulate cntx l

data LoopEnd v x = LoopEnd (Loop v x) (I v) deriving (Typeable, Eq)
instance (Var v, Show x) => Show (LoopEnd v x) where show = label
instance Var v => Label (LoopEnd v x) where
    label (LoopEnd (Loop _ os _) i) = "LoopEnd(" <> show i <> ") pair out: " <> show os
instance Var v => Function (LoopEnd v x) v where
    inputs (LoopEnd _ o) = variables o
    isInternalLockPossible _ = True
instance Var v => Patch (LoopEnd v x) (v, v) where
    patch diff (LoopEnd l a) = LoopEnd (patch diff l) $ patch diff a
instance Var v => Locks (LoopEnd v x) v where locks (LoopEnd l _) = locks l
instance Var v => FunctionSimulation (LoopEnd v x) v x where
    simulate cntx (LoopEnd l _) = simulate cntx l

data Buffer v x = Buffer (I v) (O v) deriving (Typeable, Eq)
instance Label (Buffer v x) where label Buffer{} = "buf"
instance Var v => Show (Buffer v x) where
    show (Buffer i os) = "buffer(" <> show i <> ")" <> " = " <> show os
buffer :: (Var v, Val x) => v -> [v] -> F v x
buffer a b = packF $ Buffer (I a) (O $ fromList b)

instance Var v => Function (Buffer v x) v where
    inputs (Buffer a _b) = variables a
    outputs (Buffer _a b) = variables b
instance Var v => Patch (Buffer v x) (v, v) where
    patch diff (Buffer a b) = Buffer (patch diff a) (patch diff b)
instance Var v => Locks (Buffer v x) v where
    locks = inputsLockOutputs
instance Var v => FunctionSimulation (Buffer v x) v x where
    simulate cntx (Buffer (I a) (O vs)) =
        [(v, cntx `getCntx` a) | v <- elems vs]

data Add v x = Add (I v) (I v) (O v) deriving (Typeable, Eq)
instance Label (Add v x) where label Add{} = "+"
instance Var v => Show (Add v x) where
    show (Add a b c) =
        let lexp = show a <> " + " <> show b
            rexp = show c
         in lexp <> " = " <> rexp
add :: (Var v, Val x) => v -> v -> [v] -> F v x
add a b c = packF $ Add (I a) (I b) $ O $ fromList c

instance Var v => Function (Add v x) v where
    inputs (Add a b _c) = variables a `union` variables b
    outputs (Add _a _b c) = variables c
instance Var v => Patch (Add v x) (v, v) where
    patch diff (Add a b c) = Add (patch diff a) (patch diff b) (patch diff c)
instance Var v => Locks (Add v x) v where
    locks = inputsLockOutputs
instance (Var v, Num x) => FunctionSimulation (Add v x) v x where
    simulate cntx (Add (I v1) (I v2) (O vs)) =
        let x1 = cntx `getCntx` v1
            x2 = cntx `getCntx` v2
            y = x1 + x2
         in [(v, y) | v <- elems vs]

data Sub v x = Sub (I v) (I v) (O v) deriving (Typeable, Eq)
instance Label (Sub v x) where label Sub{} = "-"
instance Var v => Show (Sub v x) where
    show (Sub a b c) =
        let lexp = show a <> " - " <> show b
            rexp = show c
         in lexp <> " = " <> rexp
sub :: (Var v, Val x) => v -> v -> [v] -> F v x
sub a b c = packF $ Sub (I a) (I b) $ O $ fromList c

instance Var v => Function (Sub v x) v where
    inputs (Sub a b _c) = variables a `union` variables b
    outputs (Sub _a _b c) = variables c
instance Var v => Patch (Sub v x) (v, v) where
    patch diff (Sub a b c) = Sub (patch diff a) (patch diff b) (patch diff c)
instance Var v => Locks (Sub v x) v where
    locks = inputsLockOutputs
instance (Var v, Num x) => FunctionSimulation (Sub v x) v x where
    simulate cntx (Sub (I v1) (I v2) (O vs)) =
        let x1 = cntx `getCntx` v1
            x2 = cntx `getCntx` v2
            y = x1 - x2
         in [(v, y) | v <- elems vs]

data Multiply v x = Multiply (I v) (I v) (O v) deriving (Typeable, Eq)
instance Label (Multiply v x) where label Multiply{} = "*"
instance Var v => Show (Multiply v x) where
    show (Multiply a b c) =
        show a <> " * " <> show b <> " = " <> show c
multiply :: (Var v, Val x) => v -> v -> [v] -> F v x
multiply a b c = packF $ Multiply (I a) (I b) $ O $ fromList c

instance Var v => Function (Multiply v x) v where
    inputs (Multiply a b _c) = variables a `union` variables b
    outputs (Multiply _a _b c) = variables c
instance Var v => Patch (Multiply v x) (v, v) where
    patch diff (Multiply a b c) = Multiply (patch diff a) (patch diff b) (patch diff c)
instance Var v => Locks (Multiply v x) v where
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
instance Var v => Show (Division v x) where
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

instance Var v => Function (Division v x) v where
    inputs Division{denom, numer} = variables denom `union` variables numer
    outputs Division{quotient, remain} = variables quotient `union` variables remain
instance Var v => Patch (Division v x) (v, v) where
    patch diff (Division a b c d) = Division (patch diff a) (patch diff b) (patch diff c) (patch diff d)
instance Var v => Locks (Division v x) v where
    locks = inputsLockOutputs
instance (Var v, Integral x) => FunctionSimulation (Division v x) v x where
    simulate cntx Division{denom = I d, numer = I n, quotient = O qs, remain = O rs} =
        let dx = cntx `getCntx` d
            nx = cntx `getCntx` n
            (qx, rx) = dx `quotRem` nx
         in [(v, qx) | v <- elems qs] ++ [(v, rx) | v <- elems rs]

data Neg v x = Neg (I v) (O v) deriving (Typeable, Eq)
instance Label (Neg v x) where label Neg{} = "neg"
instance Var v => Show (Neg v x) where
    show (Neg i o) = "-" <> show i <> " = " <> show o

neg :: (Var v, Val x) => v -> [v] -> F v x
neg i o = packF $ Neg (I i) $ O $ fromList o

instance Ord v => Function (Neg v x) v where
    inputs (Neg i _) = variables i
    outputs (Neg _ o) = variables o
instance Ord v => Patch (Neg v x) (v, v) where
    patch diff (Neg i o) = Neg (patch diff i) (patch diff o)
instance Var v => Locks (Neg v x) v where
    locks = inputsLockOutputs
instance (Var v, Num x) => FunctionSimulation (Neg v x) v x where
    simulate cntx (Neg (I i) (O o)) =
        let x1 = cntx `getCntx` i
            y = -x1
         in [(v, y) | v <- elems o]

data Constant v x = Constant (X x) (O v) deriving (Typeable, Eq)
instance Show x => Label (Constant v x) where label (Constant (X x) _) = show x
instance (Var v, Show x) => Show (Constant v x) where
    show (Constant (X x) os) = "const(" <> show x <> ") = " <> show os
constant :: (Var v, Val x) => x -> [v] -> F v x
constant x vs = packF $ Constant (X x) $ O $ fromList vs
isConst f
    | Just Constant{} <- castF f = True
    | otherwise = False

instance (Show x, Eq x, Typeable x) => Function (Constant v x) v where
    outputs (Constant _ o) = variables o
instance Var v => Patch (Constant v x) (v, v) where
    patch diff (Constant x a) = Constant x (patch diff a)
instance Var v => Locks (Constant v x) v where locks _ = []
instance FunctionSimulation (Constant v x) v x where
    simulate _cntx (Constant (X x) (O vs)) = [(v, x) | v <- elems vs]

-- TODO: separete into two different functions

-- | Functional unit that implements logic shift operations
data ShiftLR v x
    = ShiftL Int (I v) (O v)
    | ShiftR Int (I v) (O v)
    deriving (Typeable, Eq)

instance Var v => Show (ShiftLR v x) where
    show (ShiftL s i os) = show i <> " << " <> show s <> " = " <> show os
    show (ShiftR s i os) = show i <> " >> " <> show s <> " = " <> show os
instance Var v => Label (ShiftLR v x) where label = show

shiftL :: (Var v, Val x) => Int -> v -> [v] -> F v x
shiftL s i o = packF $ ShiftL s (I i) $ O $ fromList o
shiftR :: (Var v, Val x) => Int -> v -> [v] -> F v x
shiftR s i o = packF $ ShiftR s (I i) $ O $ fromList o

instance Var v => Function (ShiftLR v x) v where
    inputs (ShiftL _ i _) = variables i
    inputs (ShiftR _ i _) = variables i
    outputs (ShiftL _ _ o) = variables o
    outputs (ShiftR _ _ o) = variables o
instance Var v => Patch (ShiftLR v x) (v, v) where
    patch diff (ShiftL s i o) = ShiftL s (patch diff i) (patch diff o)
    patch diff (ShiftR s i o) = ShiftR s (patch diff i) (patch diff o)
instance Var v => Locks (ShiftLR v x) v where
    locks = inputsLockOutputs
instance (Var v, B.Bits x) => FunctionSimulation (ShiftLR v x) v x where
    simulate cntx (ShiftL s (I i) (O os)) = do
        [(o, getCntx cntx i `B.shiftL` s) | o <- elems os]
    simulate cntx (ShiftR s (I i) (O os)) = do
        [(o, getCntx cntx i `B.shiftR` s) | o <- elems os]

newtype Send v x = Send (I v) deriving (Typeable, Eq)
instance Var v => Show (Send v x) where
    show (Send i) = "send(" <> show i <> ")"
instance Label (Send v x) where label Send{} = "send"
send :: (Var v, Val x) => v -> F v x
send a = packF $ Send $ I a
instance Var v => Function (Send v x) v where
    inputs (Send i) = variables i
instance Var v => Patch (Send v x) (v, v) where
    patch diff (Send a) = Send (patch diff a)
instance Var v => Locks (Send v x) v where locks _ = []
instance FunctionSimulation (Send v x) v x where
    simulate _cntx Send{} = []

newtype Receive v x = Receive (O v) deriving (Typeable, Eq)
instance Var v => Show (Receive v x) where
    show (Receive os) = "receive() = " <> show os
instance Label (Receive v x) where label Receive{} = "receive"
receive :: (Var v, Val x) => [v] -> F v x
receive a = packF $ Receive $ O $ fromList a
instance Var v => Function (Receive v x) v where
    outputs (Receive o) = variables o
instance Var v => Patch (Receive v x) (v, v) where
    patch diff (Receive a) = Receive (patch diff a)
instance Var v => Locks (Receive v x) v where locks _ = []
instance (Var v, Val x) => FunctionSimulation (Receive v x) v x where
    simulate CycleCntx{cycleCntx} (Receive (O vs)) =
        case oneOf vs `HM.lookup` cycleCntx of
            -- if output variables are defined - nothing to do (values thrown on upper level)
            Just _ -> []
            -- if output variables are not defined - set initial value
            Nothing -> [(v, def) | v <- elems vs]

-- | Special function for negative tests only.
data BrokenBuffer v x = BrokenBuffer (I v) (O v) deriving (Typeable, Eq)

instance Label (BrokenBuffer v x) where label BrokenBuffer{} = "broken"
instance Var v => Show (BrokenBuffer v x) where
    show (BrokenBuffer i os) = "brokenBuffer(" <> show i <> ")" <> " = " <> show os
brokenBuffer :: (Var v, Val x) => v -> [v] -> F v x
brokenBuffer a b = packF $ BrokenBuffer (I a) (O $ fromList b)

instance Var v => Function (BrokenBuffer v x) v where
    inputs (BrokenBuffer a _b) = variables a
    outputs (BrokenBuffer _a b) = variables b
instance Var v => Patch (BrokenBuffer v x) (v, v) where
    patch diff (BrokenBuffer a b) = BrokenBuffer (patch diff a) (patch diff b)
instance Var v => Locks (BrokenBuffer v x) v where
    locks = inputsLockOutputs
instance Var v => FunctionSimulation (BrokenBuffer v x) v x where
    simulate cntx (BrokenBuffer (I a) (O vs)) = [(v, cntx `getCntx` a) | v <- elems vs]
data LogicFunction v x
    = LogicAnd (I v) (I v) (O v)
    | LogicOr (I v) (I v) (O v)
    | LogicNot (I v) (O v)
    deriving (Typeable, Eq)

deriving instance (Data v, Data (I v), Data (O v), Data x) => Data (LogicFunction v x)

data Op = CMP_EQ | CMP_LT | CMP_LTE | CMP_GT | CMP_GTE
    deriving (Typeable, Eq, Show, Data)

logicAnd :: (Var v, Val x) => v -> v -> [v] -> F v x
logicAnd a b c = packF $ LogicAnd (I a) (I b) $ O $ fromList c

logicOr :: (Var v, Val x) => v -> v -> [v] -> F v x
logicOr a b c = packF $ LogicOr (I a) (I b) $ O $ fromList c

logicNot :: (Var v, Val x) => v -> [v] -> F v x
logicNot a c = packF $ LogicNot (I a) $ O $ fromList c

instance Label (LogicFunction v x) where
    label LogicAnd{} = "and"
    label LogicOr{} = "or"
    label LogicNot{} = "not"

instance Var v => Patch (LogicFunction v x) (v, v) where
    patch diff (LogicAnd a b c) = LogicAnd (patch diff a) (patch diff b) (patch diff c)
    patch diff (LogicOr a b c) = LogicOr (patch diff a) (patch diff b) (patch diff c)
    patch diff (LogicNot a b) = LogicNot (patch diff a) (patch diff b)

instance Var v => Show (LogicFunction v x) where
    show (LogicAnd a b o) = show a <> " and " <> show b <> " = " <> show o
    show (LogicOr a b o) = show a <> " or " <> show b <> " = " <> show o
    show (LogicNot a o) = "not " <> show a <> " = " <> show o

instance Var v => Function (LogicFunction v x) v where
    inputs (LogicOr a b _) = variables a `S.union` variables b
    inputs (LogicAnd a b _) = variables a `S.union` variables b
    inputs (LogicNot a _) = variables a
    outputs (LogicOr _ _ o) = variables o
    outputs (LogicAnd _ _ o) = variables o
    outputs (LogicNot _ o) = variables o
instance (Var v, B.Bits x, Num x, Ord x) => FunctionSimulation (LogicFunction v x) v x where
    simulate cntx (LogicAnd (I a) (I b) (O o)) =
        let x1 = toBool (cntx `getCntx` a)
            x2 = toBool (cntx `getCntx` b)
            y = x1 * x2
         in [(v, y) | v <- S.elems o]
    simulate cntx (LogicOr (I a) (I b) (O o)) =
        let x1 = toBool (cntx `getCntx` a)
            x2 = toBool (cntx `getCntx` b)
            y = if x1 + x2 > 0 then 1 else 0
         in [(v, y) | v <- S.elems o]
    simulate cntx (LogicNot (I a) (O o)) =
        let x1 = toBool (cntx `getCntx` a)
            y = 1 - x1
         in [(v, y) | v <- S.elems o]

toBool :: (Num x, Eq x) => x -> x
toBool n = if n /= 0 then 1 else 0

instance Var v => Locks (LogicFunction v x) v where
    locks = inputsLockOutputs

data LogicCompare v x = LogicCompare Op (I v) (I v) (O v) deriving (Typeable, Eq)
instance Label (LogicCompare v x) where
    label (LogicCompare op _ _ _) = show op
instance Var v => Patch (LogicCompare v x) (v, v) where
    patch diff (LogicCompare op a b c) = LogicCompare op (patch diff a) (patch diff b) (patch diff c)

instance Var v => Show (LogicCompare v x) where
    show (LogicCompare op a b o) = show a <> " " <> show op <> " " <> show b <> " = " <> show o

instance Var v => Function (LogicCompare v x) v where
    inputs (LogicCompare _ a b _) = variables a `S.union` variables b
    outputs (LogicCompare _ _ _ o) = variables o
instance (Var v, Val x) => FunctionSimulation (LogicCompare v x) v x where
    simulate cntx (LogicCompare op (I a) (I b) (O o)) =
        let
            x1 = getCntx cntx a
            x2 = getCntx cntx b
            y = if op2func op x1 x2 then 1 else 0
         in
            [(v, y) | v <- S.elems o]
        where
            op2func CMP_EQ = (==)
            op2func CMP_LT = (<)
            op2func CMP_LTE = (<=)
            op2func CMP_GT = (>)
            op2func CMP_GTE = (>=)
instance Var v => Locks (LogicCompare v x) v where
    locks = inputsLockOutputs

logicCompare :: (Var v, Val x) => Op -> v -> v -> [v] -> F v x
logicCompare op a b c = packF $ LogicCompare op (I a) (I b) $ O $ fromList c

-- Look Up Table
data Lut v x = Lut (Map [Bool] Bool) [I v] (O v) deriving (Typeable, Eq)

instance Var v => Patch (Lut v x) (v, v) where
    patch (old, new) (Lut table ins out) =
        Lut table (patch (old, new) ins) (patch (old, new) out)

instance Var v => Locks (Lut v x) v where
    locks (Lut{}) = []

instance Label (Lut v x) where
    label (Lut{}) = "Lut"
instance Var v => Show (Lut v x) where
    show (Lut table ins output) = "Lut " <> show table <> " " <> show ins <> " = " <> show output

instance Var v => Function (Lut v x) v where
    inputs (Lut _ ins _) = S.unions $ map variables ins
    outputs (Lut _ _ output) = variables output

instance (Var v, Num x, Eq x) => FunctionSimulation (Lut v x) v x where
    simulate cntx (Lut table ins (O output)) =
        let inputValues = map (\(I v) -> cntx `getCntx` v == 1) ins
            result = M.findWithDefault False inputValues table -- todo add default value
         in [(v, fromIntegral (fromEnum result)) | v <- S.elems output]

data Mux v x = Mux [I v] (I v) (O v) deriving (Typeable, Eq)

instance Var v => Patch (Mux v x) (v, v) where
    patch (old, new) (Mux ins sel out) =
        Mux (patch (old, new) ins) sel (patch (old, new) out)

instance Var v => Locks (Mux v x) v where
    locks (Mux{}) = []

instance Label (Mux v x) where
    label (Mux{}) = "Mux"
instance Var v => Show (Mux v x) where
    show (Mux ins sel output) = "Mux " <> show ins <> " " <> show sel <> " = " <> show output

instance Var v => Function (Mux v x) v where
    inputs (Mux ins cond _) =
        S.unions $ map variables (ins ++ [cond])
    outputs (Mux _ _ output) = variables output

instance (Var v, Val x) => FunctionSimulation (Mux v x) v x where
    simulate cntx (Mux ins (I sel) (O outs)) =
        let
            selValue = getCntx cntx sel `mod` 16
            insCount = length ins
            selectedValue
                | selValue >= 0 && fromIntegral selValue < insCount =
                    case ins !! fromIntegral (selValue `mod` 16) of
                        I inputVar -> getCntx cntx inputVar
                | otherwise = 0
         in
            [(outVar, selectedValue) | outVar <- S.elems outs]

mux :: (Var v, Val x) => v -> v -> v -> [v] -> F v x
mux a b c d = packF $ Mux [I a, I b] (I c) $ O $ fromList d
