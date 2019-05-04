{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures -fno-warn-orphans #-}

{-|
Module      : NITTA.Functions
Description : Functions for an application algorithm
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

В данном модуле описываются все функции доступные в системе. Функции (ранее
функциональные блоки) могут быть поддержаны вычислительными блоками в любых
вариантах (связь многие ко многим). Описание того, какие функции поддерживает
конретный PU можно посмотреть в:

- bindToState (класс SerialPUState) для последовательных вычислительных узлов;
- bind (класс ProcessorUnit) для остальных.

[@Функция (функциональный блок)@] Оператор прикладного алгоритма. Может обладать
внутренним состояние (между циклами), подразумевать внутренний процесс.
-}
module NITTA.Functions
    ( -- *Arithmetics
      Add(..), add
    , Division(..), division
    , Multiply(..), multiply
    , ShiftLR(..), shiftL, shiftR
    , Sub(..), sub
    -- *Memory
    , Constant(..), constant
    , FramInput(..), framInput
    , FramOutput(..), framOutput
    , Loop(..), loop
    , Reg(..), reg
    -- *Input/Output
    , Receive(..), receive
    , Send(..), send
    -- *Simulation
    , funSim
    , reorderAlgorithm
    , simulateAlg
    , simulateAlgByCycle
    , get, get', set
    -- *Utils
    , castF
    ) where

import qualified Data.Bits         as B
import           Data.Default
import           Data.List         (cycle, intersect, (\\))
import qualified Data.Map          as M
import           Data.Maybe
import           Data.Set          (elems, fromList, union)
import qualified Data.String.Utils as S
import           Data.Typeable
import           NITTA.Types
import           NITTA.Types.VisJS
import           NITTA.Utils


-----------------------------------------------------------
-- *VizJS

instance ( Function (f v x) v, Label (f v x), Var v ) => ToVizJS (f v x) where
    toVizJS f = GraphStructure
        { nodes=[ NodeElement 1 $ box "#cbbeb5" $ label f ]
        , edges=mkEdges InVertex (inputs f) ++ mkEdges OutVertex (outputs f)
        }
        where
            mkEdges t = map ( \v -> GraphVertex t (label v) 1 ) . elems

instance {-# OVERLAPS #-} ToVizJS (F v x) where
    toVizJS (F f) = toVizJS f

instance {-# OVERLAPS #-} ( Var v ) => ToVizJS (Loop v x) where
    toVizJS (Loop _ (O a) (I b))
        = GraphStructure
            { nodes=
                [ NodeElement 1 $ box "#6dc066" $ "prev: " ++ label b
                , NodeElement 2 $ ellipse  "#fa8072" $ "throw: " ++ label b
                ]
            , edges=GraphVertex InVertex (label b) 2
                :   map (\c -> GraphVertex OutVertex (label c) 1) (elems a)
            }

box     name color = NodeParam{ nodeName=name, nodeColor=color, nodeShape="box",     fontSize="20", nodeSize="30" }
ellipse name color = NodeParam{ nodeName=name, nodeColor=color, nodeShape="ellipse", fontSize="20", nodeSize="30" }


-----------------------------------------------------------


get' cntx k = fromMaybe (error $ "Can't get from cntx: " ++ show k ++ " cntx: " ++ show cntx) $ get cntx k
get Cntx{ cntxVars } k = do
    values <- cntxVars M.!? k
    case values of
        []      -> Nothing
        value:_ -> Just value

-- |@set@ позволяет обновить в контексте состояние группы выходных переменных (пример: @O vs@).
--
-- @cntx@ - контекст вычислительного процесса со значениями переменных. @ks@ - список переменных в
-- словаре, для которых нужно обновить знаение. @val@ - новое значение переменной.
set cntx@Cntx{ cntxVars } ks val = do
    let cntxVars' = foldl (flip $ M.alter (Just . maybe [val] (val:))) cntxVars ks
    return cntx{ cntxVars=cntxVars' }

receiveSim cntx@Cntx{ cntxInputs } k = do
    value : values <- cntxInputs M.!? k
    let cntxInputs' = M.insert k values cntxInputs
    return (cntx{ cntxInputs=cntxInputs' }, value)

sendSim cntx@Cntx{ cntxOutputs } k v = do
    let cntxOutputs' = M.alter (Just . maybe [v] (v:)) k cntxOutputs
    return cntx{ cntxOutputs=cntxOutputs' }

inputsLockOutputs f =
    [ Lock{ locked=y, lockBy=x }
    | x <- elems $ inputs f
    , y <- elems $ outputs f
    ]


-- |Симмулировать алгоритм.

funSim n cntx alg
    = let cntxs = simulateAlgByCycle cntx alg
    in mapM_ (putStrLn . ("---------------------\n"++) . filter (/= '"') . show) $ take n cntxs

-- FIXME: Заменить симуляцию [Function] на DataFlowGraph в simulateAlg и simulateAlgByCycle. В случае
-- "расщеплённого времени" останавливаться с ошибкой.
simulateAlg cntx0 fbs = step cntx0 $ cycle $ reorderAlgorithm fbs
    where
        step cntx (f:fs)
            | Just cntx' <- simulate cntx f
            = cntx' : step cntx' fs
            | otherwise = error $ "Simulation error: " ++ show f
        step _ _ = error "Simulation error."

simulateAlgByCycle cntx fbs = simulateAlgByCycle' $ simulateAlg cntx fbs
    where
        l = length fbs - 1
        simulateAlgByCycle' xs
            = let x : xs' = drop l xs
            in x : simulateAlgByCycle' xs'


reorderAlgorithm alg = orderAlgorithm' [] alg
     where
        orderAlgorithm' _ [] = []
        orderAlgorithm' vs fs
            | let insideOuts = filter isBreakLoop fs
            , not $ null insideOuts
            , let insideOutsOutputs = elems $ unionsMap outputs insideOuts
            = case filter (not . null . intersect insideOutsOutputs . elems . inputs) insideOuts of
                [] -> insideOuts ++ orderAlgorithm' (elems (unionsMap variables insideOuts) ++ vs) (fs \\ insideOuts)
                ready -> ready ++ orderAlgorithm' (elems (unionsMap variables ready) ++ vs) (fs \\ ready)
        orderAlgorithm' vs fs
            | let ready = filter (null . (\\ vs) . elems . inputs) fs
            , not $ null ready
            = ready ++ orderAlgorithm' (elems (unionsMap variables ready) ++ vs) (fs \\ ready)
        orderAlgorithm' _ _ = error "Can't sort algorithm."


castF :: ( Typeable f, Typeable v, Typeable x ) => F v x -> Maybe (f v x)
castF (F f) = cast f

----------------------------------------


data FramInput v x = FramInput Int (O v) deriving ( Typeable, Eq, Show )
framInput addr vs = F $ FramInput addr $ O $ fromList vs

instance ( Ord v ) => Function (FramInput v x) v where
    outputs (FramInput _ o) = variables o
    isInternalLockPossible _ = True
instance ( Ord v ) => Patch (FramInput v x) (v, v) where
    patch diff (FramInput x a) = FramInput x $ patch diff a
instance Locks (FramInput v x) v where locks _ = []
instance ( Ord v, Default x ) => FunctionSimulation (FramInput v x) v x where
    -- |Невозможно симулировать данные операции без привязки их к конкретному PU, так как нет
    -- возможности понять что мы что-то записали по тому или иному адресу.
    simulate cntx (FramInput _addr (O k)) = do
        -- FIXME: change def value to cntx value
        let v = fromMaybe def $ cntx `get` oneOf k
        set cntx k v



data FramOutput v x = FramOutput Int (I v) deriving ( Typeable, Eq, Show )
framOutput addr v = F $ FramOutput addr $ I v

instance ( Ord v ) => Function (FramOutput v x) v where
    inputs (FramOutput _ o) = variables o
    isInternalLockPossible _ = True
instance ( Ord v ) => Patch (FramOutput v x) (v, v) where
    patch diff (FramOutput x a) = FramOutput x $ patch diff a
instance Locks (FramOutput v x) v where locks _ = []
instance ( Ord v ) => FunctionSimulation (FramOutput v x) v x where
    simulate cntx@Cntx{ cntxFram } (FramOutput addr (I k)) = do
        v <- get cntx k
        let cntxFram' = M.alter (Just . maybe [v] (v:)) (addr, k) cntxFram
        return cntx{ cntxFram=cntxFram' }



data Reg v x = Reg (I v) (O v) deriving ( Typeable, Eq )
instance {-# OVERLAPS #-} Label (Reg v x) where label Reg{} = "r"
instance ( Show v ) => Show (Reg v x) where
    show (Reg (I k1) (O k2)) = S.join " = " (map show $ elems k2) ++ " = reg(" ++ show k1 ++ ")"
reg a b = F $ Reg (I a) (O $ fromList b)

instance ( Ord v ) => Function (Reg v x) v where
    inputs  (Reg a _b) = variables a
    outputs (Reg _a b) = variables b
instance ( Ord v ) => Patch (Reg v x) (v, v) where
    patch diff (Reg a b) = Reg (patch diff a) (patch diff b)
instance ( Ord v ) => Locks (Reg v x) v where
    locks = inputsLockOutputs
instance ( Ord v ) => FunctionSimulation (Reg v x) v x where
    simulate cntx (Reg (I k1) (O k2)) = do
        v <- cntx `get` k1
        set cntx k2 v




data Loop v x = Loop (X x) (O v) (I v) deriving ( Typeable, Eq )
instance {-# OVERLAPS #-} ( Show x, Label v ) => Label (Loop v x) where
    label (Loop (X x) _ (I b)) = show x ++ "->" ++ label b
instance ( Show v, Show x ) => Show (Loop v x) where
    show (Loop (X x) (O k2) (I k1)) = show x ++ ", " ++ show k1 ++ " >>> " ++ S.join ", " (map show $ elems k2)
loop x a bs = F $ Loop (X x) (O $ fromList bs) $ I a

instance ( Ord v ) => Function (Loop v x) v where
    inputs  (Loop _ _a b) = variables b
    outputs (Loop _ a _b) = variables a
    isBreakLoop _ = True
instance ( Ord v ) => Patch (Loop v x) (v, v) where
    patch diff (Loop x a b) = Loop x (patch diff a) (patch diff b)
instance Locks (Loop v x) v where
    -- locks (Loop _ (O as) (I b)) = map (\a -> Lock{ locked=a, lockBy=b }) $ elems as
    locks _ = []
instance ( Ord v ) => FunctionSimulation (Loop v x) v x where
    simulate cntx (Loop (X x) (O v2) (I v1)) = do
        let x' = fromMaybe x $ cntx `get` v1
        set cntx v2 x'



data Add v x = Add (I v) (I v) (O v) deriving ( Typeable, Eq )
instance {-# OVERLAPS #-} Label (Add v x) where label Add{} = "+"
instance ( Show v ) => Show (Add v x) where
    show (Add (I k1) (I k2) (O k3)) = S.join " = " (map show $ elems k3) ++ " = " ++ show k1 ++ " + " ++ show k2
add a b c = F $ Add (I a) (I b) $ O $ fromList c

instance ( Ord v ) => Function (Add v x) v where
    inputs  (Add  a  b _c) = variables a `union` variables b
    outputs (Add _a _b  c) = variables c
instance ( Ord v ) => Patch (Add v x) (v, v) where
    patch diff (Add a b c) = Add (patch diff a) (patch diff b) (patch diff c)
instance ( Ord v ) => Locks (Add v x) v where
    locks = inputsLockOutputs
instance ( Ord v, Num x ) => FunctionSimulation (Add v x) v x where
    simulate cntx (Add (I k1) (I k2) (O k3)) = do
        v1 <- cntx `get` k1
        v2 <- cntx `get` k2
        let v3 = v1 + v2
        set cntx k3 v3



data Sub v x = Sub (I v) (I v) (O v) deriving ( Typeable, Eq )
instance {-# OVERLAPS #-} Label (Sub v x) where label Sub{} = "-"
instance ( Show v ) => Show (Sub v x) where
    show (Sub (I k1) (I k2) (O k3)) = S.join " = " (map show $ elems k3) ++ " = " ++ show k1 ++ " - " ++ show k2
sub a b c = F $ Sub (I a) (I b) $ O $ fromList c

instance ( Ord v ) => Function (Sub v x) v where
    inputs  (Sub  a  b _c) = variables a `union` variables b
    outputs (Sub _a _b  c) = variables c
instance ( Ord v ) => Patch (Sub v x) (v, v) where
    patch diff (Sub a b c) = Sub (patch diff a) (patch diff b) (patch diff c)
instance ( Ord v ) => Locks (Sub v x) v where
    locks = inputsLockOutputs
instance ( Ord v, Num x ) => FunctionSimulation (Sub v x) v x where
    simulate cntx (Sub (I k1) (I k2) (O k3)) = do
        v1 <- cntx `get` k1
        v2 <- cntx `get` k2
        let v3 = v1 - v2
        set cntx k3 v3



data Multiply v x = Multiply (I v) (I v) (O v) deriving ( Typeable, Eq )
instance {-# OVERLAPS #-} Label (Multiply v x) where label Multiply{} = "*"
instance ( Show v ) => Show (Multiply v x) where
    show (Multiply (I k1) (I k2) (O k3)) = S.join " = " (map show $ elems k3) ++ " = " ++ show k1 ++ " * " ++ show k2
multiply a b c = F $ Multiply (I a) (I b) $ O $ fromList c

instance ( Ord v ) => Function (Multiply v x) v where
    inputs  (Multiply  a  b _c) = variables a `union` variables b
    outputs (Multiply _a _b  c) = variables c
instance ( Ord v ) => Patch (Multiply v x) (v, v) where
    patch diff (Multiply a b c) = Multiply (patch diff a) (patch diff b) (patch diff c)
instance ( Ord v ) => Locks (Multiply v x) v where
    locks = inputsLockOutputs
instance ( Ord v, Num x ) => FunctionSimulation (Multiply v x) v x where
    simulate cntx (Multiply (I k1) (I k2) (O k3)) = do
        v1 <- cntx `get` k1
        v2 <- cntx `get` k2
        let v3 = v1 * v2
        set cntx k3 v3



data Division v x = Division
    { denom, numer     :: I v
    , quotient, remain :: O v
    } deriving ( Typeable, Eq )
instance {-# OVERLAPS #-} Label (Division v x) where label Division{} = "/"
instance ( Show v ) => Show (Division v x) where
    show (Division (I k1) (I k2) (O k3) (O k4))
        =  S.join " = " (map show $ elems k3) ++ " = " ++ show k1 ++ " / " ++ show k2 ++ "; "
        ++ S.join " = " (if null k4 then ["_"] else map show $ elems k4) ++ " = " ++ show k1 ++ " `mod` " ++ show k2
division d n q r = F Division
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
instance ( Ord v ) => Locks (Division v x) v where
    locks = inputsLockOutputs
instance ( Ord v, Integral x ) => FunctionSimulation (Division v x) v x where
    simulate cntx Division{ denom=I d, numer=I n, quotient=O q, remain=O r } = do
        v1 <- cntx `get` d
        v2 <- cntx `get` n
        let (q', r') = v1 `quotRem` v2
        cntx' <- set cntx q q'
        set cntx' r r'


data Constant v x = Constant (X x) (O v) deriving ( Typeable, Eq )
instance {-# OVERLAPS #-} ( Show x ) => Label (Constant v x) where label (Constant (X x) _) = show x
instance ( Show v, Show x ) => Show (Constant v x) where
    show (Constant (X x) (O k)) = S.join " = " (map show $ elems k) ++ " = const(" ++ show x ++ ")"
constant x vs = F $ Constant (X x) $ O $ fromList vs

instance ( Show x, Eq x, Typeable x ) => Function (Constant v x) v where
    outputs (Constant _ o) = variables o
instance ( Ord v ) => Patch (Constant v x) (v, v) where
    patch diff (Constant x a) = Constant x (patch diff a)
instance Locks (Constant v x) v where locks _ = []
instance ( Ord v ) => FunctionSimulation (Constant v x) v x where
    simulate cntx (Constant (X x) (O k))
        = set cntx k x



-- FIXME: just fixme
data ShiftLR v x = ShiftL (I v) (O v)
                 | ShiftR (I v) (O v)
                deriving ( Typeable, Eq )
instance ( Show v ) => Show (ShiftLR v x) where
    show (ShiftL (I k1) (O k2)) = S.join " = " (map show $ elems k2) ++ " = " ++ show k1 ++ " << 1"
    show (ShiftR (I k1) (O k2)) = S.join " = " (map show $ elems k2) ++ " = " ++ show k1 ++ " >> 1"
shiftL a b = F $ ShiftL (I a) $ O $ fromList b
shiftR a b = F $ ShiftR (I a) $ O $ fromList b

instance ( Ord v ) => Function (ShiftLR v x) v where
    outputs (ShiftL i o) = variables i `union` variables o
    outputs (ShiftR i o) = variables i `union` variables o
instance ( Ord v ) => Patch (ShiftLR v x) (v, v) where
    patch diff (ShiftL a b) = ShiftL (patch diff a) (patch diff b)
    patch diff (ShiftR a b) = ShiftR (patch diff a) (patch diff b)
instance ( Ord v ) => Locks (ShiftLR v x) v where
    locks = inputsLockOutputs
instance ( Ord v, B.Bits x ) => FunctionSimulation (ShiftLR v x) v x where
    simulate cntx (ShiftL (I k1) (O k2)) = do
        v1 <- cntx `get` k1
        let v2 = v1 `B.shiftL` 1
        set cntx k2 v2
    simulate cntx (ShiftR (I k1) (O k2)) = do
        v1 <- cntx `get` k1
        let v2 = v1 `B.shiftR` 1
        set cntx k2 v2



newtype Send v x = Send (I v) deriving ( Typeable, Eq, Show )
instance {-# OVERLAPS #-} Label (Send v x) where label Send{} = "send"
send a = F $ Send $ I a
instance ( Ord v ) => Function (Send v x) v where
    inputs (Send i) = variables i
instance ( Ord v ) => Patch (Send v x) (v, v) where
    patch diff (Send a) = Send (patch diff a)
instance Locks (Send v x) v where locks _ = []
instance ( Ord v ) => FunctionSimulation (Send v x) v x where
    simulate cntx (Send (I k)) = do
        v <- cntx `get` k
        sendSim cntx k v



newtype Receive v x = Receive (O v) deriving ( Typeable, Eq, Show )
instance {-# OVERLAPS #-} Label (Receive v x) where label Receive{} = "receive"
receive a = F $ Receive $ O $ fromList a
instance ( Ord v ) => Function (Receive v x) v where
    outputs (Receive o) = variables o
instance ( Ord v ) => Patch (Receive v x) (v, v) where
    patch diff (Receive a) = Receive (patch diff a)
instance Locks (Receive v x) v where locks _ = []
instance ( Ord v, Default x ) => FunctionSimulation (Receive v x) v x where
    simulate cntx (Receive (O ks)) = do
        let k = oneOf ks
        let (cntx', v) = fromMaybe (cntx, def)  $ cntx `receiveSim` k
        set cntx' ks v
