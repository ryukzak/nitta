{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

-- |В данном модуле описываются все функции доступные в системе. Функции (ранее функциональные
-- блоки) могут быть поддержаны вычислительными блоками в любых вариантах (связь многие ко многим).
-- Описание того, какие функции поддерживает конретный PU можно посмотреть в:
--
-- - bindToState (класс SerialPUState) для последовательных вычислительных узлов;
-- - bind (класс ProcessUnit) для остальных
--
-- [@Функция (функциональный блок)@] Оператор прикладного алгоритма. Может обладать внутренним
-- состояние (между циклами), подразумевать внутренний процесс.

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
    , addr2value
    -- *Input/Output
    , Receive(..), receive
    , Send(..), send
    -- *Simulation
    , reorderAlgorithm
    , simulateAlg
    , simulateAlgByCycle
    , get, get', set
    -- *Utils
    , castF
    ) where

import qualified Data.Bits         as B
import           Data.List         (cycle, intersect, (\\))
import qualified Data.Map          as M
import           Data.Maybe
import           Data.Set          (elems, fromList, union)
import qualified Data.String.Utils as S
import           Data.Typeable
import           NITTA.Types
import           NITTA.Utils



class ( Typeable a, Num a, Eq a, Ord a, Enum a, Show a, B.Bits a ) => Addr a
instance ( Typeable a, Num a, Eq a, Ord a, Enum a, Show a, B.Bits a ) => Addr a
addr2value addr = 0x1000 + fromIntegral addr -- must be coordinated with test bench initialization


get' cntx k = fromMaybe (error $ "Can't get from cntx." ++ show k) $ get cntx k
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
    values <- cntxInputs M.!? k
    value <- listToMaybe values
    let cntxInputs' = M.adjust tail k cntxInputs
    return (cntx{ cntxInputs=cntxInputs' }, value)

sendSim cntx@Cntx{ cntxOutputs } k v = do
    let cntxOutputs' = M.alter (Just . maybe [v] (v:)) k cntxOutputs
    return cntx{ cntxOutputs=cntxOutputs' }



-- |Симмулировать алгоритм.

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
            | let insideOuts = filter insideOut fs
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


castF :: ( Typeable f ) => F io -> Maybe (f io)
castF (F f) = cast f

----------------------------------------


data FramInput io = FramInput Int (O io) deriving ( Typeable )
deriving instance ( IOType io v x ) => Show (FramInput io)
deriving instance ( IOType io v x ) => Eq (FramInput io)
framInput addr vs = F $ FramInput addr $ O $ fromList vs

instance ( IOType io v x ) => Function (FramInput io) v where
    outputs (FramInput _ o) = variables o
    isCritical _ = True
instance ( Ord v, Num x ) => FunctionSimulation (FramInput (Parcel v x)) v x where
    -- |Невозможно симулировать данные операции без привязки их к конкретному PU, так как нет
    -- возможности понять что мы что-то записали по тому или иному адресу.
    simulate cntx (FramInput addr (O k)) = do
        let v = fromMaybe (addr2value addr) $ cntx `get` oneOf k
        set cntx k v



data FramOutput io = FramOutput Int (I io) deriving ( Typeable )
deriving instance ( IOType io v x ) => Show (FramOutput io)
deriving instance ( IOType io v x ) => Eq (FramOutput io)
framOutput addr v = F $ FramOutput addr $ I v

instance ( IOType io v x ) => Function (FramOutput io) v where
    inputs (FramOutput _ o) = variables o
    isCritical _ = True
instance ( Ord v ) => FunctionSimulation (FramOutput (Parcel v x)) v x where
    simulate cntx@Cntx{ cntxFram } (FramOutput addr (I k)) = do
        v <- get cntx k
        let cntxFram' = M.alter (Just . maybe [v] (v:)) (addr, k) cntxFram
        return cntx{ cntxFram=cntxFram' }



data Reg io = Reg (I io) (O io) deriving ( Typeable )
instance ( Show v ) => Show (Reg (Parcel v x)) where
    show (Reg (I k1) (O k2)) = S.join " = " (map show $ elems k2) ++ " = reg(" ++ show k1 ++ ")"
deriving instance ( IOType io v x ) => Eq (Reg io)
reg a b = F $ Reg (I a) (O $ fromList b)

instance ( IOType io v x ) => Function (Reg io) v where
    inputs  (Reg a _b) = variables a
    outputs (Reg _a b) = variables b
    dependency (Reg i o) = [ (b, a) | a <- elems $ variables i
                                    , b <- elems $ variables o
                                    ]
instance ( Ord v ) => FunctionSimulation (Reg (Parcel v x)) v x where
    simulate cntx (Reg (I k1) (O k2)) = do
        v <- cntx `get` k1
        set cntx k2 v




data Loop io = Loop (X io) (O io) (I io) deriving ( Typeable )
instance ( Show v, Show x ) => Show (Loop (Parcel v x)) where
    show (Loop (X x) (O k2) (I k1)) = show x ++ ", " ++ show k1 ++ " >>> " ++ S.join ", " (map show $ elems k2)
deriving instance ( IOType io v x ) => Eq (Loop io)
loop x a bs = F $ Loop (X x) (O $ fromList bs) $ I a

instance ( IOType io v x ) => Function (Loop io) v where
    inputs  (Loop _ _a b) = variables b
    outputs (Loop _ a _b) = variables a
    insideOut _ = True
instance ( Ord v, Show v, Show x ) => FunctionSimulation (Loop (Parcel v x)) v x where
    simulate cntx (Loop (X x) (O v2) (I v1)) = do
        let x' = fromMaybe x $ cntx `get` v1
        set cntx v2 x'



data Add io = Add (I io) (I io) (O io) deriving ( Typeable )
instance ( Show v ) => Show (Add (Parcel v x)) where
    show (Add (I k1) (I k2) (O k3)) = S.join " = " (map show $ elems k3) ++ " = " ++ show k1 ++ " + " ++ show k2
deriving instance ( IOType io v x ) => Eq (Add io)
add a b c = F $ Add (I a) (I b) $ O $ fromList c

instance ( IOType io v x ) => Function (Add io) v where
    inputs  (Add  a  b _c) = variables a `union` variables b
    outputs (Add _a _b  c) = variables c
    dependency (Add a b c) = [ (y, x) | x <- elems $ variables a `union` variables b
                                      , y <- elems $ variables c
                                      ]
instance ( Ord v, Num x ) => FunctionSimulation (Add (Parcel v x)) v x where
    simulate cntx (Add (I k1) (I k2) (O k3)) = do
        v1 <- cntx `get` k1
        v2 <- cntx `get` k2
        let v3 = v1 + v2
        set cntx k3 v3



data Sub io = Sub (I io) (I io) (O io) deriving ( Typeable )
instance ( Show v ) => Show (Sub (Parcel v x)) where
    show (Sub (I k1) (I k2) (O k3)) = S.join " = " (map show $ elems k3) ++ " = " ++ show k1 ++ " - " ++ show k2
deriving instance ( IOType io v x ) => Eq (Sub io)
sub a b c = F $ Sub (I a) (I b) $ O $ fromList c

instance ( IOType io v x ) => Function (Sub io) v where
    inputs  (Sub  a  b _c) = variables a `union` variables b
    outputs (Sub _a _b  c) = variables c
    dependency (Sub a b c) = [ (y, x) | x <- elems $ variables a `union` variables b
                                      , y <- elems $ variables c
                                      ]
instance ( Ord v, Num x ) => FunctionSimulation (Sub (Parcel v x)) v x where
    simulate cntx (Sub (I k1) (I k2) (O k3)) = do
        v1 <- cntx `get` k1
        v2 <- cntx `get` k2
        let v3 = v1 - v2
        set cntx k3 v3



data Multiply io = Multiply (I io) (I io) (O io) deriving ( Typeable )
instance ( Show v ) => Show (Multiply (Parcel v x)) where
    show (Multiply (I k1) (I k2) (O k3)) = S.join " = " (map show $ elems k3) ++ " = " ++ show k1 ++ " * " ++ show k2
deriving instance ( IOType io v x ) => Eq (Multiply io)
multiply a b c = F $ Multiply (I a) (I b) $ O $ fromList c

instance ( IOType io v x ) => Function (Multiply io) v where
    inputs  (Multiply  a  b _c) = variables a `union` variables b
    outputs (Multiply _a _b  c) = variables c
    dependency (Multiply a b c) = [ (y, x) | x <- elems $ variables a `union` variables b
                                  , y <- elems $ variables c
                                  ]
instance ( Ord v, Num x ) => FunctionSimulation (Multiply (Parcel v x)) v x where
    simulate cntx (Multiply (I k1) (I k2) (O k3)) = do
        v1 <- cntx `get` k1
        v2 <- cntx `get` k2
        let v3 = v1 * v2
        set cntx k3 v3



data Division io = Division
    { denom, numer     :: I io
    , quotient, remain :: O io
    } deriving ( Typeable )
instance ( Show v ) => Show (Division (Parcel v x)) where
    show (Division (I k1) (I k2) (O k3) (O k4))
        = S.join " = " (map show $ elems k3) ++ " = " ++ show k1 ++ " / " ++ show k2 ++ "; "
        ++ S.join " = " (map show $ elems k4) ++ " = " ++ show k1 ++ " mod " ++ show k2
deriving instance ( IOType io v x ) => Eq (Division io)
division d n q r = F Division
    { denom=I d
    , numer=I n
    , quotient=O $ fromList q
    , remain=O $ fromList r
    }

instance ( IOType io v x ) => Function (Division io) v where
    inputs  Division{ denom, numer } = variables denom `union` variables numer
    outputs Division{ quotient, remain } = variables quotient `union` variables remain
    dependency fb = [ (y, x) | x <- elems $ inputs fb
                             , y <- elems $ outputs fb
                             ]
instance ( Ord v, Num x, Integral x ) => FunctionSimulation (Division (Parcel v x)) v x where
    simulate cntx Division{ denom=I d, numer=I n, quotient=O q, remain=O r } = do
        v1 <- cntx `get` d
        v2 <- cntx `get` n
        let quotient' = fromIntegral v1 / fromIntegral v2 :: Double
        -- The rounding function is selected according to the mock behaviur.
        -- The IP-block have different behaviour.
        cntx' <- set cntx q $ ceiling quotient'
        let remain' = v1 `mod` v2
        set cntx' r remain'


data Constant io = Constant (X io) (O io) deriving ( Typeable )
instance ( Show v, Show x ) => Show (Constant (Parcel v x)) where
    show (Constant (X x) (O k)) = S.join " = " (map show $ elems k) ++ " = const(" ++ show x ++ ")"
deriving instance ( IOType io v x, Eq x ) => Eq (Constant io)
constant x vs = F $ Constant (X x) $ O $ fromList vs

instance ( IOType io v x, Show x, Eq x, Typeable x ) => Function (Constant io) v where
    outputs (Constant _ o) = variables o
instance ( Ord v ) => FunctionSimulation (Constant (Parcel v x)) v x where
    simulate cntx (Constant (X x) (O k))
        = set cntx k x



data ShiftLR io = ShiftL (I io) (O io)
                | ShiftR (I io) (O io)
                deriving ( Typeable )
instance ( Show v ) => Show (ShiftLR (Parcel v x)) where
    show (ShiftL (I k1) (O k2)) = S.join " = " (map show $ elems k2) ++ " = " ++ show k1 ++ " << 1"
    show (ShiftR (I k1) (O k2)) = S.join " = " (map show $ elems k2) ++ " = " ++ show k1 ++ " >> 1"
deriving instance ( IOType io v x ) => Eq (ShiftLR io)
shiftL a b = F $ ShiftL (I a) $ O $ fromList b
shiftR a b = F $ ShiftR (I a) $ O $ fromList b

instance ( IOType io v x ) => Function (ShiftLR io) v where
    outputs (ShiftL i o) = variables i `union` variables o
    outputs (ShiftR i o) = variables i `union` variables o
instance ( Ord v, B.Bits x ) => FunctionSimulation (ShiftLR (Parcel v x)) v x where
    simulate cntx (ShiftL (I k1) (O k2)) = do
        v1 <- cntx `get` k1
        let v2 = v1 `B.shiftL` 1
        set cntx k2 v2
    simulate cntx (ShiftR (I k1) (O k2)) = do
        v1 <- cntx `get` k1
        let v2 = v1 `B.shiftR` 1
        set cntx k2 v2



newtype Send io = Send (I io) deriving ( Typeable )
send a = F $ Send $ I a

deriving instance ( IOType io v x ) => Show (Send io)
deriving instance ( IOType io v x ) => Eq (Send io)
instance ( IOType io v x ) => Function (Send io) v where
    inputs (Send i) = variables i
instance ( Ord v ) => FunctionSimulation (Send (Parcel v x)) v x where
    simulate cntx (Send (I k)) = do
        v <- cntx `get` k
        sendSim cntx k v



newtype Receive io = Receive (O io) deriving ( Typeable )
receive a = F $ Receive $ O $ fromList a

deriving instance ( IOType io v x ) => Show (Receive io)
deriving instance ( IOType io v x ) => Eq (Receive io)
instance ( IOType io v x ) => Function (Receive io) v where
    outputs (Receive o) = variables o
instance ( Ord v ) => FunctionSimulation (Receive (Parcel v x)) v x where
    simulate cntx (Receive (O ks)) = do
        let k = oneOf ks
        (cntx', v) <- cntx `receiveSim` k
        set cntx' ks v
