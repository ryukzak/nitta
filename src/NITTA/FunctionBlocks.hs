{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

-- | В данном модуле описываются все функциональные блоки доступные в системе. Функциональные блоки
-- могут быть поддержаны вычислительными блоками в любых вариантах (связь многие ко многим).
-- Описание того, какие функциональные блоки поддерживает конретный PU можно посмотреть в:
--
-- - bindToState (класс SerialPUState) для последовательных вычислительных узлов;
-- - bind (класс ProcessUnit) для остальных
--
-- [@Функциональный блок@] Оператор прикладного алгоритма. Может обладать внутренним состояние
-- (между циклами), подразумевать внутренний процесс.


module NITTA.FunctionBlocks where

import qualified Data.Bits     as B
import           Data.List     (cycle, intersect, (\\))
import qualified Data.Map      as M
import           Data.Maybe
import           Data.Set      (elems, fromList, union)
import           Data.Typeable
import           NITTA.Types
import           NITTA.Utils



class ( Typeable a, Num a, Eq a, Ord a, Enum a, Show a, B.Bits a ) => Addr a
instance ( Typeable a, Num a, Eq a, Ord a, Enum a, Show a, B.Bits a ) => Addr a
addr2value addr = 0x1000 + fromIntegral addr -- must be coordinated with test bench initialization


get' cntx k = fromMaybe (error $ "Can't get from cntx." ++ show k) $ get cntx k
get Cntx{..} k = do
  values <- cntxVars M.!? k
  case values of
    []      -> Nothing
    value:_ -> Just value

-- | Функция set - позволяет обновить состояние всех выходных переменных одного порта (одно
-- значение на выходе функционального блока может быть считано много раз) указанным значением.
--
-- cntx - контекст, имеет тип: FunSimCntx. Это словарь со значениями переменных, входов, выходов.
-- ks - список ключей в словаре, для которых нужно обновить знаение.
-- v - новое значение вызодных переменных, которое необходимо записать для всех ключей.
set cntx@Cntx{..} ks v = do
  let cntxVars' = foldl (flip $ M.alter (Just . maybe [v] (v:))) cntxVars ks
  return cntx{ cntxVars=cntxVars' }
set' cntx ks v = fromMaybe (error "Can't set in cntx.") $ set cntx ks v

receiveSim cntx@Cntx{..} k = do
  values <- cntxInputs M.!? k
  value <- listToMaybe values
  let cntxInputs' = M.adjust tail k cntxInputs
  return (cntx{ cntxInputs=cntxInputs' }, value)

sendSim cntx@Cntx{..} k v = do
  let cntxOutputs' = M.alter (Just . maybe [v] (v:)) k cntxOutputs
  return cntx{ cntxOutputs=cntxOutputs' }



-- | Функция set - позволяет обновить состояние всех выходных переменных одного порта (одно
-- значение на выходе функционального блока может быть считано много раз) указанным значением.
--
-- cntx - контекст, имеет тип: FunSimCntx. Это словарь со значениями переменных, входов, выходов.
-- ks - список ключей в словаре, для которых нужно обновить знаение.
-- v - новое значение вызодных переменных, которое необходимо записать для всех ключей.
push ks v cntx
  = return $ foldl (flip $ M.alter (Just . maybe [v] (v:))) cntx ks


-- | Симмулировать алгоритм.
--
-- TODO: simulate DFG
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
    simulateAlgByCycle' xs = let x : xs' = drop l xs
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
    orderAlgorithm' _ _ = error $ "Can't sort algorithm. "


castFB :: ( Typeable fb ) => FB io -> Maybe (fb io)
castFB (FB fb) = cast fb

----------------------------------------


data FramInput io = FramInput Int (O io) deriving ( Typeable )
deriving instance ( IOType io v x ) => Show (FramInput io)
deriving instance ( IOType io v x ) => Eq (FramInput io)
framInput addr vs = FB $ FramInput addr $ O $ fromList vs

instance ( IOType io v x ) => FunctionalBlock (FramInput io) v where
  outputs (FramInput _ o) = variables o
  isCritical _ = True
instance ( Ord v, Num x ) => FunctionSimulation (FramInput (Parcel v x)) v x where
  -- | Невозможно симулировать данные операции без привязки их к конкретному PU, так как нет
  -- возможности понять что мы что-то записали по тому или иному адресу.
  simulate cntx (FramInput addr (O k)) = do
    let v = fromMaybe (addr2value addr) $ cntx `get` oneOf k
    set cntx k v



data FramOutput io = FramOutput Int (I io) deriving ( Typeable )
deriving instance ( IOType io v x ) => Show (FramOutput io)
deriving instance ( IOType io v x ) => Eq (FramOutput io)
framOutput addr v = FB $ FramOutput addr $ I v

instance ( IOType io v x ) => FunctionalBlock (FramOutput io) v where
  inputs (FramOutput _ o) = variables o
  isCritical _ = True
instance ( Ord v ) => FunctionSimulation (FramOutput (Parcel v x)) v x where
  simulate cntx@Cntx{ cntxFram } (FramOutput addr (I k)) = do
    v <- get cntx k
    let cntxFram' = M.alter (Just . maybe [v] (v:)) (addr, k) cntxFram
    return cntx{ cntxFram=cntxFram' }



data Reg io = Reg (I io) (O io) deriving ( Typeable )
deriving instance ( IOType io v x ) => Show (Reg io)
deriving instance ( IOType io v x ) => Eq (Reg io)
reg a b = FB $ Reg (I a) (O $ fromList b)

instance ( IOType io v x ) => FunctionalBlock (Reg io) v where
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
deriving instance ( IOType io v x ) => Show (Loop io)
deriving instance ( IOType io v x ) => Eq (Loop io)
loop x bs a = FB $ Loop (X x) (O $ fromList bs) $ I a

instance ( IOType io v x ) => FunctionalBlock (Loop io) v where
  inputs  (Loop _ _a b) = variables b
  outputs (Loop _ a _b) = variables a
  insideOut _ = True
instance ( Ord v, Show v, Show x ) => FunctionSimulation (Loop (Parcel v x)) v x where
  simulate cntx (Loop (X x) (O v2) (I v1)) = do
    let x' = fromMaybe x $ cntx `get` v1
    set cntx v2 x'



data Add io = Add (I io) (I io) (O io) deriving ( Typeable )
deriving instance ( IOType io v x ) => Show (Add io)
deriving instance ( IOType io v x ) => Eq (Add io)
add a b c = FB $ Add (I a) (I b) $ O $ fromList c

instance ( IOType io v x ) => FunctionalBlock (Add io) v where
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
deriving instance ( IOType io v x ) => Show (Sub io)
deriving instance ( IOType io v x ) => Eq (Sub io)
sub a b c = FB $ Sub (I a) (I b) $ O $ fromList c

instance ( IOType io v x ) => FunctionalBlock (Sub io) v where
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
deriving instance ( IOType io v x ) => Show (Multiply io)
deriving instance ( IOType io v x ) => Eq (Multiply io)
multiply a b c = FB $ Multiply (I a) (I b) $ O $ fromList c

instance ( IOType io v x ) => FunctionalBlock (Multiply io) v where
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
deriving instance ( IOType io v x ) => Show (Division io)
deriving instance ( IOType io v x ) => Eq (Division io)
division d n q r = FB Division
  { denom=I d
  , numer=I n
  , quotient=O $ fromList q
  , remain=O $ fromList r
  }

instance ( IOType io v x ) => FunctionalBlock (Division io) v where
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
deriving instance ( IOType io v x, Show x ) => Show (Constant io)
deriving instance ( IOType io v x, Eq x ) => Eq (Constant io)
constant x vs = FB $ Constant (X x) $ O $ fromList vs

instance ( IOType io v x, Show x, Eq x, Typeable x ) => FunctionalBlock (Constant io) v where
  outputs (Constant _ o) = variables o
instance ( Ord v ) => FunctionSimulation (Constant (Parcel v x)) v x where
  simulate cntx (Constant (X x) (O k))
    = set cntx k x



data ShiftLR io = ShiftL (I io) (O io)
                | ShiftR (I io) (O io)
                deriving ( Typeable )
deriving instance ( IOType io v x ) => Show (ShiftLR io)
deriving instance ( IOType io v x ) => Eq (ShiftLR io)
shiftL a b = FB $ ShiftL (I a) $ O $ fromList b
shiftR a b = FB $ ShiftR (I a) $ O $ fromList b

instance ( IOType io v x ) => FunctionalBlock (ShiftLR io) v where
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
send a = FB $ Send $ I a

deriving instance ( IOType io v x ) => Show (Send io)
deriving instance ( IOType io v x ) => Eq (Send io)
instance ( IOType io v x ) => FunctionalBlock (Send io) v where
  inputs (Send i) = variables i
instance ( Ord v ) => FunctionSimulation (Send (Parcel v x)) v x where
  simulate cntx (Send (I k)) = do
    v <- cntx `get` k
    sendSim cntx k v



newtype Receive io = Receive (O io) deriving ( Typeable )
receive a = FB $ Receive $ O $ fromList a

deriving instance ( IOType io v x ) => Show (Receive io)
deriving instance ( IOType io v x ) => Eq (Receive io)
instance ( IOType io v x ) => FunctionalBlock (Receive io) v where
  outputs (Receive o) = variables o
instance ( Ord v ) => FunctionSimulation (Receive (Parcel v x)) v x where
  simulate cntx (Receive (O ks)) = do
    let k = oneOf ks
    (cntx', v) <- cntx `receiveSim` k
    set cntx' ks v
