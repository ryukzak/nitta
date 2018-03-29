{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
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

module NITTA.FunctionBlocks where

import           Data.Bits
import           Data.List     (find)
import qualified Data.Map      as M
import           Data.Maybe
import           Data.Typeable
import           NITTA.Types



class ( Typeable a, Num a, Eq a, Ord a, Enum a, Show a, Bits a ) => Addr a
instance ( Typeable a, Num a, Eq a, Ord a, Enum a, Show a, Bits a ) => Addr a

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

receive cntx@Cntx{..} k = do
  values <- cntxInputs M.!? k
  value <- listToMaybe values
  let cntxInputs' = M.adjust tail k cntxInputs
  return (cntx{ cntxInputs=cntxInputs' }, value)

send cntx@Cntx{..} k v = do
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
simulateAlg cntx0 fbs
  = inner cntx0 [] $ sort cntx0 fbs
  where
    step cntx fs
      = let err = error "Can't simulate algorithm, because can't define execution sequence or recived data is over."
            f = fromMaybe err $ find (isJust . simulate cntx) fs
            fs' = filter (/= f) fs
            Just cntx' = simulate cntx f
        in (f, fs', cntx')

    sort _ [] = []
    sort cntx fs = let (f, fs', cntx') = step cntx fs
                   in f : sort cntx' fs'

    inner cntx [] fs0 = cntx : inner cntx fs0 fs0
    inner cntx fs fs0 = let (_f, fs', cntx') = step cntx fs
                        in inner cntx' fs' fs0



castFB :: ( Typeable fb ) => FB io -> Maybe (fb io)
castFB (FB fb) = cast fb

----------------------------------------


data FramInput io = FramInput Int (O io) deriving ( Typeable )
deriving instance ( IOType io v x ) => Show (FramInput io)
deriving instance ( IOType io v x ) => Eq (FramInput io)
framInput addr vs = FB $ FramInput addr vs

instance ( IOType io v x ) => FunctionalBlock (FramInput io) v where
  outputs (FramInput _ o) = variables o
  isCritical _ = True
instance FunctionSimulation (FramInput (Parcel v x)) v x where
  -- | Невозможно симулировать данные операции без привязки их к конкретному PU, так как нет
  -- возможности понять что мы что-то записали по тому или иному адресу.
  simulate = error "Can't functional simulate FramInput!"



data FramOutput io = FramOutput Int (I io) deriving ( Typeable )
deriving instance ( IOType io v x ) => Show (FramOutput io)
deriving instance ( IOType io v x ) => Eq (FramOutput io)
framOutput addr v = FB $ FramOutput addr v

instance ( IOType io v x ) => FunctionalBlock (FramOutput io) v where
  inputs (FramOutput _ o) = variables o
  isCritical _ = True
instance FunctionSimulation (FramOutput (Parcel v x)) v x where
  simulate = error "Can't functional simulate FramOutput!"



data Reg io = Reg (I io) (O io) deriving ( Typeable )
deriving instance ( IOType io v x ) => Show (Reg io)
deriving instance ( IOType io v x ) => Eq (Reg io)
reg a b = FB $ Reg a b

instance ( IOType io v x ) => FunctionalBlock (Reg io) v where
  inputs  (Reg a _b) = variables a
  outputs (Reg _a b) = variables b
  dependency (Reg i o) = [ (b, a) | a <- variables i
                                  , b <- variables o
                                  ]
instance ( Ord v ) => FunctionSimulation (Reg (Parcel v x)) v x where
  simulate cntx (Reg (I k1) (O k2)) = do
    v <- cntx `get` k1
    set cntx k2 v



data Loop io = Loop (X io) (O io) (I io) deriving ( Typeable )
deriving instance ( IOType io v x ) => Show (Loop io)
deriving instance ( IOType io v x ) => Eq (Loop io)
loop x bs a = FB $ Loop x bs a

instance ( IOType io v x ) => FunctionalBlock (Loop io) v where
  inputs  (Loop _ _a b) = variables b
  outputs (Loop _ a _b) = variables a
  insideOut _ = True
instance ( Ord v ) => FunctionSimulation (Loop (Parcel v x)) v x where
  simulate cntx (Loop _ (O k1) (I k2)) = do
    let (cntx', v) = fromMaybe (cntx, fromMaybe undefined $ cntx `get` k2) $ cntx `receive` k2
    set cntx' k1 v



data Add io = Add (I io) (I io) (O io) deriving ( Typeable )
deriving instance ( IOType io v x ) => Show (Add io)
deriving instance ( IOType io v x ) => Eq (Add io)

instance ( IOType io v x ) => FunctionalBlock (Add io) v where
  inputs  (Add  a  b _c) = variables a ++ variables b
  outputs (Add _a _b  c) = variables c
  dependency (Add a b c) = [ (y, x) | x <- variables a ++ variables b
                                    , y <- variables c
                                    ]
instance ( Ord v, Num x ) => FunctionSimulation (Add (Parcel v x)) v x where
  simulate cntx (Add (I k1) (I k2) (O k3)) = do
    v1 <- cntx `get` k1
    v2 <- cntx `get` k2
    let v3 = v1 + v2
    set cntx k3 v3



data Constant io = Constant (X io) (O io) deriving ( Typeable )
deriving instance ( IOType io v x, Show x ) => Show (Constant io)
deriving instance ( IOType io v x, Eq x ) => Eq (Constant io)

instance ( IOType io v x, Show x, Eq x, Typeable x ) => FunctionalBlock (Constant io) v where
  outputs (Constant _ o) = variables o
instance ( Ord v ) => FunctionSimulation (Constant (Parcel v x)) v x where
  simulate cntx (Constant (X x) (O k))
    = set cntx k x



data ShiftL io = ShiftL (I io) (O io) deriving ( Typeable )
deriving instance ( IOType io v x ) => Show (ShiftL io)
deriving instance ( IOType io v x ) => Eq (ShiftL io)

instance ( IOType io v x ) => FunctionalBlock (ShiftL io) v where
  outputs (ShiftL i o) = variables i ++ variables o
instance ( Ord v, Bits x ) => FunctionSimulation (ShiftL (Parcel v x)) v x where
  simulate cntx (ShiftL (I k1) (O k2)) = do
    v1 <- cntx `get` k1
    let v2 = v1 `shiftR` 1
    set cntx k2 v2



newtype Send io = Send (I io) deriving ( Typeable )
deriving instance ( IOType io v x ) => Show (Send io)
deriving instance ( IOType io v x ) => Eq (Send io)
instance ( IOType io v x ) => FunctionalBlock (Send io) v where
  inputs (Send i) = variables i
instance ( Ord v ) => FunctionSimulation (Send (Parcel v x)) v x where
  simulate cntx (Send (I k)) = do
    v <- cntx `get` k
    send cntx k v



newtype Receive io = Receive (O io) deriving ( Typeable )
deriving instance ( IOType io v x ) => Show (Receive io)
deriving instance ( IOType io v x ) => Eq (Receive io)
instance ( IOType io v x ) => FunctionalBlock (Receive io) v where
  outputs (Receive o) = variables o
instance ( Ord v ) => FunctionSimulation (Receive (Parcel v x)) v x where
  simulate cntx (Receive (O ks)) = do
    let k = head ks
    (cntx', v) <- cntx `receive` k
    set cntx' ks v
