{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-orphans #-}

module NITTA.Utils where

import           Control.Monad.State
import           Data.Default
import           Data.List           (minimumBy, sortOn)
import           Data.Maybe          (mapMaybe)
import           Data.Set            (difference, elems, unions)
import           Data.Typeable       (Typeable, cast)
import           NITTA.Types
import           NITTA.Utils.Lens
import           Numeric             (readInt, showHex)
import qualified Numeric.Interval    as I
import           Text.StringTemplate



unionsMap f lst = unions $ map f lst
oneOf = head . elems

instance ( Default (Instruction pu)
         , Show (Instruction pu)
         , ProcessUnit pu v t
         , UnambiguouslyDecode pu
         , Time t
         , Typeable pu
         ) => ByTime pu t where
  microcodeAt pu t =
    let instruction = case mapMaybe (extractInstruction pu) $ whatsHappen t (process pu) of
          []  -> def
          [i] -> i
          is  -> error $ "Ambiguously instruction at " ++ show t ++ ": " ++ show is
    in decodeInstruction instruction



isTarget (EndpointO (Target _) _) = True
isTarget _                        = False
isSource (EndpointO (Source _) _) = True
isSource _                        = False

bool2binstr True  = "1"
bool2binstr False = "0"



modifyProcess p st = runState st p

addStep placeInTime info = do
  p@Process{..} <- get
  put p { nextUid=succ nextUid
        , steps=Step nextUid placeInTime info : steps
        }
  return nextUid

addActivity interval = addStep $ Activity interval

relation r = do
  p@Process{..} <- get
  put p{ relations=r : relations }

setProcessTime t = do
  p <- get
  put p{ nextTick=t }

getProcessTime :: State (Process v t) t
getProcessTime = do
  Process{..} <- get
  return nextTick

bindFB fb t = addStep (Event t) $ CADStep $ "Bind " ++ show fb

atSameTime a (Activity t) = a `I.member` t
atSameTime a (Event t)    = a == t

placeInTimeTag (Activity t) = tag $ I.inf t
placeInTimeTag (Event t)    = tag t


stepStart Step{ sTime=Event t }    = t
stepStart Step{ sTime=Activity t } = I.inf t


whatsHappen t Process{ steps } = filter (\Step{ sTime } -> t `atSameTime` sTime) steps


isFB (FBStep _)                = True
isFB (NestedStep _ (FBStep _)) = True
isFB _                         = False

getFB Step{ sDesc=FBStep fb }                = Just fb
getFB Step{ sDesc=NestedStep _ (FBStep fb) } = Just fb
getFB _                                      = Nothing

getFBs p = mapMaybe getFB $ sortOn stepStart $ steps p

getEndpoint :: Step (Parcel v x) t -> Maybe (EndpointRole v)
getEndpoint Step{ sDesc=EndpointRoleStep role }                = Just role
getEndpoint Step{ sDesc=NestedStep _ (EndpointRoleStep role) } = Just role
getEndpoint _                                                  = Nothing

getEndpoints p = mapMaybe getEndpoint $ sortOn stepStart $ steps p

endpointAt t p
  | [eff] <- mapMaybe getEndpoint $ whatsHappen t p = Just eff
  | otherwise = Nothing

endpointsAt t p = mapMaybe getEndpoint $ whatsHappen t p



isInfo (CADStep _) = True
isInfo _           = False


isInstruction (InstructionStep _) = True
isInstruction _                   = False



extractInstruction :: ( Typeable (Instruction pu) ) => pu -> Step v t -> Maybe (Instruction pu)
extractInstruction _ Step{ sDesc=InstructionStep instr } = cast instr
extractInstruction _ _                                   = Nothing


extractInstructionAt pu t
  = let p = process pu
        is = mapMaybe (extractInstruction pu) $ whatsHappen t p
    in case is of
      []  -> Nothing
      [i] -> Just i
      _   -> error $ "Too many instruction on tick." ++ show is


extractInstructions pu = mapMaybe (extractInstruction pu) $ sortOn stepStart $ steps $ process pu


-- | Собрать список переменных подаваемых на вход указанного списка функциональных блоков. При
-- формировании результата отсеиваются входы, получаемые из функциональных блоков рассматриваемого
-- списка.
algInputs fbs = unionsMap inputs fbs `difference` unionsMap outputs fbs
algOutputs fbs = unionsMap outputs fbs `difference` unionsMap inputs fbs



values2dump vs
  = let vs' = concatMap show vs
        x = length vs' `mod` 4
        vs'' = if x == 0 then vs' else replicate (4 - x) '0' ++ vs'
    in concatMap (\e -> showHex (readBin e) "") $ groupBy4 vs''
  where
    groupBy4 [] = []
    groupBy4 xs = take 4 xs : groupBy4 (drop 4 xs)
    readBin :: String -> Int
    readBin = fst . head . readInt 2 (`elem` "x01") (\case '1' -> 1; _ -> 0)


renderST st attrs = render $ setManyAttrib attrs $ newSTMP $ unlines st


isTimeWrap p act = nextTick p > act^.at.infimum
timeWrapError p act = error $ "You can't start work yesterday :) fram time: " ++ show (nextTick p) ++ " action start at: " ++ show (act^.at.infimum)

addInstr :: ( Typeable pu, Show (Instruction pu) ) => pu -> I.Interval t -> Instruction pu -> State (Process v t) ProcessUid
addInstr _pu t i = addStep (Activity t) $ InstructionStep i


minimumOn f = minimumBy (\a b -> f a `compare` f b)
