{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module NITTA.Utils where

import           Control.Monad.State
import           Data.Default
import           Data.List           (sortBy)
import qualified Data.Map            as M
import           Data.Maybe          (mapMaybe)
import           Data.Proxy
import           Data.Typeable       (Typeable, cast)
import           NITTA.Types
import           Numeric             (readInt)
import qualified Numeric.Interval    as I
import           Text.StringTemplate



isTarget (EndpointO (Target _) _) = True
isTarget _                        = False
isSource (EndpointO (Source _) _) = True
isSource _                        = False




modifyProcess p st = runState st p

add placeInTime info = do
  p@Process{..} <- get
  put p { nextUid=succ nextUid
        , steps=Step nextUid placeInTime info : steps
        }
  return nextUid

addActivity interval = add $ Activity interval

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

bindFB fb t = add (Event t) $ CADStep $ "Bind " ++ show fb

atSameTime a (Activity t) = a `I.member` t
atSameTime a (Event t)    = a == t

placeInTimeTag (Activity t) = tag $ I.inf t
placeInTimeTag (Event t)    = tag t


stepStart Step{ sTime=Event t }    = t
stepStart Step{ sTime=Activity t } = I.inf t


whatsHappen t Process{..} = filter (\Step{..} -> t `atSameTime` sTime) steps


isFB (FBStep _)                = True
isFB (NestedStep _ (FBStep _)) = True
isFB _                         = False

getFB Step{ sDesc=FBStep fb }                = Just fb
getFB Step{ sDesc=NestedStep _ (FBStep fb) } = Just fb
getFB _                                      = Nothing

getFBs p = mapMaybe getFB
           $ sortBy (\a b -> stepStart a `compare` stepStart b)
           $ steps p


getEndpoint Step{ sDesc=EndpointStep eff }                = Just eff
getEndpoint Step{ sDesc=NestedStep _ (EndpointStep eff) } = Just eff
getEndpoint _                                             = Nothing

getEndpoints p = mapMaybe getEndpoint
               $ sortBy (\a b -> stepStart a `compare` stepStart b)
               $ steps p

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


extractInstructions pu = mapMaybe (extractInstruction pu)
                            $ sortBy (\a b -> stepStart a `compare` stepStart b)
                            $ steps $ process pu


-- | Собрать список переменных подаваемых на вход указанного списка функциональных блоков. При
-- формировании результата отсеиваются входы, получаемые из функциональных блоков рассматриваемого
-- списка.
inputsOfFBs fbs
  = let deps0 = M.fromList [(v, []) | v <- concatMap variables fbs]
        deps = foldl (\dict (a, b) -> M.adjust ((:) b) a dict) deps0 $ concatMap dependency fbs
    in map fst $ filter (null . snd) $ M.assocs deps

-- outputsOfFBs fbs
--   = let deps0 = (M.fromList [(v, []) | v <- concatMap variables fbs])
--         deps = foldl (\dict (a, b) -> M.adjust ((:) b) a dict) deps0 $ concatMap dependency fbs
--     in filter (\a -> all (not . (a `elem`)) deps) $ M.keys deps


fromLeft :: a -> Either a b -> a
fromLeft _ (Left a) = a
fromLeft a _        = a

fromRight :: b -> Either a b -> b
fromRight _ (Right b) = b
fromRight b _         = b



values2dump vs = concatMap (show . readBin) $ groupBy4 $ concatMap show vs
  where
    groupBy4 [] = []
    groupBy4 xs = take 4 xs : groupBy4 (drop 4 xs)
    readBin :: String -> Int
    readBin = fst . head . readInt 2 (`elem` "x01") (\x -> case x of '1' -> 1; _ -> 0 )


renderST st attrs = render $ setManyAttrib attrs $ newSTMP $ unlines st


variableValueWithoutFB pu cntx vi@(v, _)
  | [fb] <- filter (elem v . variables) fbs
  = variableValue fb pu cntx vi
  | otherwise = error $ "can't find varValue for: " ++ show v ++ " "
                ++ show cntx ++ " "
                ++ show fbs
  where
    fbs = mapMaybe getFB $ steps $ process pu


nopFor :: ( Default (Instruction pu) ) => Proxy pu -> Instruction pu
nopFor _proxy = def
