{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module NITTA.Utils where

import           Control.Monad.State
import           Data.Default
import           Data.Functor.Const
import           Data.List            (find, intersect, isSubsequenceOf,
                                       partition, sortBy)
import qualified Data.List            as L
import qualified Data.Map             as M
import           Data.Maybe           (catMaybes, fromMaybe, isJust)
import           Data.Proxy
import           Data.Typeable        (Typeable, cast, typeOf)
import           NITTA.FunctionBlocks
import qualified NITTA.FunctionBlocks as FB
import           NITTA.Types
import           Numeric              (readInt)
import           Numeric.Interval     (inf, singleton, sup, (...))
import qualified Numeric.Interval     as I
import           System.Exit
import           System.Process
import           Text.StringTemplate

import           Debug.Trace


-- v (@@) str = trace str v


isPull (EffectOpt (Pull _) _) = True
isPull _                      = False
isPush (EffectOpt (Push _) _) = True
isPush _                      = False




modifyProcess p state = runState state p

add time info = do
  p@Process{..} <- get
  put p { nextUid=succ nextUid
        , steps=Step nextUid time info : steps
        }
  return nextUid

relation r = do
  p@Process{..} <- get
  put p{ relations=r : relations }

setProcessTime t = do
  p <- get
  put p{ nextTick=t }

processTime :: State (Process v t) t
processTime = do
  Process{..} <- get
  return nextTick

bindFB fb t = add (Event t) $ CADStep $ "Bind " ++ show fb

atSameTime a (Activity t) = a `I.elem` t
atSameTime a (Event t)    = a == t

placeInTimeTag (Activity t) = tag $ inf t
placeInTimeTag (Event t)    = tag t


stepStart Step{ sTime=Event t }    = t
stepStart Step{ sTime=Activity t } = inf t


whatsHappen t Process{..} =
  -- FIXME
  filter (\Step{..} -> t `atSameTime` sTime) steps


isFB (FBStep _)                = True
isFB (NestedStep _ (FBStep _)) = True
isFB _                         = False

getFB Step{ sDesc=FBStep fb }                = Just fb
getFB Step{ sDesc=NestedStep _ (FBStep fb) } = Just fb
getFB _                                      = Nothing

getFBs p = catMaybes $ map getFB
           $ sortBy (\a b -> stepStart a `compare` stepStart b)
           $ steps p


isEffect (EffectStep _) = True
isEffect _              = False

getEffect Step{ sDesc=EffectStep eff }                = Just eff
getEffect Step{ sDesc=NestedStep _ (EffectStep eff) } = Just eff
getEffect s                                           = -- trace (">>" ++ show s)
                                                        Nothing

getEffects p = catMaybes $ map getEffect
               $ sortBy (\a b -> stepStart a `compare` stepStart b)
               $ steps p

effectAt t p
  | [eff] <- catMaybes $ map getEffect $ -- trace (">" ++ show (whatsHappen t p)) $
                                         whatsHappen t p = Just eff
  | otherwise = Nothing

effectsAt t p = catMaybes $ map getEffect $ -- trace (">" ++ show (whatsHappen t p)) $
                                            whatsHappen t p



isInfo (CADStep _) = True
isInfo _           = False


isInstruction (InstructionStep _) = True
isInstruction _                   = False



extractInstruction :: ( Typeable pu ) => pu -> Step v t -> Maybe (Instruction pu)
extractInstruction _ Step{ sDesc=InstructionStep instr } = cast instr
extractInstruction _ _                                   = Nothing


extractInstructionAt pu t
  = let p = process pu
        is = catMaybes $ map (extractInstruction pu) $ whatsHappen t p
    in case is of
      []  -> Nothing
      [i] -> Just i
      _   -> error $ "Too many instruction on tick " ++ show is


extractInstructions pu = catMaybes $ map (extractInstruction pu)
                                   $ sortBy (\a b -> stepStart a `compare` stepStart b)
                                   $ steps $ process pu



inputsOfFBs fbs
  = let deps0 = (M.fromList [(v, []) | v <- concatMap variables fbs])
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
    groupBy4 xs = (take 4 xs) : (groupBy4 $ drop 4 xs)
    readBin :: String -> Int
    readBin = fst . head . readInt 2 (`elem` "x01") (\x -> case x of '1' -> 1; _ -> 0 )


renderST st attrs = render $ setManyAttrib attrs $ newSTMP $ unlines st


variableValueWithoutFB pu cntx vi@(v, _)
  | [fb] <- filter (elem v . (\(FB fb) -> variables fb)) fbs
  = variableValue fb pu cntx vi
  -- = variableValue (trace (">>> " ++ show vi ++ " " ++ show fb ++ " " ++ show cntx) fb) pu cntx vi
  | otherwise = error $ "can't find varValue for: " ++ show v ++ " "
                ++ show cntx ++ " "
                ++ show fbs
  where
    fbs = catMaybes $ map getFB $ steps $ process pu
