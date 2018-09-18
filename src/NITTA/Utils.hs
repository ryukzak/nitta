{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-orphans #-}

module NITTA.Utils
    ( unionsMap
    , oneOf
    , algInputs
    , algOutputs
    , isTimeWrap
    , timeWrapError
    , minimumOn
    , maximumOn
    , shift
    -- *HDL generation
    , bool2verilog
    , values2dump
    -- *HDL generation (depricated)
    , renderMST
    , renderST
    -- *Process construction (depricated)
    , modifyProcess
    , addActivity
    , addInstr
    , addStep
    , addStep_
    , bindFB
    , relation
    , setProcessTime
    -- *Process inspection
    , endpointAt
    , extractInstruction
    , extractInstructionAt
    , getEndpoints
    , getFBs
    , isFB
    , isInstruction
    , isTarget
    , placeInTimeTag
    , whatsHappen
    , instructionOf
    , maybeInstructionOf
    ) where

import           Control.Monad.State
import           Data.Default
import           Data.List           (minimumBy, maximumBy, sortOn)
import           Data.Maybe          (isJust, mapMaybe)
import           Data.Set            (difference, elems, unions)
import           Data.Typeable       (Typeable, cast)
import           NITTA.Types
import           NITTA.Utils.Lens
import           Numeric             (readInt, showHex)
import           Numeric.Interval    ((...))
import qualified Numeric.Interval    as I
import           Text.StringTemplate



instance ( Show (Instruction pu)
         , Default (Microcode pu)
         , ProcessUnit pu v t
         , UnambiguouslyDecode pu
         , Time t
         , Typeable pu
         , Controllable pu
         ) => ByTime pu t where
    microcodeAt pu t = case mapMaybe (extractInstruction pu) $ whatsHappen t (process pu) of
        []  -> def
        [i] -> decodeInstruction i
        is  -> error $ "Ambiguously instruction at " ++ show t ++ ": " ++ show is

instance ( Ord t ) => WithFunctions (Process (Parcel v x) t) (F (Parcel v x)) where
    functions = getFBs



unionsMap f lst = unions $ map f lst
oneOf = head . elems


-- |Собрать список переменных подаваемых на вход указанных функций. При формировании результата
-- отсеиваются входы, получаемые из функциональных блоков рассматриваемого списка.
algInputs fbs = unionsMap inputs fbs `difference` unionsMap outputs fbs
algOutputs fbs = unionsMap outputs fbs `difference` unionsMap inputs fbs


isTimeWrap p act = nextTick p > act^.at.infimum
timeWrapError p act = error $ "You can't start work yesterday :) fram time: " ++ show (nextTick p) ++ " action start at: " ++ show (act^.at.infimum)

minimumOn f = minimumBy (\a b -> f a `compare` f b)
maximumOn f = maximumBy (\a b -> f a `compare` f b)

shift n d@EndpointD{ epdAt } = d{ epdAt=(I.inf epdAt + n) ... (I.sup epdAt + n) }



bool2verilog True  = "1'b1"
bool2verilog False = "1'b0"

values2dump vs
    = let
        vs' = concatMap show vs
        x = length vs' `mod` 4
        vs'' = if x == 0 then vs' else replicate (4 - x) '0' ++ vs'
    in concatMap (\e -> showHex (readBin e) "") $ groupBy4 vs''
    where
        groupBy4 [] = []
        groupBy4 xs = take 4 xs : groupBy4 (drop 4 xs)
        readBin :: String -> Int
        readBin = fst . head . readInt 2 (`elem` "x01") (\case '1' -> 1; _ -> 0)



renderMST st attrs = render $ setManyAttrib attrs $ newSTMP $ unlines st
renderST st attrs = render $ setManyAttrib attrs $ newSTMP st



modifyProcess p st = runState st p

addStep placeInTime info = do
    p@Process{ nextUid, steps } <- get
    put p { nextUid=succ nextUid
            , steps=Step nextUid placeInTime info : steps
            }
    return nextUid

addStep_ placeInTime info = do
    _ <- addStep placeInTime info
    return ()

addActivity interval = addStep $ Activity interval

relation r = do
    p@Process{ relations } <- get
    put p{ relations=r : relations }

setProcessTime t = do
    p <- get
    put p{ nextTick=t }

bindFB fb t = addStep (Event t) $ CADStep $ "Bind " ++ show fb

addInstr :: ( Typeable pu, Show (Instruction pu) ) => pu -> I.Interval t -> Instruction pu -> State (Process v t) ProcessUid
addInstr _pu t i = addStep (Activity t) $ InstructionStep i



whatsHappen t Process{ steps } = filter (\Step{ sTime } -> t `atSameTime` sTime) steps

endpointAt t p
    = case mapMaybe getEndpoint $ whatsHappen t p of
        [ep] -> Just ep
        []   -> Nothing
        eps  -> error $ "Too many endpoint at a time: " ++ show eps


getFB step | Step{ sDesc=FStep fb } <- descent step = Just fb
getFB _    = Nothing

getFBs p = mapMaybe getFB $ sortOn stepStart $ steps p


getEndpoint :: Step (Parcel v x) t -> Maybe (EndpointRole v)
getEndpoint step | Step{ sDesc=EndpointRoleStep role } <- descent step = Just role
getEndpoint _                                                          = Nothing

getEndpoints p = mapMaybe getEndpoint $ sortOn stepStart $ steps p


extractInstruction :: ( Typeable (Instruction pu) ) => pu -> Step v t -> Maybe (Instruction pu)
extractInstruction _ Step{ sDesc=InstructionStep instr } = cast instr
extractInstruction _ _                                   = Nothing

extractInstructionAt pu t = mapMaybe (extractInstruction pu) $ whatsHappen t $ process pu


isTarget (EndpointO (Target _) _) = True
isTarget _                        = False

isFB s = isJust $ getFB s

isInstruction (InstructionStep _) = True
isInstruction _                   = False


atSameTime a (Activity t) = a `I.member` t
atSameTime a (Event t)    = a == t


placeInTimeTag (Activity t) = tag $ I.inf t
placeInTimeTag (Event t)    = tag t


stepStart Step{ sTime=Event t }    = t
stepStart Step{ sTime=Activity t } = I.inf t


-- modern

instructionOf :: Instruction pu -> pu -> Instruction pu
i `instructionOf` _pu = i

maybeInstructionOf :: Maybe (Instruction pu) -> pu -> Maybe (Instruction pu)
i `maybeInstructionOf` _pu = i
