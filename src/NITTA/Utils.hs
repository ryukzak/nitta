{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
--Module      : NITTA.Utils
--Description :
--Copyright   : (c) Aleksandr Penskoi, 2019
--License     : BSD3
--Maintainer  : aleksandr.penskoi@gmail.com
--Stability   : experimental
module NITTA.Utils (
    shift,
    shiftI,
    modify'_,

    -- *HDL generation
    bool2verilog,
    values2dump,
    hdlValDump,
    toModuleName,

    -- *HDL generation (deprecated)
    renderST,

    -- *Process construction (deprecated)
    modifyProcess,
    addStep,
    bindFB,
    setProcessTime,

    -- *Process inspection
    endpointAt,
    getEndpoints,
    transferred,
    inputsPushedAt,
    stepsInterval,
    relatedEndpoints,
    isFB,
    isInstruction,
    isTarget,
    module NITTA.Utils.Base,
    module NITTA.Utils.CodeFormat,
) where

import Control.Monad.State (State, get, modify', put, runState)
import Data.Bits (setBit, testBit)
import Data.List (sortOn)
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Set as S
import qualified Data.String.Utils as S
import NITTA.Intermediate.Types
import NITTA.Model.Problems.Endpoint
import NITTA.Model.ProcessorUnits.Types
import NITTA.Utils.Base
import NITTA.Utils.CodeFormat
import Numeric (readInt, showHex)
import Numeric.Interval (inf, sup, (...))
import qualified Numeric.Interval as I
import Text.StringTemplate

modify'_ :: (s -> s) -> State s ()
modify'_ = modify'

shift offset d@EndpointSt{epAt} = d{epAt = shiftI offset epAt}
shiftI offset i = (I.inf i + offset) ... (I.sup i + offset)

bool2verilog True = "1'b1"
bool2verilog False = "1'b0"

values2dump vs =
    let vs' = concatMap show vs
        x = length vs' `mod` 4
        vs'' = if x == 0 then vs' else replicate (4 - x) '0' ++ vs'
     in concatMap (\e -> showHex (readBin e) "") $ groupBy4 vs''
    where
        groupBy4 [] = []
        groupBy4 xs = take 4 xs : groupBy4 (drop 4 xs)
        readBin :: String -> Int
        readBin = fst . head . readInt 2 (`elem` "x01") (\case '1' -> 1; _ -> 0)

hdlValDump x =
    let bins =
            map (testBit $ rawAttr x) (reverse [0 .. attrWidth x - 1])
                ++ map (testBit $ rawData x) (reverse [0 .. dataWidth x - 1])

        lMod = length bins `mod` 4
        bins' =
            groupBy4 $
                if lMod == 0
                    then bins
                    else replicate (4 - lMod) (head bins) ++ bins
        hs = map (foldr (\(i, a) acc -> if a then setBit acc i else acc) (0 :: Int) . zip [3, 2, 1, 0]) bins'
     in concatMap (`showHex` "") hs
    where
        groupBy4 [] = []
        groupBy4 xs = take 4 xs : groupBy4 (drop 4 xs)

toModuleName = S.replace " " "_"

renderST st attrs = render $ setManyAttrib attrs $ newSTMP st

modifyProcess p st = runState st p

addStep placeInTime info = do
    p@Process{nextUid, steps} <- get
    put
        p
            { nextUid = succ nextUid
            , steps = Step nextUid placeInTime info : steps
            }
    return nextUid

setProcessTime t = do
    p <- get
    put p{nextTick = t}

bindFB fb t = addStep (I.singleton t) $ CADStep $ "bind: " ++ show fb

endpointAt t p =
    case mapMaybe getEndpoint $ whatsHappen t p of
        [ep] -> Just ep
        [] -> Nothing
        eps -> error $ "endpoints collision at: " ++ show t ++ " " ++ show eps

isFB s = isJust $ getFB s

getFB Step{sDesc} | FStep fb <- descent sDesc = Just fb
getFB _ = Nothing

getEndpoint Step{sDesc} | EndpointRoleStep role <- descent sDesc = Just role
getEndpoint _ = Nothing

getEndpoints p = mapMaybe getEndpoint $ sortOn stepStart $ steps p
transferred pu = unionsMap variables $ getEndpoints $ process pu

inputsPushedAt process_ f = sup $ stepsInterval $ relatedEndpoints process_ $ inputs f

stepsInterval ss =
    let a = minimum $ map (inf . sTime) ss
        b = maximum $ map (sup . sTime) ss
     in a ... b

relatedEndpoints process_ vs =
    filter
        ( \s -> case s of
            Step{sDesc = EndpointRoleStep role} -> not $ null (variables role `S.intersection` vs)
            _ -> False
        )
        $ steps process_

isTarget (EndpointSt (Target _) _) = True
isTarget _ = False

isInstruction (InstructionStep _) = True
isInstruction _ = False

stepStart Step{sTime} = I.inf sTime
