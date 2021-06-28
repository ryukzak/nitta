{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : NITTA.Utils
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Utils (
    doc2text,
    Verilog,
    comment,
    shiftI,
    modify'_,

    -- *HDL generation
    bool2verilog,
    values2dump,
    hdlValDump,
    toModuleName,

    -- *Process inspection
    endpointAt,
    getEndpoints,
    transferred,
    inputsPushedAt,
    stepsInterval,
    relatedEndpoints,
    isFB,
    getFBs,
    isInstruction,
    module NITTA.Utils.Base,

    -- *Toml
    getToml,
    getFromToml,
    getFromTomlSection,
) where

import Control.Monad.State (State, modify')
import Data.Aeson
import Data.Bits (setBit, testBit)
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import Data.Maybe
import qualified Data.Set as S
import qualified Data.String.Utils as S
import qualified Data.Text as T
import NITTA.Intermediate.Types
import NITTA.Model.ProcessorUnits.Types
import NITTA.Utils.Base
import Numeric (readInt, showHex)
import Numeric.Interval.NonEmpty (inf, sup, (...))
import qualified Numeric.Interval.NonEmpty as I
import Prettyprinter
import Prettyprinter.Render.Text
import Text.Toml (parseTomlDoc)

type Verilog = Doc ()
doc2text :: Verilog -> T.Text
doc2text = renderStrict . layoutPretty defaultLayoutOptions

comment str = unlines $ map ("// " <>) $ lines str

modify'_ :: (s -> s) -> State s ()
modify'_ = modify'

shiftI offset i = i + I.singleton offset

bool2verilog True = "1'b1" :: T.Text
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
        readBin = fst . head . readInt 2 (`elem` ("x01" :: String)) (\case '1' -> 1; _ -> 0)

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
     in T.concat $ map (T.pack . (`showHex` "")) hs
    where
        groupBy4 [] = []
        groupBy4 xs = take 4 xs : groupBy4 (drop 4 xs)

toModuleName = S.replace " " "_"

endpointAt t p =
    case mapMaybe getEndpoint $ whatsHappen t p of
        [ep] -> Just ep
        [] -> Nothing
        eps -> error $ "endpoints collision at: " ++ show t ++ " " ++ show eps

isFB s = isJust $ getFB s

getFB Step{pDesc} | FStep fb <- descent pDesc = Just fb
getFB _ = Nothing

getFBs p = mapMaybe getFB $ sortOn stepStart $ steps p

getEndpoint Step{pDesc} | EndpointRoleStep role <- descent pDesc = Just role
getEndpoint _ = Nothing

getEndpoints p = mapMaybe getEndpoint $ sortOn stepStart $ steps p
transferred pu = unionsMap variables $ getEndpoints $ process pu

inputsPushedAt process_ f = sup $ stepsInterval $ relatedEndpoints process_ $ inputs f

stepsInterval ss =
    let a = minimum $ map (inf . pInterval) ss
        b = maximum $ map (sup . pInterval) ss
     in a ... b

relatedEndpoints process_ vs =
    filter
        ( \case
            Step{pDesc = EndpointRoleStep role} -> not $ null (variables role `S.intersection` vs)
            _ -> False
        )
        $ steps process_

isInstruction (InstructionStep _) = True
isInstruction _ = False

stepStart Step{pInterval} = I.inf pInterval

getToml text = either (error . show) id $ parseTomlDoc "parse error: " text

getFromToml toml = getFromTomlSection T.empty toml

getFromTomlSection section toml
    | section == T.empty = unwrap $ fromJSON $ toJSON toml
    | otherwise = case HM.lookup section toml of
        Just s -> unwrap $ fromJSON $ toJSON s
        Nothing -> error $ "section not found - " <> T.unpack section
    where
        unwrap (Success conf) = conf
        unwrap (Error msg) = error msg
