{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : NITTA.UIBackend.Timeline
Description : Preparing data for process visualization.
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.UIBackend.Timeline (
    ViewPointID,
    TimelinePoint,
    processTimelines,
    ProcessTimelines (..),
    TimelineWithViewPoint (..),
) where

import Data.Aeson
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.String.ToString
import qualified Data.String.Utils as S
import GHC.Generics
import NITTA.Model.ProcessorUnits
import NITTA.Model.Types
import Numeric.Interval.NonEmpty
import Servant.Docs

data ProcessTimelines t = ProcessTimelines
    { timelines :: [TimelineWithViewPoint t]
    , verticalRelations :: [(ProcessStepID, ProcessStepID)]
    }
    deriving (Generic)

data TimelineWithViewPoint t = TimelineWithViewPoint
    { timelineViewpoint :: ViewPointID
    , timelinePoints :: [[TimelinePoint t]]
    }
    deriving (Generic)

instance (Time t) => Show (ProcessTimelines t) where
    show ProcessTimelines{timelines} =
        let vpLength = maximum $ map (length . show . timelineViewpoint) timelines
            normalizeVP s = s ++ replicate (vpLength - length s) ' '
            line vp tl = normalizeVP (show vp) ++ "\t" ++ concatMap show tl
         in S.join "\n" $ map (\(TimelineWithViewPoint vp tl) -> line vp tl) timelines

data ViewPointID = ViewPointID
    { level :: String
    , component :: [String]
    }
    deriving (Eq, Ord, Generic)

instance Show ViewPointID where
    show ViewPointID{level, component} = S.join "." component ++ "@" ++ level

data TimelinePoint t = TimelinePoint
    { pID :: ProcessStepID
    , pTime :: Interval t
    , pInfo :: String
    }
    deriving (Generic)

instance {-# OVERLAPS #-} (Time t) => Show [TimelinePoint t] where
    show [] = "."
    -- show [TimelinePoint{ pInfo }] | EndpointRoleStep Source{} <- descent pDesc = "^"
    -- show ( Single Step{ pDesc } ) | EndpointRoleStep Target{} <- descent pDesc = "v"

    -- show [ TimelinePoint{ pInfo } ]
    --     | InstructionStep i <- descent pDesc
    --     , Just (_ :: Instruction (BusNetwork String v x t)) <- cast i
    --     = "-"

    show [_] = "*"
    show _ = "#"

processTimelines Process{steps, relations} =
    let views = foldl appendToViews M.empty steps
        a = minimum $ map (inf . pInterval) $ concat $ concat $ M.elems views
        b = maximum $ map (sup . pInterval) $ concat $ concat $ M.elems views
     in ProcessTimelines
            { timelines =
                concatMap
                    ( \(vp, vs) -> map (\v -> TimelineWithViewPoint vp $ timeline a b v) vs
                    )
                    $ M.assocs views
            , verticalRelations = [(u, d) | (Vertical u d) <- relations]
            }

viewpoint FStep{} = ViewPointID{level = "Fun", component = []}
viewpoint CADStep{} = ViewPointID{level = "CAD", component = []}
viewpoint EndpointRoleStep{} = ViewPointID{level = "EndPoint", component = []}
viewpoint InstructionStep{} = ViewPointID{level = "INST", component = []}
viewpoint NestedStep{nTitle, nStep = Step{pDesc}} =
    let ViewPointID{level, component} = viewpoint pDesc
     in ViewPointID{level, component = toString nTitle : component}

appendToViews views step =
    M.alter
        (Just . appendToView step . fromMaybe [])
        (viewpoint $ pDesc step)
        views

appendToView step (tl : tls)
    | any (isConflicted step) tl = tl : appendToView step tls
    | otherwise = (step : tl) : tls
appendToView step [] = [[step]]

isConflicted Step{pInterval = a, pDesc = aD} Step{pInterval = b, pDesc = bD}
    -- we can hold a and b in one bucket, if both is a singleton
    | CADStep{} <- descent aD
      , CADStep{} <- descent bD
      , width a == 0 && width b == 0 =
        False
    --  | width a == 0 && width b == 0 = False
    | otherwise = a ==? b

timeline a b steps = map findSteps [a .. b]
    where
        findSteps t = map foo $ filter (member t . pInterval) steps
        foo Step{pID, pInterval, pDesc} =
            TimelinePoint
                { pID = pID
                , pTime = pInterval
                , pInfo = S.replace "\"" "" $ case pDesc of
                    NestedStep{nTitle, nStep = Step{pDesc = subDesc}} -> toString nTitle ++ " do " ++ show subDesc
                    _ -> show pDesc
                }

instance ToJSON ViewPointID
instance (Time t, ToJSON t) => ToJSON (TimelinePoint t)
instance (Time t, ToJSON t) => ToJSON (TimelineWithViewPoint t)
instance (Time t, ToJSON t) => ToJSON (ProcessTimelines t)

instance ToSample (ProcessTimelines Int) where
    toSamples _ = noSamples
