{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : NITTA.UIBackend.Timeline
Description : Preparing data for process visualization.
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.UIBackend.Timeline
    ( ViewPointID, TimelinePoint, processTimelines, ProcessTimelines(..), TimelineWithViewPoint(..)
    ) where

import qualified Data.Map as M
import           Data.Maybe
import qualified Data.String.Utils as S
import           GHC.Generics
import           NITTA.Model.ProcessorUnits.Time
import           NITTA.Model.Types
import           Numeric.Interval

data ProcessTimelines t
    = ProcessTimelines
        { timelines         :: [ TimelineWithViewPoint t ]
        , verticalRelations :: [ (ProcessUid, ProcessUid) ]
        }
    deriving ( Generic )

data TimelineWithViewPoint t = TimelineWithViewPoint
        { timelineViewpoint :: ViewPointID
        , timelinePoints    :: [ [TimelinePoint t] ]
        }
    deriving ( Generic )

instance ( Time t ) => Show (ProcessTimelines t) where
    show ProcessTimelines{ timelines } = let
            vpLength = maximum $ map (length . show . timelineViewpoint) timelines
            normalizeVP s = s ++ replicate (vpLength - length s) ' '
            line vp tl = normalizeVP (show vp) ++ "\t" ++ concatMap show tl
        in S.join "\n" $ map (\(TimelineWithViewPoint vp tl) -> line vp tl) timelines


data ViewPointID = ViewPointID
        { level     :: String
        , component :: [ String ]
        }
    deriving ( Eq, Ord, Generic )

instance Show ViewPointID where
    show ViewPointID{ level, component } = S.join "." component ++ "@" ++ level


data TimelinePoint t
    = TimelinePoint
        { pID   :: ProcessUid
        , pTime :: Interval t
        , pInfo :: String
        }
    deriving ( Generic )

instance {-# OVERLAPS #-} ( Time t ) => Show [ TimelinePoint t ] where
    show []    = "."

    -- show [TimelinePoint{ pInfo }] | EndpointRoleStep Source{} <- descent sDesc = "^"
    -- show ( Single Step{ sDesc } ) | EndpointRoleStep Target{} <- descent sDesc = "v"

    -- show [ TimelinePoint{ pInfo } ]
    --     | InstructionStep i <- descent sDesc
    --     , Just (_ :: Instruction (BusNetwork String v x t)) <- cast i
    --     = "-"

    show [ _ ] = "*"
    show _     = "#"


processTimelines Process{ steps, relations } = let
        views = foldl appendToViews M.empty steps
        a = minimum $ map (inf . sTime) $ concat $ concat $ M.elems views
        b = maximum $ map (sup . sTime) $ concat $ concat $ M.elems views
    in ProcessTimelines
        { timelines=concatMap (
                \(vp, vs) -> map (\v -> TimelineWithViewPoint vp $ timeline a b v ) vs
            ) $ M.assocs views
        , verticalRelations=[ (u, d) | (Vertical u d) <- relations ]
        }

viewpoint FStep{} = ViewPointID{ level="Fun", component=[] }
viewpoint CADStep{} = ViewPointID{ level="CAD", component=[] }
viewpoint EndpointRoleStep{} = ViewPointID{ level="EndPoint", component=[] }
viewpoint InstructionStep{} = ViewPointID{ level="INST", component=[] }
viewpoint NestedStep{ nTitle, nStep=Step{ sDesc } } = let
        ViewPointID{ level, component } = viewpoint sDesc
    in ViewPointID{ level, component=S.replace "\"" "" (show nTitle) : component }

appendToViews views step = M.alter
    (Just . appendToView step . fromMaybe [])
    (viewpoint $ sDesc step)
    views

appendToView step (tl:tls)
    | any (isConflicted step) tl = tl : appendToView step tls
    | otherwise                           = (step : tl) : tls
appendToView step [] = [ [ step ] ]

isConflicted Step{ sTime=a, sDesc=aD } Step{ sTime=b, sDesc=bD }
    -- we can hold a and b in one bucket, if both is a singleton
    | CADStep{} <- descent aD
    , CADStep{} <- descent bD
    , width a == 0 && width b == 0 = False
    --  | width a == 0 && width b == 0 = False
    | otherwise = a ==? b

timeline a b steps = map findSteps [a..b]
    where
        findSteps t = map foo $ filter (member t . sTime) steps
        foo Step{ sKey, sTime, sDesc } = TimelinePoint
            { pID=sKey
            , pTime=sTime
            , pInfo=S.replace "\"" "" $ case sDesc of
                NestedStep{ nTitle, nStep=Step{ sDesc=subDesc } } -> show nTitle ++ " do " ++ show subDesc
                _                                                 -> show sDesc
            }
