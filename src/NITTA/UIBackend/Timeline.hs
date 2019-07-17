{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.UIBackend.Timeline
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.UIBackend.Timeline
    ( ViewPointID, TimelinePoint, processTimelines, ProcessTimelines(..)
    ) where

import qualified Data.Map                         as M
import           Data.Maybe
import qualified Data.String.Utils                as S
import           Data.Typeable
import           GHC.Generics
import           NITTA.Model.Networks.Bus
import           NITTA.Model.Problems.Endpoint
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Model.Types
import           Numeric.Interval

data ProcessTimelines v x t
    = ProcessTimelines
        { timelines :: [ ( ViewPointID, [ TimelinePoint v x t ] ) ]
        }
    deriving ( Generic )

instance ( VarValTime v x t ) => Show (ProcessTimelines v x t) where
    show ProcessTimelines{ timelines } = let
            vpLength = maximum $ map (length . show . fst) timelines
            normalizeVP s = s ++ replicate (vpLength - length s) ' '
            line vp tl = normalizeVP (show vp) ++ "\t" ++ concatMap show tl
        in S.join "\n" $ map (\(vp, tl) -> line vp tl) timelines


data ViewPointID = ViewPointID
        { level     :: String
        , component :: [ String ]
        }
    deriving ( Eq, Ord, Generic )

instance Show ViewPointID where
    show ViewPointID{ level, component } = S.join "." component ++ "@" ++ level


data TimelinePoint v x t
    = None
    | Single ( Step v x t )
    | Bucket [ Step v x t ]
    deriving ( Generic )

instance ( VarValTime v x t ) => Show ( TimelinePoint v x t ) where
    show None = "."

    show ( Single Step{ sDesc } ) | EndpointRoleStep Source{} <- descent sDesc = "^"
    show ( Single Step{ sDesc } ) | EndpointRoleStep Target{} <- descent sDesc = "v"

    show ( Single Step{ sDesc } )
        | InstructionStep i <- descent sDesc
        , Just (_ :: Instruction (BusNetwork String v x t)) <- cast i
        = "-"

    show Single{} = "*"
    show Bucket{} = "#"



processTimelines Process{ steps } = let
        views = foldl appendToViews M.empty steps
        a = minimum $ map (inf . sTime) $ concat $ concat $ M.elems views
        b = maximum $ map (sup . sTime) $ concat $ concat $ M.elems views
    in ProcessTimelines
        { timelines=concatMap (
                \(vp, vs) -> map (\v -> ( vp, timeline a b v )) vs
            ) $ M.assocs views
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
        findSteps t = case filter (member t . sTime) steps of
            []     -> None
            [step] -> Single step
            steps' -> Bucket steps'

