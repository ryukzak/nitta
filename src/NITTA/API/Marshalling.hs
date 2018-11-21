{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-orphans #-}

{-|
Module      : NITTA.API.Marshalling
Description : Marshalling data for REST API
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}

module NITTA.API.Marshalling where

import           Data.Aeson
import qualified Data.Map              as M
import qualified Data.Text             as T
import           Data.Set              (toList)
import           Data.Function         (on)
import           Data.Typeable
import           NITTA.BusNetwork
import           NITTA.Compiler
import           NITTA.DataFlow
import           NITTA.Types
import           NITTA.Types.Project
import           NITTA.Types.Synthesis
import           NITTA.Utils           (transfered)
import qualified NITTA.Functions       as F
import           Numeric.Interval
import           Servant



-- *Option/Decision
instance ( ToJSON title
         , ToJSONKey v, ToJSON v, Var v
         , ToJSON (TimeConstrain t)
         ) => ToJSON (Option (CompilerDT title tag v t))
instance ( ToJSON title
         , ToJSONKey v, ToJSON v, Var v
         , ToJSON (TimeConstrain t), Time t
         ) => ToJSON (Decision (CompilerDT title tag v t))
instance ( ToJSON title
         , ToJSONKey v
         , ToJSON (TimeConstrain t)
         ) => ToJSON (Option (DataFlowDT title v t))
instance ( ToJSON title
         , ToJSONKey v
         , ToJSON (TimeConstrain t), Time t
         ) => ToJSON (Decision (DataFlowDT title v t))
instance ( Show title
         ) => ToJSON (Option (BindingDT title v x)) where
    toJSON (BindingO f title) = toJSON [ show f, show title ]
instance ( Show title
         ) => ToJSON (Decision (BindingDT title v x)) where
    toJSON (BindingD f title) = toJSON [ show f, show title ]
-- instance ( ToJSON v, Var v ) => ToJSON (Option (ControlDT v))
-- instance ( ToJSON v, Var v ) => ToJSON (Decision (ControlDT v))



-- *Process units
instance ( ToJSONKey title, ToJSON title, Typeable title, Ord title, Show title
         , Var v
         , Time t, ToJSON t
         , Typeable x, ToJSON x, ToJSONKey x
         ) => ToJSON (BusNetwork title v x t) where
    toJSON n@BusNetwork{..} = object
        [ "width" .= bnSignalBusWidth
        , "remain" .= bnRemains
        , "forwardedVariables" .= map (String . T.pack . show) (transfered n)
        , "binds" .= bnBinded
        , "processLength" .= nextTick (process n)
        , "processUnits" .= M.keys bnPus
        , "process" .= process n
        ]



-- *Model
instance ( ToJSON v, Var v, ToJSON x ) => ToJSON (DataFlowGraph v x)

instance ToJSON Relation where
    toJSON (Vertical a b) = toJSON [ a, b ]

instance ( ToJSONKey title, ToJSON title, Show title, Ord title, Typeable title
         , ToJSON tag
         , ToJSON v, Var v
         , ToJSON t, Time t
         , ToJSONKey v
         , Show x, Ord x, Typeable x, ToJSON x, ToJSONKey x
         ) => ToJSON (ModelState title tag x v t)

instance ( ToJSON t, Time t, Show v
         ) => ToJSON (Process v x t) where
    toJSON Process{ steps, nextTick, relations } = object
        [ "steps" .= steps
        , "nextTick" .= nextTick
        , "relations" .= relations
        ]

instance ( ToJSON t, Time t, Show v
         ) => ToJSON (Step v x t) where
    toJSON Step{ sKey, sTime, sDesc } = object
        [ "sKey" .= sKey
        , "sDesc" .= show sDesc
        , "sTime" .= sTime
        , "sLevel" .= level sDesc
        , "sPU" .= showPU sDesc
        ]



-- *Synthesis
instance ToJSON Nid where
    toJSON nid = toJSON $ show nid

instance FromJSON Nid where
    parseJSON v = read <$> parseJSON v

instance FromHttpApiData Nid where
    parseUrlPiece = Right . read . T.unpack


instance ToJSON (Synthesis String String String Int (TaggedTime String Int)) where
    toJSON Synthesis{ sModel, sCntx, sStatus } = object
        [ "sModel" .= sModel
        , "sCntx" .= map show sCntx
        , "sStatus" .= show sStatus
        ]

instance ToJSON SynthesisStatus
instance ToJSON TestBenchReport


-- *Simple compiler
instance ToJSON SynthesisSetup
instance ToJSON SpecialMetrics

instance ToJSON (WithMetric (CompilerDT String String String (TaggedTime String Int))) where
    toJSON WithMetric{ mIntegral, mSpecial, mOption, mDecision }
        = toJSON ( mIntegral, mSpecial, mOption, mDecision )


-- *Basic data
instance ( ToJSON tag, ToJSON t ) => ToJSON (TaggedTime tag t)

instance ( ToJSON t, Time t ) => ToJSON (PlaceInTime t) where
    toJSON (Event t)    = toJSON [ fromEnum t, fromEnum t ]
    toJSON (Activity i) = toJSON [ fromEnum $ inf i, fromEnum $ sup i ]

instance ( Show v ) => ToJSON (F v x) where
    toJSON = String . T.pack . show

instance ( ToJSON t, Time t ) => ToJSON (TimeConstrain t) where
    toJSON TimeConstrain{..} = object
        [ "available" .= tcAvailable
        , "duration" .= tcDuration
        ]



-- *System
instance ( Show a ) => ToJSON (Interval a) where
    toJSON = String . T.pack . show



-- *Graph converting
data GraphStructure = GraphStructure {
    nodes :: [NodeElement],
    edges :: [GraphEdge]
}

instance ToJSON GraphStructure where
    toJSON GraphStructure{ nodes, edges } =  object
        [ "nodes" .= nodes
        , "edges" .= edges
        ]


data NodeElement = NodeElement {
    nodeId    :: Int,
    nodeParam :: NodeParam
}

data NodeParam = NodeParam {
    nodeName  :: String,
    nodeColor :: String,
    nodeShape :: String,
    fontSize  :: String,
    nodeSize  :: String
}

box     name color = NodeParam{nodeName=name, nodeColor=color, nodeShape="box",     fontSize="20", nodeSize="30"}
ellipse name color = NodeParam{nodeName=name, nodeColor=color, nodeShape="ellipse", fontSize="20", nodeSize="30"}

instance ToJSON NodeElement where
    toJSON NodeElement{ nodeId, nodeParam = NodeParam{ nodeName, nodeColor, nodeShape, fontSize, nodeSize } } =  object
        [ "id"    .= nodeId
        , "label" .= nodeName
        , "color" .= nodeColor
        , "shape" .= nodeShape
        , "size"  .= nodeSize
        , "font"  .= object 
            [
                "size" .= fontSize
            ]
        ]


data EdgeParam = EdgeParam {
    edgeName   :: String,
    edgeWidth  :: String,
    fontAllign :: String
}

arrow name = EdgeParam name "2" "bottom"

data GraphEdge = GraphEdge {
    edgeParam :: EdgeParam,
    inNodeId  :: Int,
    outNodeId :: Int
}

instance ToJSON GraphEdge where
    toJSON GraphEdge{ edgeParam = EdgeParam { edgeName, edgeWidth, fontAllign }, inNodeId, outNodeId } =  object
        [ "from"  .= outNodeId
        , "to"    .= inNodeId
        , "label" .= edgeName
        , "width" .= edgeWidth
        , "font"  .= object 
            [
                "allign" .= fontAllign
            ]
        ]


data VertexType = InVertex | OutVertex deriving Eq
data GraphVertex = GraphVertex {
    vertexType   :: VertexType,
    vertexName   :: String,
    vertexNodeId :: Int
}

transformElement :: (Typeable x) => F String x -> Int -> Bool -> ([NodeElement], [GraphVertex])
transformElement fb n constNodes
        | Just (F.Add          (I a) (I b) (O cs))        <- F.castF fb = configure box n "#e04141" (show fb) [a, b] $ toList cs
        | Just (F.Sub          (I a) (I b) (O cs))        <- F.castF fb = configure box n "#7fffd4" (show fb) [a, b] $ toList cs
        | Just (F.Multiply     (I a) (I b) (O cs))        <- F.castF fb = configure box n "#0099bf" (show fb) [a, b] $ toList cs
        | Just (F.Division     (I a) (I b) (O cs) (O ds)) <- F.castF fb = configure box n "#fa8072" (show fb) [a, b] $ on (++) toList cs ds
        | Just (F.ShiftL       (I a) (O cs))              <- F.castF fb = configure box n "#fff68f" (show fb) [a]    $ toList cs
        | Just (F.ShiftR       (I a) (O cs))              <- F.castF fb = configure box n "#fff68f" (show fb) [a]    $ toList cs
        | Just (F.Send         (I a))                     <- F.castF fb = configure box n "#088da5" (show fb) [a]    []
        | Just (F.Receive      (O cs))                    <- F.castF fb = configure box n "#6897bb" (show fb) []     $ toList cs
        | Just (F.FramOutput _ (I a))                     <- F.castF fb = configure box n "#daa520" (show fb) [a]    []
        | Just (F.FramInput  _ (O cs))                    <- F.castF fb = configure box n "#ffa500" (show fb) []     $ toList cs
        | Just (F.Reg          (I a) (O cs))              <- F.castF fb = configure box n "#c0c0c0" (show fb) [a]    $ toList cs
        | Just (F.Loop         (X _) (O cs) (I a))        <- F.castF fb = configureLoopNodes n "#cbbeb5" "#808080" (show fb) ("next -> " ++ a) a $ toList cs
        | Just (F.Constant     (X _) (O cs))              <- F.castF fb = configureConstantNodes n "#6dc066" (show fb) $ toList cs
        | otherwise                                                     = error $ "The functional block is unsupported: " ++ show fb
    where
        configure :: (String -> String -> NodeParam)-> Int -> String -> String -> [String] -> [String] -> ([NodeElement], [GraphVertex])
        configure shape m color name xs ys = ([NodeElement m $ shape name color], 
                                              map (\c -> GraphVertex InVertex  c m) xs ++ 
                                              map (\c -> GraphVertex OutVertex c m) ys)

        configureLoopNodes :: Int -> String -> String -> String -> String -> String -> [String] -> ([NodeElement], [GraphVertex])
        configureLoopNodes m colorS colorE nameS nameE x ys = ([NodeElement m     $ box     nameS colorS, 
                                                                NodeElement (m+1) $ ellipse nameE colorE],
                                                               GraphVertex InVertex  x (m+1) : 
                                                               map (\c -> GraphVertex OutVertex c m) ys)

        configureConstantNodes :: Int -> String -> String -> [String] -> ([NodeElement], [GraphVertex])
        configureConstantNodes m color name ys = if constNodes
                                                 then let additional = map (\(y, i) -> configure ellipse i color y [' ' : y] [y]) $ zip ys [m+1 ..]
                                                          root = configure box m color name [] $ map (' ' :) ys
                                                      in foldl (\(b1, b2) (a1, a2) -> (b1 ++ a1, b2 ++ a2)) root additional
                                                 else configure box m color name [] ys

transformElements :: (Typeable x) => [F String x] -> Bool -> ([NodeElement], [GraphVertex])
transformElements fbs constNodes = foldl (\(b1, b2) (a1, a2) -> (b1 ++ a1, b2 ++ a2)) ([], []) $ transformElements' fbs 1
    where transformElements' (f:fs) n = (\nodes -> nodes : transformElements' fs ((n +) $ length $ fst nodes)) $! transformElement f n constNodes 
          transformElements' []     _ = []

bindVertexes :: [GraphVertex] -> [GraphEdge]
bindVertexes vs = let !inVertexes  = filter ((InVertex  ==) . vertexType) vs
                      !outVertexes = filter ((OutVertex ==) . vertexType) vs
                  in concatMap (\(GraphVertex _ name inId) -> 
                                   map (\(GraphVertex _ _ outId) -> 
                                       GraphEdge (arrow name) inId outId) $ filter ((name ==) . vertexName) outVertexes) inVertexes

toGraphStructure :: (Typeable x) => [F String x] -> Bool -> GraphStructure
toGraphStructure fbs constNodes = let (nodes, vertexes) = transformElements fbs constNodes
                                      eges              = bindVertexes vertexes
                                  in GraphStructure nodes eges