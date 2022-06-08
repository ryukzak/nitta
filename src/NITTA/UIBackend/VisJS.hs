{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : NITTA.UIBackend.VisJS
Description : Graph of intermediate view.
Copyright   : (c) Dmitriy Anoshchenkov, Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.UIBackend.VisJS (
    VisJS,
    algToVizJS,
    GraphStructure (..),
    GraphNode (..),
    GraphEdge (..),
) where

import Data.Aeson
import Data.Default
import Data.Set qualified as S
import GHC.Generics hiding (from, to)
import NITTA.Intermediate.Types qualified as F
import Servant.Docs
import Prelude hiding (id)

type VisJS = GraphStructure GraphEdge

data GraphEdge = GraphEdge
    { to :: Int
    , from :: Int
    , label :: String
    , edgeWidth :: String
    , fontAllign :: String
    }
    deriving (Generic)

instance Default GraphEdge where
    def =
        GraphEdge
            { to = 0
            , from = 0
            , label = ""
            , edgeWidth = "2"
            , fontAllign = "bottom"
            }

data GraphStructure v = GraphStructure
    { nodes :: [GraphNode]
    , edges :: [v]
    }
    deriving (Generic)

data GraphNode = GraphNode
    { id :: Int
    , label :: String
    , function :: String
    , history :: [String]
    , nodeColor :: String
    , nodeShape :: String
    , fontSize :: String
    , nodeSize :: String
    }
    deriving (Generic)

instance Default GraphNode where
    def =
        GraphNode
            { id = -1
            , label = ""
            , function = ""
            , history = []
            , nodeColor = "#cbbeb5"
            , nodeShape = "box"
            , fontSize = "20"
            , nodeSize = "30"
            }

data VertexType
    = InVertex
    | OutVertex
    deriving (Eq)

data GraphVertex = GraphVertex
    { vertexType :: VertexType
    , vertexName :: String
    , vertexNodeId :: Int
    }

algToVizJS fbs =
    let graphs = map toVizJS fbs
        GraphStructure nodes vertexes = connectGraph $ calculateIndexes graphs 0
        edges = bindVertexes vertexes
     in GraphStructure nodes edges
    where
        calculateIndexes [] _ = []
        calculateIndexes (GraphStructure ns vs : gss) t =
            GraphStructure
                (map (\n -> n{id = t + id n}) ns)
                (map (\v -> v{vertexNodeId = t + vertexNodeId v}) vs)
                : calculateIndexes gss (t + length ns)

        connectGraph =
            foldl
                ( \(GraphStructure n1 v1) (GraphStructure n2 v2) ->
                    GraphStructure (n1 ++ n2) (v1 ++ v2)
                )
                (GraphStructure [] [])

        bindVertexes vs =
            let inVertexes = filter ((InVertex ==) . vertexType) vs
                outVertexes = filter ((OutVertex ==) . vertexType) vs
             in concatMap
                    ( \(GraphVertex _ name inId) ->
                        map
                            (\(GraphVertex _ _ outId) -> def{to = inId, from = outId, label = name})
                            $ filter ((name ==) . vertexName) outVertexes
                    )
                    inVertexes

toVizJS F.F{fun, funHistory} =
    GraphStructure
        { nodes =
            [ def
                { label = F.label fun
                , function = show fun
                , history = map show funHistory
                }
            ]
        , edges = mkEdges InVertex (F.inputs fun) ++ mkEdges OutVertex (F.outputs fun)
        }
    where
        mkEdges t = map (\v -> GraphVertex t (F.label v) 1) . S.elems

instance ToJSON VisJS
instance ToJSON GraphNode
instance ToJSON GraphEdge

instance ToSample VisJS where
    toSamples _ =
        singleSample
            GraphStructure
                { nodes =
                    [ def{id = 1, label = "r", function = "buffer(x#0) = tmp_0#0"}
                    , def{id = 2, label = "loop(0.000000, tmp_0#0) = x#0", function = "Loop (X 0.000000) (O [x#0]) (I tmp_0#0)"}
                    ]
                , edges =
                    [ def{from = 2, to = 1, label = "x#0"}
                    , def{from = 1, to = 2, label = "tmp_0#0"}
                    ]
                }
