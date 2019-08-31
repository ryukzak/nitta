{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|
Module      : NITTA.UIBackend.VisJS
Description : Graph for https://github.com/crubier/react-graph-vis/blob/master/README.md
Copyright   : (c) Dmitriy Anoshchenkov, Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.UIBackend.VisJS
    ( VisJS
    , algToVizJS
    ) where

import           Data.Aeson
import qualified Data.Map                    as M
import qualified Data.Set                    as S
import qualified Data.String.Utils           as S
import qualified Data.Text                   as T
import           Data.Typeable
import           NITTA.UIBackend.VisJS.Types
import           NITTA.Utils                 (oneOf)


algToVizJS fbs = let
        graphs                        = map toVizJS fbs
        GraphStructure nodes vertexes = connectGraph $ calculateIndexes graphs 0
        edges                         = bindVertexes vertexes
    in GraphStructure nodes edges
    where
        calculateIndexes []                           _ = []
        calculateIndexes (GraphStructure ns vs : gss) t =
            GraphStructure (map (\ n -> n{nodeId = t + nodeId n}) ns)
                                (map (\ v -> v{vertexNodeId = t + vertexNodeId v}) vs)
            : calculateIndexes gss (t + length ns)

        connectGraph = foldl (\ (GraphStructure n1 v1) (GraphStructure n2 v2)
                                    -> GraphStructure (n1 ++ n2) (v1 ++ v2))
                                (GraphStructure [] [])

        bindVertexes vs = let
                inVertexes  = filter ((InVertex  ==) . vertexType) vs
                outVertexes = filter ((OutVertex ==) . vertexType) vs
            in concatMap (\(GraphVertex _ name inId) ->
                            map (\(GraphVertex _ _ outId) ->
                                    GraphEdge (arrow name) inId outId)
                                $ filter ((name ==) . vertexName)
                                        outVertexes) inVertexes



-- *JSON Marshaling

instance ToJSON (GraphStructure GraphEdge) where
    toJSON GraphStructure{ nodes, edges } = object
        [ "nodes" .= nodes
        , "edges" .= edges
        ]

instance ToJSON NodeElement where
    toJSON NodeElement{ nodeId, nodeParam = NodeParam{ nodeName, nodeColor, nodeShape, fontSize, nodeSize } } = object
        [ "id"    .= nodeId
        , "label" .= nodeName
        , "color" .= nodeColor
        , "shape" .= nodeShape
        , "size"  .= nodeSize
        , "font"  .= object
            [ "size" .= fontSize
            ]
        ]

instance ToJSON GraphEdge where
    toJSON GraphEdge{ edgeParam = EdgeParam { edgeName, edgeWidth, fontAllign }, inNodeId, outNodeId } = object
        [ "from"  .= outNodeId
        , "to"    .= inNodeId
        , "label" .= edgeName
        , "width" .= edgeWidth
        , "font"  .= object
            [ "allign" .= fontAllign
            ]
        ]
