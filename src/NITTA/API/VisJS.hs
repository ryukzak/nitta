{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}

{-|
Module      : NITTA.API.VisJS
Description : Graph for https://github.com/crubier/react-graph-vis/blob/master/README.md
Copyright   : (c) Dmitriy Anoshchenkov, Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.API.VisJS
    ( VisJS
    , algToVizJS
    ) where

import           Data.Aeson
import qualified Data.Map          as M
import qualified Data.Set          as S
import qualified Data.String.Utils as S
import qualified Data.Text         as T
import           Data.Typeable
import           NITTA.Functions
import           NITTA.Types
import           NITTA.Types.VisJS
import           NITTA.Utils       (oneOf)


algToVizJS fbs = let
        graphs                        = map toVizJS fbs
        GraphStructure nodes vertexes = connectGraph $ calculateIndexes graphs 0
        eges                          = bindVertexes vertexes
    in GraphStructure nodes eges
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


instance ( Function (f v x) v, Label (f v x), Var v ) => ToVizJS (f v x) where
    toVizJS f = GraphStructure
        { nodes=[ NodeElement 1 $ box "#cbbeb5" $ label f ]
        , edges=mkEdges InVertex (inputs f) ++ mkEdges OutVertex (outputs f)
        }
        where
            mkEdges t = map ( \v -> GraphVertex t (label v) 1 ) . S.elems

instance {-# OVERLAPS #-} ToVizJS (F v x) where
    toVizJS (F f) = toVizJS f

instance {-# OVERLAPS #-} ( Var v ) => ToVizJS (Loop v x) where
    toVizJS (Loop _ (O a) (I b))
        = GraphStructure
            { nodes=
                [ NodeElement 1 $ box "#6dc066" $ "prev: " ++ label b
                , NodeElement 2 $ ellipse  "#fa8072" $ "throw: " ++ label b
                ]
            , edges=GraphVertex InVertex (label b) 2
                :   map (\c -> GraphVertex OutVertex (label c) 1) (S.elems a)
            }

box     name color = NodeParam{ nodeName=name, nodeColor=color, nodeShape="box",     fontSize="20", nodeSize="30" }
ellipse name color = NodeParam{ nodeName=name, nodeColor=color, nodeShape="ellipse", fontSize="20", nodeSize="30" }
arrow   name       = EdgeParam name "2" "bottom"


-----------------------------------------------------------
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
            [
                "size" .= fontSize
            ]
        ]

instance ToJSON GraphEdge where
    toJSON GraphEdge{ edgeParam = EdgeParam { edgeName, edgeWidth, fontAllign }, inNodeId, outNodeId } = object
        [ "from"  .= outNodeId
        , "to"    .= inNodeId
        , "label" .= edgeName
        , "width" .= edgeWidth
        , "font"  .= object
            [
                "allign" .= fontAllign
            ]
        ]
