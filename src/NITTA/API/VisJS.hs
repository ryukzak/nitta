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
import           NITTA.Utils       (oneOf)

type VisJS = GraphStructure GraphEdge

data GraphStructure v = GraphStructure 
    { nodes :: [NodeElement]
    , edges :: [v]
    }

data NodeElement = NodeElement 
    { nodeId    :: Int
    , nodeParam :: NodeParam
    }

data NodeParam = NodeParam 
    { nodeName  :: String
    , nodeColor :: String
    , nodeShape :: String
    , fontSize  :: String
    , nodeSize  :: String
    }


class ToVizJS f where
    toVizJS :: f -> GraphStructure GraphVertex


box     name color = NodeParam{ nodeName=name, nodeColor=color, nodeShape="box",     fontSize="20", nodeSize="30" }
ellipse name color = NodeParam{ nodeName=name, nodeColor=color, nodeShape="ellipse", fontSize="20", nodeSize="30" }

data EdgeParam = EdgeParam 
    { edgeName   :: String
    , edgeWidth  :: String
    , fontAllign :: String
    }

arrow name = EdgeParam name "2" "bottom"

data GraphEdge = GraphEdge 
    { edgeParam :: EdgeParam
    , inNodeId  :: Int
    , outNodeId :: Int
    }

data VertexType 
    = InVertex 
    | OutVertex 
    deriving ( Eq )

data GraphVertex = GraphVertex 
    { vertexType   :: VertexType
    , vertexName   :: String
    , vertexNodeId :: Int
    }




instance ( Function (f v x) v, Show (f v x), Var v, Typeable x, Show x ) => ToVizJS (f v x) where
    toVizJS f = GraphStructure
        { nodes=[ NodeElement 1 $ box "#cbbeb5" $ label f ]
        , edges=map (\c -> GraphVertex InVertex  (label c) 1) (S.elems $ inputs f)
             ++ map (\c -> GraphVertex OutVertex (label c) 1) (S.elems $ outputs f)
        }


instance {-# OVERLAPS #-} ( Var v, Typeable x, Show x ) => ToVizJS (Loop v x) where
    toVizJS fb@(Loop _ (O a) (I b))
        = GraphStructure
            { nodes=
                [ NodeElement 1 $ box "#6dc066" $ "prev: " ++ label (oneOf a)
                , NodeElement 2 $ ellipse  "#fa8072" $ "throw: " ++ label b
                ]
            , edges=GraphVertex InVertex (label b) 2
                :   map (\c -> GraphVertex OutVertex (label c) 1) (S.elems a)
            }



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
