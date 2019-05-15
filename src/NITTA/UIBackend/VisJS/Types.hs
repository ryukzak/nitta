{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.UIBackend.VisJS.Types
Description :
Copyright   : (c) Dmitriy Anoshchenkov, Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.UIBackend.VisJS.Types
    ( VisJS, ToVizJS(..)
    , GraphStructure(..)
    , NodeElement(..), NodeParam(..), GraphEdge(..), EdgeParam(..)
    , GraphVertex(..), VertexType(..)
    , arrow, box, ellipse
    ) where

type VisJS = GraphStructure GraphEdge

data GraphEdge = GraphEdge
    { edgeParam :: EdgeParam
    , inNodeId  :: Int
    , outNodeId :: Int
    }

data EdgeParam = EdgeParam
    { edgeName   :: String
    , edgeWidth  :: String
    , fontAllign :: String
    }

class ToVizJS f where
    toVizJS :: f -> GraphStructure GraphVertex

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

data VertexType
    = InVertex
    | OutVertex
    deriving ( Eq )

data GraphVertex = GraphVertex
    { vertexType   :: VertexType
    , vertexName   :: String
    , vertexNodeId :: Int
    }

box     name color = NodeParam{ nodeName=name, nodeColor=color, nodeShape="box",     fontSize="20", nodeSize="30" }
ellipse name color = NodeParam{ nodeName=name, nodeColor=color, nodeShape="ellipse", fontSize="20", nodeSize="30" }
arrow   name       = EdgeParam name "2" "bottom"
