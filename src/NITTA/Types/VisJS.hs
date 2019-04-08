{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Types.VisJS
Description :
Copyright   : (c) Dmitriy Anoshchenkov, Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Types.VisJS where

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