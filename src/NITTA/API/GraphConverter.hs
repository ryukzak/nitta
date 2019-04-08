{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
Module      : NITTA.API.GraphConverter
Description : Graph for https://github.com/crubier/react-graph-vis/blob/master/README.md
Copyright   : (c) Dmitriy Anoshchenkov, Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}

module NITTA.API.GraphConverter
    ( GraphStructure(..)
    , NodeElement(..)
    , NodeParam(..)
    , GraphEdge(..)
    , EdgeParam(..)
    , toGraphStructure
    ) where

import           Data.Set          (elems, toList)
import qualified Data.String.Utils as S
import           Data.Typeable
import           NITTA.Functions
import           NITTA.Types
import           NITTA.Utils       (oneOf)



data GraphStructure v = GraphStructure {
    nodes :: [NodeElement],
    edges :: [v]
}


configureNode nodeParam xs ys = GraphStructure
    { nodes=[ NodeElement 1 nodeParam ]
    , edges=map (\c -> GraphVertex InVertex  (label c) 1) xs
         ++ map (\c -> GraphVertex OutVertex (label c) 1) ys
    }

configureLoopNodes nodeParamS nodeParamE x ys = GraphStructure
    { nodes=[ NodeElement 1 nodeParamS, NodeElement 2 nodeParamE ]
    , edges=GraphVertex InVertex (label x) 2
          : map (\c -> GraphVertex OutVertex (label c) 1) ys
    }


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

box     name color = NodeParam{ nodeName=name, nodeColor=color, nodeShape="box",     fontSize="20", nodeSize="30" }
ellipse name color = NodeParam{ nodeName=name, nodeColor=color, nodeShape="ellipse", fontSize="20", nodeSize="30" }

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

data VertexType = InVertex | OutVertex deriving Eq
data GraphVertex = GraphVertex {
    vertexType   :: VertexType,
    vertexName   :: String,
    vertexNodeId :: Int
}




class ToGraphStructure f e | f -> e where
    toGraphStructure :: f -> GraphStructure e

instance (Var v, Typeable x, Show x) => ToGraphStructure (F v x) GraphVertex where
    toGraphStructure fb | insideOut fb = configureLoopNodes (box "#6dc066" $ "prev: " ++ label (head outVars))
                                                            (ellipse  "#fa8072" $ "throw: " ++ label inVar)
                                                            inVar outVars
                        | otherwise    = configureNode (box "#cbbeb5" $ label fb) inVars outVars
        where
            inVars  = toList $ inputs fb
            outVars = toList $ outputs fb
            inVar   = (\(x:xs) -> if null xs
                                  then x
                                  else error "Input vars of loop node greater than one") inVars

instance ToGraphStructure t GraphVertex => ToGraphStructure [t] GraphEdge where
    toGraphStructure fbs = let graphs                        = map toGraphStructure fbs
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

            bindVertexes vs = let inVertexes  = filter ((InVertex  ==) . vertexType) vs
                                  outVertexes = filter ((OutVertex ==) . vertexType) vs
                              in concatMap (\(GraphVertex _ name inId) ->
                                               map (\(GraphVertex _ _ outId) ->
                                                       GraphEdge (arrow name) inId outId)
                                                   $ filter ((name ==) . vertexName)
                                                            outVertexes) inVertexes
