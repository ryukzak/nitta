{-# LANGUAGE DuplicateRecordFields #-}
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
import qualified Data.Map                     as M
import qualified Data.Set                     as S
import qualified Data.String.Utils            as S
import qualified Data.Text                    as T
import           Data.Typeable
import qualified NITTA.Intermediate.Functions as F
import qualified NITTA.Intermediate.Types     as F
import           NITTA.UIBackend.VisJS.Types
import           NITTA.Utils                  (oneOf)
import           Prelude                      hiding (id)


algToVizJS fbs = let
        graphs                        = map toVizJS fbs
        GraphStructure nodes vertexes = connectGraph $ calculateIndexes graphs 0
        edges                         = bindVertexes vertexes
    in GraphStructure nodes edges
    where
        calculateIndexes []                           _ = []
        calculateIndexes (GraphStructure ns vs : gss) t =
            GraphStructure (map (\ n -> n{id = t + id n}) ns)
                                (map (\ v -> v{vertexNodeId = t + vertexNodeId v}) vs)
            : calculateIndexes gss (t + length ns)

        connectGraph = foldl (\ (GraphStructure n1 v1) (GraphStructure n2 v2)
                                    -> GraphStructure (n1 ++ n2) (v1 ++ v2))
                                (GraphStructure [] [])

        bindVertexes vs = let
                inVertexes  = filter ((InVertex  ==) . vertexType) vs
                outVertexes = filter ((OutVertex ==) . vertexType) vs
            in concatMap
                    ( \(GraphVertex _ name inId) ->
                        map ( \(GraphVertex _ _ outId) -> GraphEdge
                                { to=inId
                                , from=outId
                                , label=name
                                , edgeWidth="2"
                                , fontAllign="bottom"
                                })
                            $ filter ((name ==) . vertexName) outVertexes )
                    inVertexes


toVizJS (F.F f) = GraphStructure
        { nodes=NodeElement
            { id=1
            , label=S.replace "\"" "" $ F.label f
            , nodeColor="#cbbeb5"
            , nodeShape="box"
            , fontSize="20"
            , nodeSize="30"
            } : []
        , edges=mkEdges InVertex (F.inputs f) ++ mkEdges OutVertex (F.outputs f)
        }
    where
        mkEdges t = map ( \v -> GraphVertex t (F.label v) 1 ) . S.elems


-- *JSON Marshaling
instance ToJSON ( GraphStructure GraphEdge )
instance ToJSON NodeElement
instance ToJSON GraphEdge
