{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.UIBackend.VisJS
Description : Graph of intermediate view.
Copyright   : (c) Dmitriy Anoshchenkov, Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.UIBackend.VisJS
    ( VisJS
    , algToVizJS
    , GraphStructure(..)
    , NodeElement(..), GraphEdge(..)
    ) where

import           Data.Aeson
import qualified Data.Set                 as S
import qualified Data.String.Utils        as S
import           GHC.Generics
import qualified NITTA.Intermediate.Types as F
import           Prelude                  hiding (id)


type VisJS = GraphStructure GraphEdge

data GraphEdge = GraphEdge
        { to         :: Int
        , from       :: Int
        , label      :: String
        , edgeWidth  :: String
        , fontAllign :: String
        }
    deriving ( Generic )

data GraphStructure v = GraphStructure
        { nodes :: [NodeElement]
        , edges :: [v]
        }
    deriving ( Generic )

data NodeElement = NodeElement
        { id        :: Int
        , label     :: String
        , function  :: String
        , history   :: [ String ]
        , nodeColor :: String
        , nodeShape :: String
        , fontSize  :: String
        , nodeSize  :: String
        }
    deriving ( Generic )

data VertexType
    = InVertex
    | OutVertex
    deriving ( Eq )

data GraphVertex = GraphVertex
    { vertexType   :: VertexType
    , vertexName   :: String
    , vertexNodeId :: Int
    }



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


toVizJS F.F{ fun, funHistory } = GraphStructure
        { nodes=NodeElement
            { id=1
            , label=S.replace "\"" "" $ F.label fun
            , function=S.replace "\"" "" $ show fun
            , history=map (S.replace "\"" "" . show) funHistory
            , nodeColor="#cbbeb5"
            , nodeShape="box"
            , fontSize="20"
            , nodeSize="30"
            } : []
        , edges=mkEdges InVertex (F.inputs fun) ++ mkEdges OutVertex (F.outputs fun)
        }
    where
        mkEdges t = map ( \v -> GraphVertex t (F.label v) 1 ) . S.elems


-- *JSON Marshaling
instance ToJSON ( GraphStructure GraphEdge )
instance ToJSON NodeElement
instance ToJSON GraphEdge
