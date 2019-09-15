{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
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
    ( VisJS
    , GraphStructure(..)
    , NodeElement(..), GraphEdge(..)
    , GraphVertex(..), VertexType(..)
    ) where

import           GHC.Generics

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
