module NITTA.API.GraphConverter 
    ( GraphStructure(..)
    , NodeElement(..)
    , NodeParam(..)
    , GraphEdge(..)
    , EdgeParam(..)
    , toGraphStructure
    ) where

import           Data.Typeable
import           NITTA.BusNetwork
import           NITTA.Types
import           Data.Set              (toList)
import           Data.Function         (on)
import qualified NITTA.Functions       as F



data GraphStructure = GraphStructure {
    nodes :: [NodeElement],
    edges :: [GraphEdge]
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

box     name color = NodeParam{nodeName=name, nodeColor=color, nodeShape="box",     fontSize="20", nodeSize="30"}
ellipse name color = NodeParam{nodeName=name, nodeColor=color, nodeShape="ellipse", fontSize="20", nodeSize="30"}

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

transformElement fb n constNodes
        | Just (F.Add          (I a) (I b) (O cs))        <- F.castF fb = configure box n "#e04141" (show fb) [a, b] $ toList cs
        | Just (F.Sub          (I a) (I b) (O cs))        <- F.castF fb = configure box n "#7fffd4" (show fb) [a, b] $ toList cs
        | Just (F.Multiply     (I a) (I b) (O cs))        <- F.castF fb = configure box n "#0099bf" (show fb) [a, b] $ toList cs
        | Just (F.Division     (I a) (I b) (O cs) (O ds)) <- F.castF fb = configure box n "#fa8072" (show fb) [a, b] $ on (++) toList cs ds
        | Just (F.ShiftL       (I a) (O cs))              <- F.castF fb = configure box n "#fff68f" (show fb) [a]    $ toList cs
        | Just (F.ShiftR       (I a) (O cs))              <- F.castF fb = configure box n "#fff68f" (show fb) [a]    $ toList cs
        | Just (F.Send         (I a))                     <- F.castF fb = configure box n "#088da5" (show fb) [a]    []
        | Just (F.Receive      (O cs))                    <- F.castF fb = configure box n "#6897bb" (show fb) []     $ toList cs
        | Just (F.FramOutput _ (I a))                     <- F.castF fb = configure box n "#daa520" (show fb) [a]    []
        | Just (F.FramInput  _ (O cs))                    <- F.castF fb = configure box n "#ffa500" (show fb) []     $ toList cs
        | Just (F.Reg          (I a) (O cs))              <- F.castF fb = configure box n "#c0c0c0" (show fb) [a]    $ toList cs
        | Just (F.Loop         (X _) (O cs) (I a))        <- F.castF fb = configureLoopNodes n "#cbbeb5" "#808080" (show fb) ("next -> " ++ a) a $ toList cs
        | Just (F.Constant     (X _) (O cs))              <- F.castF fb = configureConstantNodes n "#6dc066" (show fb) $ toList cs
        | otherwise                                                     = error $ "The functional block is unsupported: " ++ show fb
    where
        configure shape m color name xs ys = ([NodeElement m $ shape name color], 
                                              map (\c -> GraphVertex InVertex  c m) xs ++ 
                                              map (\c -> GraphVertex OutVertex c m) ys)

        configureLoopNodes m colorS colorE nameS nameE x ys = ([NodeElement m     $ box     nameS colorS, 
                                                                NodeElement (m+1) $ ellipse nameE colorE],
                                                               GraphVertex InVertex  x (m+1) : 
                                                               map (\c -> GraphVertex OutVertex c m) ys)

        configureConstantNodes m color name ys = if constNodes
                                                 then let additional = map (\(y, i) -> configure ellipse i color y [' ' : y] [y]) $ zip ys [m+1 ..]
                                                          root = configure box m color name [] $ map (' ' :) ys
                                                      in foldl (\(b1, b2) (a1, a2) -> (b1 ++ a1, b2 ++ a2)) root additional
                                                 else configure box m color name [] ys

transformElements fbs constNodes = foldl (\(b1, b2) (a1, a2) -> (b1 ++ a1, b2 ++ a2)) ([], []) $ transformElements' fbs 1
    where transformElements' (f:fs) n = (\nodes -> nodes : transformElements' fs ((n +) $ length $ fst nodes)) $! transformElement f n constNodes 
          transformElements' []     _ = []

bindVertexes vs = let !inVertexes  = filter ((InVertex  ==) . vertexType) vs
                      !outVertexes = filter ((OutVertex ==) . vertexType) vs
                  in concatMap (\(GraphVertex _ name inId) -> 
                                   map (\(GraphVertex _ _ outId) -> 
                                       GraphEdge (arrow name) inId outId) $ filter ((name ==) . vertexName) outVertexes) inVertexes

toGraphStructure fbs constNodes = let (nodes, vertexes) = transformElements fbs constNodes
                                      eges              = bindVertexes vertexes
                                  in GraphStructure nodes eges