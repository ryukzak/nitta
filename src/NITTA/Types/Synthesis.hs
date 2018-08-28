{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-orphans #-}

{-|
Module      : NITTA.Types.Synthesis
Description : Types to describe synthesis process
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}

module NITTA.Types.Synthesis
    ( Synthesis(..)
    , SynthesisNode
    , Nid(..)
    , rootSynthesis
    , simpleSynthesis
    , getSynthesis
    , simpleSynthesisOneStepAt
    , simpleSynthesisAt
    , nids
    ) where

import           Data.Tree
import           GHC.Generics
import           NITTA.Compiler
import           NITTA.DataFlow   (SystemState)
import           NITTA.Utils.JSON ()


type SynthesisNode title tag v x t = Tree (Synthesis title tag v x t)

data Synthesis title tag v x t
    = Synthesis
        { sModel :: SystemState title tag v x t
        , sCntx  :: [String]
        }
    deriving ( Generic )

-- |Synthesis identical.
newtype Nid = Nid [Int]

nids n = inner [] n
    where
        inner is Node{ subForest } = Node
            { rootLabel=Nid $ reverse is
            , subForest=zipWith (\i subN -> inner (i:is) subN) [0..] subForest
            }

getSynthesis (Nid []) n                     = n
getSynthesis (Nid (i:is)) Node{ subForest } = getSynthesis (Nid is) (subForest !! i)

-- | Create initial synthesis.
rootSynthesis m = Node
    { rootLabel=Synthesis
        { sModel=m
        , sCntx=[]
        }
    , subForest=[]
    }


simpleSynthesis opt n@Node{ rootLabel=rl@Synthesis{ sCntx } }
    = simpleSynthesis' opt n{ rootLabel=rl{ sCntx="simple":sCntx } } $ Nid []

simpleSynthesis' opt n@Node{ subForest } nid
    = case simpleSynthesisOneStep opt nid n of
        Just (Node{ subForest=subForest' }, _) ->
            let subN = last subForest'
                (subN', Nid subIs) = simpleSynthesis' opt subN nid
            in (n{ subForest=subForest ++ [ subN' ] }, Nid (length subForest : subIs) )
        Nothing -> (n, nid)

simpleSynthesisOneStep opt (Nid is) n@Node{ rootLabel=Synthesis{ sModel }, subForest }
    = let
        cStep = CompilerStep sModel opt Nothing
    in case naive' cStep of
        Just CompilerStep{ state=sModel' } ->
            let subN = Node
                    { rootLabel=Synthesis
                        { sModel=sModel'
                        , sCntx=["simple1"]
                        }
                    , subForest=[]
                    }
            in Just (n{ subForest=subForest ++ [ subN ] }, Nid (length subForest : is))
        Nothing -> Nothing


simpleSynthesisAt opt nid n
    = updateSynthesis (Just . simpleSynthesis opt) nid n

simpleSynthesisOneStepAt opt nid
    = updateSynthesis (simpleSynthesisOneStep opt nid)

updateSynthesis f nid0 n0 = inner nid0 n0
    where
        inner (Nid []) n = f n
        inner (Nid (i:is)) n@Node{ subForest }
            = let
                (before, subN : after) = splitAt i subForest
            in case inner (Nid is) subN of
                Just (subN', is') -> Just (n{ subForest=before ++ (subN' : after) }, is')
                Nothing -> Nothing
