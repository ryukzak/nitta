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
    , Nid
    , rootSynthesis
    , simpleSynthesis
    , getSynthesis
    , simpleSynthesisOneStepAt
    ) where

import           GHC.Generics
import Data.Tree
import           NITTA.Compiler
import           NITTA.DataFlow   (SystemState)
import           NITTA.Utils.JSON ()


type SynthesisNode title tag v x t = Tree (Synthesis title tag v x t)

data Synthesis title tag v x t
    = Synthesis
        { sModel    :: SystemState title tag v x t
        , sCntx     :: Maybe ()
        }
    deriving ( Generic )

-- |Synthesis identical.
type Nid = [Int]

getSynthesis [] n = n
getSynthesis (s:sid) Node{ subForest } = getSynthesis sid (subForest !! s)

-- | Create initial synthesis.
rootSynthesis m = Node
    { rootLabel=Synthesis
        { sModel=m
        , sCntx=Nothing
        }
    , subForest=[]
    }


simpleSynthesis opt syn = simpleSynthesis' opt syn []

simpleSynthesis' opt n sid
    = case simpleSynthesisOneStep opt sid n of
        Just (Node{ subForest }, _) -> 
            let subN = last subForest
                (subN', subSid') = simpleSynthesis' opt subN sid
            in (n{ subForest=subForest ++ [ subN' ] }, length subForest : subSid')
        Nothing -> (n, sid)

simpleSynthesisOneStep opt sid n@Node{ rootLabel=Synthesis{ sModel }, subForest }
    = let
        cStep = CompilerStep sModel opt Nothing
    in case naive' cStep of
        Just CompilerStep{ state=sModel' } -> 
            let subN = Node
                    { rootLabel=Synthesis
                        { sModel=sModel'
                        , sCntx=Nothing
                        }
                    , subForest=[]
                    }
            in Just (n{ subForest=subForest ++ [ subN ] }, length subForest : sid)
        Nothing -> Nothing



simpleSynthesisOneStepAt opt sid
    = updateSynthesis (simpleSynthesisOneStep opt sid)

-- f :: SynthesisNode -> Maybe (SynthesisNode, Nid)
updateSynthesis f sid0 n0 = inner sid0 n0
    where
        inner [] n = f n
        inner (s:subSid) n@Node{ subForest }
            = let
                (before, subN : after) = splitAt s subForest
            in case inner subSid subN of
                Just (subN', subSid') -> Just (n{ subForest=before ++ (subN' : after) }, subSid')
                Nothing -> Nothing
