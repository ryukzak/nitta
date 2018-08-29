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
    , simpleSynthesisAtManual
    , simpleSynthesisOneStepAt
    , simpleSynthesisAt
    , nids
    ) where

import           Data.List.Split
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
nidSep = ':'

instance Show Nid where
    show (Nid []) = [nidSep]
    show (Nid is) = show' is
        where
            show' []     = ""
            show' (x:xs) = nidSep : show x ++ show' xs

instance Read Nid where
    readsPrec _ [x] | x == nidSep    = [(Nid [], "")]
    readsPrec d (x:xs)
        | x == nidSep
        , let is = map (readsPrec d) $ splitOn [nidSep] xs
        , all (not . null) is
        = [(Nid $ map fst $ concat is, "")]
    readsPrec _ _ = []


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


simpleSynthesis opt nRoot@Node{ rootLabel=rl@Synthesis{ sCntx } }
    = inner nRoot{ rootLabel=rl{ sCntx="simple":sCntx } } $ Nid []
    where
        inner n@Node{ subForest } nid
            = case simpleSynthesisOneStep opt n of
                Just (Node{ subForest=subForest' }, _) ->
                    let subN = last subForest'
                        (subN', Nid subIs) = inner subN nid
                    in (n{ subForest=subForest ++ [ subN' ] }, Nid (length subForest : subIs) )
                Nothing -> (n, nid)

simpleSynthesisOneStep opt n@Node{ rootLabel=Synthesis{ sModel }, subForest }
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
            in Just (n{ subForest=subForest ++ [ subN ] }, Nid [length subForest])
        Nothing -> Nothing

simpleSynthesisManual opt m n@Node{ rootLabel=Synthesis{ sModel }, subForest }
    = let
        cStep = CompilerStep sModel opt Nothing
    in case naive'' cStep m of
        Just CompilerStep{ state=sModel' } ->
            let subN = Node
                    { rootLabel=Synthesis
                        { sModel=sModel'
                        , sCntx=["manual"]
                        }
                    , subForest=[]
                    }
            in Just (n{ subForest=subForest ++ [ subN ] }, Nid [length subForest])
        Nothing -> Nothing

simpleSynthesisAtManual opt nid n m
    = updateSynthesis (simpleSynthesisManual opt m) nid n

simpleSynthesisAt opt nid n
    = updateSynthesis (Just . simpleSynthesis opt) nid n

simpleSynthesisOneStepAt opt nid n
    = updateSynthesis (simpleSynthesisOneStep opt) nid n

updateSynthesis f rootNid rootN = inner rootNid rootN
    where
        inner (Nid []) n = f n
        inner (Nid (i:is)) n@Node{ subForest }
            = let
                (before, subN : after) = splitAt i subForest
            in case inner (Nid is) subN of
                Just (subN', Nid is') -> Just (n{ subForest=before ++ (subN' : after) }, Nid (i:is'))
                Nothing -> Nothing
