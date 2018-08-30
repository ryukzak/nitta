{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
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
    ( SynthesisTree
    , Synthesis(..)
    , rootSynthesis
    , Nid(..)
    , nids
      -- *Processing SynthesisTree
    , getSynthesis
    , update
    , apply
    , recApply
      -- *Synhesis context
    , comment
    ) where

import           Data.List.Split
import           Data.Tree
import           GHC.Generics
import           NITTA.DataFlow  (SystemState)


type SynthesisTree title tag v x t = Tree (Synthesis title tag v x t)

data Synthesis title tag v x t
    = Synthesis
        { sModel :: SystemState title tag v x t
        , sCntx  :: [Cntx]
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


-- |Create initial synthesis.
rootSynthesis m = Node
    { rootLabel=Synthesis
        { sModel=m
        , sCntx=[]
        }
    , subForest=[]
    }



-- *Synthesis context

class CntxCls a where
    data Cntx' a :: *

data Cntx = forall a. ( Show (Cntx' a) ) => Cntx (Cntx' a)

instance Show Cntx where
    show (Cntx e) = show e



data Comment

instance CntxCls Comment where
    data Cntx' Comment = Comment String
        deriving ( Show )

comment = Cntx . Comment



-- *Processing

-- |Get specific by @nid@ node from a synthesis tree.
getSynthesis (Nid []) n                     = n
getSynthesis (Nid (i:is)) Node{ subForest } = getSynthesis (Nid is) (subForest !! i)


-- |Update specific by @nid@ node in a synthesis tree by the @f@.
update f nid rootN = inner nid rootN
    where
        inner (Nid []) n = f n
        inner (Nid (i:is)) n@Node{ subForest }
            = let
                (before, subN : after) = splitAt i subForest
            in case inner (Nid is) subN of
                Just (subN', Nid is') -> Just (n{ subForest=before ++ (subN' : after) }, Nid (i:is'))
                Nothing -> Nothing


-- |Recursively apply @rec@ to a synthesis while it is applicable (returning Just value).
recApply rec nRoot = inner nRoot
    where
        inner n@Node{ rootLabel, subForest }
            = case rec rootLabel of
                Just rootLabel' ->
                    let (subN', Nid subIs) = inner Node{ rootLabel=rootLabel', subForest=[] }
                    in (n{ subForest=subForest ++ [ subN' ] }, Nid (length subForest : subIs) )
                Nothing -> (n, Nid [])


-- |Apply @f@ to a synthesis in a node.
apply f n@Node{ rootLabel, subForest }
    = case f rootLabel of
        Just rootLabel' ->
            let subN = Node
                    { rootLabel=rootLabel'
                    , subForest=[]
                    }
            in Just (n{ subForest=subForest ++ [ subN ] }, Nid [length subForest])
        Nothing -> Nothing
