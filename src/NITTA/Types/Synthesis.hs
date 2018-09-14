{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
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
    , SynthesisStatus(..)
    , rootSynthesis
    , Nid(..)
    , nidsTree
    , SynthesisSetup(..)
    , SynthesisStep(..)
      -- *Processing SynthesisTree
    , getSynthesisNode
    , getSynthesis
    , update
    , apply
    , recApply
    , targetProcessDuration
      -- *Synhesis context
    , SynthCntxCls(..)
    , SynthCntx(..)
    , comment
    , setCntx
    , findCntx
    ) where

import           Data.Default
import           Data.List.Split
import qualified Data.Map        as M
import           Data.Tree
import           Data.Typeable   (Typeable, cast)
import           GHC.Generics
import           NITTA.DataFlow  (ModelState (..))
import           NITTA.Types     (nextTick, process)


type SynthesisTree title tag v x t = Tree (Synthesis title tag v x t)

data SynthesisStatus
    = InProgress
    | Finished
    | DeadEnd
    deriving ( Show, Generic, Eq )

data Synthesis title tag v x t
    = Synthesis
        { sModel  :: ModelState title tag v x t
        , sCntx   :: [SynthCntx]
        , sStatus :: SynthesisStatus
        , sCache  :: M.Map SynthesisStep Int
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


nidsTree n = inner [] n
    where
        inner is Node{ subForest } = Node
            { rootLabel=Nid $ reverse is
            , subForest=zipWith (\i subN -> inner (i:is) subN) [0..] subForest
            }



-- | Настройки процесса синтеза.
newtype SynthesisSetup
    = Simple
        { -- | Порог колличества вариантов, после которого пересылка данных станет приоритетнее, чем
          -- привязка функциональных блоков.
          threshhold :: Int
        } 
    deriving ( Generic, Show, Eq, Ord )

instance Default SynthesisSetup where
    def = Simple
        { threshhold=1000
        }

data SynthesisStep = SynthesisStep
    { setup :: SynthesisSetup
    , ix    :: Int
    }
    deriving ( Show, Eq, Ord )



-- |Create initial synthesis.
rootSynthesis m = Node
    { rootLabel=Synthesis
        { sModel=m
        , sCntx=[]
        , sStatus=InProgress
        , sCache=def
        }
    , subForest=[]
    }

targetProcessDuration Frame{ processor } = nextTick $ process processor
targetProcessDuration _                  = undefined


-- *Synthesis context

class SynthCntxCls a where
    data SynthCntx' a :: *

data SynthCntx = forall a. ( Show (SynthCntx' a), Typeable (SynthCntx' a) ) => SynthCntx (SynthCntx' a)

instance Show SynthCntx where
    show (SynthCntx e) = show e

findCntx [] = Nothing
findCntx (SynthCntx c : cs)
    | Just cntx <- cast c = Just cntx
    | otherwise = findCntx cs

setCntx newCntx [] = [SynthCntx newCntx]
setCntx newCntx (SynthCntx c : cs)
    | Just c' <- cast c
    , let _ = c' `asTypeOf` newCntx
    = SynthCntx newCntx : cs
    | otherwise
    = SynthCntx c : setCntx newCntx cs


data Comment

instance SynthCntxCls Comment where
    data SynthCntx' Comment = Comment String
        deriving ( Show )

comment = SynthCntx . Comment



-- *Processing


-- |Get specific by @nid@ node from a synthesis tree.
getSynthesisNode (Nid []) n                     = n
getSynthesisNode nid@(Nid (i:is)) Node{ subForest }
    | length subForest <= i = error $ "getSynthesisNode: wrong nid: " ++ show nid
    | otherwise = getSynthesisNode (Nid is) (subForest !! i)

getSynthesis nid n = rootLabel $ getSynthesisNode nid n



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
recApply rec step nRoot = inner nRoot
    where
        -- inner n@Node{ rootLabel, subForest }
        --     = case apply rec step n of
        --         Just (n'@Node{ rootLabel=rootLabel', subForest=subForest' }, _) ->
        --             let i = length subForest
        --                 sub = last subForest'
        --                 (sub', Nid subIxs) = inner sub
        --             in (n{ subForest=subForest ++ [sub'] }, Nid (i : subIxs) )
        --         Nothing -> (n, Nid [])
        inner n@Node{ rootLabel, subForest }
            = case rec step rootLabel of
                Just sub ->
                    let (subN', Nid subIxs) = inner Node{ rootLabel=sub, subForest=[] }
                    in (n{ subForest=subForest ++ [subN'] }, Nid (length subForest : subIxs) )
                Nothing -> (n, Nid [])


-- |Apply @f@ to a synthesis in a node.
apply f step n@Node{ rootLabel=rootLabel@Synthesis{ sCache }, subForest }
    = case sCache M.!? step of 
        Just i -> Just (n, Nid [i])
        Nothing -> case f step rootLabel of
            Just sub ->
                let subN = Node
                        { rootLabel=sub
                        , subForest=[]
                        }
                    i = length subForest
                    rootLabel' = rootLabel{ sCache=M.insert step i sCache }
                in Just (n{ rootLabel=rootLabel', subForest=subForest ++ [subN]}, Nid [i])
            Nothing -> Nothing
