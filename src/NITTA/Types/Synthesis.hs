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
    , Sid
    , rootSynthesis
    , simpleSynthesis
    , getSynthesis
    ) where

import           Data.Aeson
import           Data.Default
import           Data.Hashable
import           GHC.Generics
import           NITTA.Compiler
import           NITTA.DataFlow   (SystemState)
import           NITTA.Utils.JSON ()


data Synthesis title tag v x t
    = Synthesis
        { sRoot     :: Maybe (Synthesis title tag v x t)
        , sModel    :: SystemState title tag v x t
        , sBranches :: [Synthesis title tag v x t]
        , sCntxs    :: [()]
        }
    deriving ( Generic )

-- |Synthesis identical.
type Sid = [Int]

getSynthesis syn [] = syn
getSynthesis Synthesis{ sBranches } (s:sid) = getSynthesis (sBranches !! s) sid

-- | Create initial synthesis.
rootSynthesis m = Synthesis
    { sRoot=Nothing
    , sModel=m
    , sBranches=[]
    , sCntxs=[]
    }


simpleSynthesis opt syn = simpleSynthesis' opt syn []

simpleSynthesis' opt syn@Synthesis{ sModel, sBranches } sid
    = let
        cStep = CompilerStep sModel opt Nothing
    in case naive' cStep of
        Just CompilerStep{ state=sModel' } -> 
            let (syn', sid') = simpleSynthesis' 
                    opt 
                    Synthesis
                        { sRoot=Just syn
                        , sModel=sModel'
                        , sBranches=[]
                        , sCntxs=[]
                        }
                    (length sBranches : sid)
            in (syn{ sBranches=sBranches ++ [ syn' ] }, sid')
        Nothing -> (syn, sid)
