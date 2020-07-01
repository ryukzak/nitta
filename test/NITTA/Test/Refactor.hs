{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE IncoherentInstances   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures -fno-warn-partial-type-signatures #-}
{-|
Module      : NITTA.Test.Refactor
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Test.Refactor
    ( refactorTests
    ) where

import qualified Data.Map                     as M
import qualified Data.Set                     as S
import           NITTA.Intermediate.Functions
import           NITTA.Intermediate.Types
import           NITTA.Model.Problems
import           NITTA.Model.TargetSystem
import           Test.Tasty                   (TestTree)
import           Test.Tasty.HUnit
import           Test.Tasty.TH


case_selfSending1 = let
        df = fsToDataFlowGraph ([ reg "a" ["b"], reg "b" ["c"] ] :: [F String Int])
        df' = refactorDecision df (ResolveDeadlock $ S.fromList ["b"])
    in df' @?= DFCluster
            [ DFLeaf $ reg "a" ["b@buf"]
            , DFLeaf $ reg "b@buf" ["b"]
            , DFLeaf $ reg "b" ["c"]
            ]


case_selfSending2 = let
        df = fsToDataFlowGraph ([ reg "a" ["b1", "b2"], reg "b1" ["c1"], reg "b2" ["c2"] ] :: [F String Int])
        df' = refactorDecision df (ResolveDeadlock $ S.fromList ["b1"])
    in df' @?= DFCluster
             [ DFLeaf $ reg "a" ["b1@buf", "b2"]
             , DFLeaf $ reg "b1@buf" ["b1"]
             , DFLeaf $ reg "b1" ["c1"]
             , DFLeaf $ reg "b2" ["c2"]
             ]


case_selfSending3 = let
        df = fsToDataFlowGraph ([ reg "a" ["b1", "b2"], reg "b1" ["c1"], reg "b2" ["c2"] ] :: [F String Int])
        df' = refactorDecision df (ResolveDeadlock $ S.fromList ["b1", "b2"])
    in df' @?= DFCluster
           [ DFLeaf $ reg "a" ["b1@buf"]
           , DFLeaf $ reg "b1@buf" ["b1", "b2"]
           , DFLeaf $ reg "b1" ["c1"]
           , DFLeaf $ reg "b2" ["c2"]
           ]


case_patchSource = do
    patch Changeset{ changeO=M.fromList [("a1@buf", S.fromList ["a1", "a2"])], changeI=M.fromList [] } (Source $ S.fromList ["a1@buf"])
        @?= (Source $ S.fromList ["a1", "a2"])
    patch Changeset{ changeO=M.fromList [("a1", S.fromList ["a1@buf"]), ("a2", S.fromList ["a1@buf"])], changeI=M.fromList [] } (Source $ S.fromList ["a1", "a2"])
        @?= (Source $ S.fromList ["a1@buf"])


case_reverseDiff = do
    reverseDiff Changeset{ changeI=M.fromList [("a", "b")], changeO=M.fromList [("c", S.fromList ["e", "f"])] }
        @?= Changeset{ changeI=M.fromList [("b", "a")], changeO=M.fromList [("e", S.fromList ["c"]), ("f", S.fromList ["c"])] }
    reverseDiff Changeset{ changeI=M.fromList [("a", "b")], changeO=M.fromList [("c", S.fromList ["e"]),("d", S.fromList ["e"])] }
        @?= Changeset{ changeI=M.fromList [("b", "a")], changeO=M.fromList [("e", S.fromList ["c", "d"])] }


refactorTests :: TestTree
refactorTests = $(testGroupGenerator)
