-- All extensions should be enabled explicitly due to doctest in this module.
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

module NITTA.Model.Problems.Refactor.OptimizeLut
  ( OptimizeLut (..),
    OptimizeLutProblem (..),
  )
where

import Data.List qualified as L
import Data.Map qualified as M
import Data.Set qualified as S
import Debug.Trace
import GHC.Generics
import NITTA.Intermediate.Functions
import NITTA.Intermediate.Types
import Data.Maybe

data OptimizeLut v x = OptimizeLut
  { rOld :: [F v x],
    rNew :: [F v x]
  }
  deriving (Generic, Show, Eq)

class OptimizeLutProblem u v x | u -> v x where
  -- | Function takes algorithm in 'DataFlowGraph' and return list of 'Refactor' that can be done
  optimizeLutOptions :: u -> [OptimizeLut v x]
  optimizeLutOptions _ = []

  -- | Function takes 'OptimizeLut' and modify 'DataFlowGraph'
  optimizeLutDecision :: u -> OptimizeLut v x -> u
  optimizeLutDecision _ _ = error "not implemented"

instance (Var v, Val x) => OptimizeLutProblem [F v x] v x where

  optimizeLutOptions fs = trace ("optimizeLutOptions: input functions = " ++ show fs) $
    let 
        supportedFunctions = filter isSupportedByLut fs
        
        rNew = if not (null supportedFunctions) 
          && isOptimizationNeeded supportedFunctions
                  then optimizeCluster supportedFunctions fs
                  else []
        
        --result = ([OptimizeLut {rOld = supportedFunctions, rNew} | not (null rNew)])
        result = [OptimizeLut {rOld = supportedFunctions, rNew} 
                 | not (null rNew) && S.fromList supportedFunctions /= S.fromList rNew]
        
    in trace ("optimizeLutOptions: supportedFunctions = " ++ show supportedFunctions
      ++ "\nresult: " ++ show result) result

  optimizeLutDecision fs OptimizeLut {rOld, rNew} = do
      let r = deleteExtraLuts $ (fs L.\\ rOld) <> rNew
      trace ("optimizeLutDecision: " ++ show r ) r


deleteExtraLuts fs =
  L.nub
    [ f1
      | f1 <- fs
      , f2 <- fs
      , f1 /= f2
      , not $ S.null (variables f1 `S.intersection` variables f2)
    ]

isOptimizationNeeded fs = countLuts fs > 1 || hasLogicFunctions fs where
  hasLogicFunctions fs = any isLogicFunction fs


  isLogicFunction f = case castF f of
    Just LogicAnd {} -> trace "isLogicFunction: LogicAnd" True
    Just LogicOr {}  -> trace "isLogicFunction: LogicOr" True
    Just LogicNot {} -> trace "isLogicFunction: LogicNot" True
    _ -> False

  isLut f = case castF f of
    Just lut@(LUT {}) -> trace ("isLut passed: " ++ show lut) True
    _ -> False

  countLuts fs = do 
    let res = length $ filter isLut fs
    trace ("countLuts = " ++ show res) res
  
selectClusters fs = [[f] | f <- fs, isSupportedByLut f]

isSupportedByLut f
  | Just LogicAnd {} <- castF f = True
  | Just LogicOr {} <- castF f = True
  | Just LogicNot {} <- castF f = True
  -- | Just LUT{} <- castF f = True
  | otherwise = False



optimizeCluster allFunctions _ =
  let
      withLogicToLut = trace ("before: " ++ show allFunctions) $ 
        concat $ mapMaybe replaceLogicWithLut allFunctions

      luts = trace ("after: " ++ show withLogicToLut) $ 
        mapMaybe castF withLogicToLut


      mergedLut = mergeAllTables luts

      result = luts
        -- case mergedLut of
        --           Just lut -> replaceLuts withLogicToLut luts [lut]
        --           Nothing -> withLogicToLut
  in trace ("optimizeCluster result: " ++ show result) result



replaceLuts allFunctions oldLuts newLuts =
  let oldFunctions = map packF oldLuts
      newFunctions = map packF newLuts
  in (allFunctions L.\\ oldFunctions) ++ newFunctions


replaceLogicWithLut f
  | Just LogicAnd {} <- castF f,
    S.size (inputs f) > 1,
    not (S.null $ outputs f) =
      trace ("Replacing AND with LUT: " ++ show f) $
        Just [ packF $ andLut (S.elemAt 0 $ inputs f) (S.elemAt 1 $ inputs f) (outputs f) ]
  | Just LogicOr {} <- castF f,
    S.size (inputs f) > 1,
    not (S.null $ outputs f) =
      trace ("Replacing OR with LUT: " ++ show f) $
        Just [ packF $ orLut (S.elemAt 0 $ inputs f) (S.elemAt 1 $ inputs f) (outputs f) ]
  | Just LogicNot {} <- castF f,
    S.size (inputs f) > 0,
    not (S.null $ outputs f) =
      trace ("Replacing NOT with LUT: " ++ show f) $
        Just [ packF $ notLut (S.elemAt 0 $ inputs f) (outputs f) ]
  | otherwise =
      trace ("No replacement for: " ++ show f) Nothing


mergeAllTables [] = Nothing
mergeAllTables [lut] = Just lut
mergeAllTables luts =
  let
      validLuts = mapMaybe castF luts
      
      result = if null validLuts
                  then Nothing
                  else
                    let
                        mergedTable = foldr1 mergeTables validLuts
                    in Just (packF mergedTable)
  in result



mergeTables :: Ord v => LUT v x1 -> LUT v x2 -> LUT v x3
mergeTables (LUT tbl1 ins1 outs1) (LUT tbl2 ins2 outs2) =
    LUT tblMerged insMerged outsMerged
  where

    tblMerged = M.unionWith mergeValues
        (M.mapKeys (False :) tbl1)
        (M.mapKeys (True :) tbl2)

    insMerged = mergeInputs ins1 ins2
    outsMerged = mergeOutputs outs1 outs2

    -- cyclicVars = S.fromList (map (\(I v) -> v) insMerged) `S.intersection` getOutputs outsMerged

    -- filteredIns = [I v | I v <- insMerged, v `S.notMember` cyclicVars]
    -- filteredOuts = O $ S.filter (`S.notMember` cyclicVars) (getOutputs outsMerged)

    mergeValues val1 val2
      | val1 == val2 = val1
      | otherwise = error "Conflicting values in LUT tables!"

    mergeInputs ins1 ins2 = L.nub (ins1 ++ ins2)
    mergeOutputs (O outs1) (O outs2) = O $ S.union outs1 outs2

    getOutputs (O outs) = outs


andLut a b outputs =
  packF $
    LUT
      ( M.fromList
          [ ([f, f], f),
            ([f, t], f),
            ([t, f], f),
            ([t, t], t)
          ]
      )
      [I a, I b]
      (O outputs)
  where
    (t, f) = (True, False)

orLut a b outputs =
  packF $
    LUT
      ( M.fromList
          [ ([f, f], f),
            ([f, t], t),
            ([t, f], t),
            ([t, t], t)
          ]
      )
      [I a, I b]
      (O outputs)
  where
    (t, f) = (True, False)

notLut a outputs =
  packF $
    LUT
      ( M.fromList
          [ ([f], t),
            ([t], f)
          ]
      )
      [I a]
      (O outputs)
  where
    (t, f) = (True, False)
