{-# LANGUAGE LambdaCase #-}


module NITTA.Model.Problems.RefactorSum (refactorFS) where

import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S
import           NITTA.Intermediate.Functions
import           NITTA.Intermediate.Types
import           NITTA.Model.Problems hiding ( fs )
import           NITTA.Model.Types

deleteFromPull v (Pull (O s))
    | deleted == S.empty = Nothing
    | otherwise          = Just $ Pull $ O $ deleted
        where
            deleted = S.delete v s

deleteFromPull _ (Push _ _) = error "delete only Pull"

refactorFS dfg = refactorContainers $ filterContainers $ createContainers $ dataFlowGraphToFs dfg

toAlgSub lst = map (uncurry AlgSub) $ filterSameListsTuple lst

filterSameListsTuple lst = filter (uncurry conditionSameLists) lst

conditionSameLists lst1 lst2
    | S.fromList lst1 == S.fromList lst2 = False
    | otherwise   = True

refactorContainers containers = toAlgSub $ zip containers refContainers
    where
        refContainers = map refactorContainer containers

filterContainers fs = filter (\case [_] -> False; _ -> True) fs

createContainers fs
    | length filtered == 1 = containered
    | otherwise              = map S.toList listOfSets
    where
        listOfSets = S.toList $ S.fromList [toOneContainer fs1 fs2 | fs1 <- containered , fs2 <- containered, fs1 /= fs2]
        filtered = catMaybes $ filterAddSub fs
        containered = map (\x -> [x]) filtered

filterAddSub []             = []
filterAddSub (f:fs)
    | Just Add{} <- castF f = Just f : filterAddSub fs
    | Just Sub{} <- castF f = Just f : filterAddSub fs
    | Just Acc{} <- castF f = Just f : filterAddSub fs
    | otherwise             = Nothing : filterAddSub fs

toOneContainer fs fs'
    | not $ null $ S.intersection
        (foldl1 S.union (map inputs fs))
        (foldl1 S.union (map outputs fs')) = S.fromList $ fs ++ fs'
    | not $ null $ S.intersection
        (foldl1 S.union (map inputs fs'))
        (foldl1 S.union (map outputs fs)) = S.fromList $ fs ++ fs'
    | otherwise                           = S.fromList fs


-- |Create Map String (HistoryTree (F v x)), where Key is input label and Value is FU that contain this input label
containerMapCreate fs = M.unions $
    map
    (\f ->
       foldl
       (\dataMap k ->
          M.insertWith (++) k [f] dataMap
       ) M.empty (S.toList $ inputs $ f)
    ) $ fs


-- |Takes container and refactor it, if it can be
refactorContainer [f] = [f]
refactorContainer fs = concatMap refactored fs
    where
        containerMap = containerMapCreate fs

        refactored f = concatMap
            (\o ->
                case M.findWithDefault [] o containerMap of
                    []         -> []
                    matchedFUs -> concatMap (refactorFunction f) matchedFUs
            ) (S.toList $ outputs f)


refactorFunction f' f
    | Just (Acc lst') <- castF f'
    , Just (Acc lst ) <- castF f
    , let
        multipleOutBool = (1 <) $ length $ outputs f'
        isOutInpIntersect = any
            (\case
                Push _ (I v) -> elem v $ outputs f'
                _            -> False
            ) lst
        makeRefactor = not multipleOutBool && isOutInpIntersect

    in
        makeRefactor = let
                refactorAcc _ _ (Pull o) = [Pull o]
                refactorAcc _lst' _f' _f@(Push s i@(I v))
                    | elem v $ outputs _f' = mapMaybe
                        ( \__f' ->
                            case (_f, __f') of
                                (Push Minus _, Push Plus x)     -> Just $ Push Minus x
                                (Push Minus _, Push Minus x)    -> Just $ Push Plus x
                                (Push Plus _, push@(Push _ _) ) -> Just push
                                (_, pull@(Pull _))              -> deleteFromPull v pull
                                (_, _)                          -> error "Pull can not be here"
                        ) _lst'
                    | s == Minus = [Push Minus i]
                    | s == Plus = [Push Plus i]
                refactorAcc _ _ (Push _ (I _)) = undefined
                newFS = [ packF
                        ( Acc $ concatMap (refactorAcc lst' f') lst
                        ) `asTypeOf` f ]
            in
                newFS
    | otherwise = [f, f']
