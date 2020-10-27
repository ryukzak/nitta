{-# LANGUAGE LambdaCase #-}


module NITTA.Model.Problems.RefactorSum (refactorSum) where

import qualified Data.List as L
import qualified Data.Map as M
import           Data.Maybe
import qualified Data.Set as S
import           NITTA.Intermediate.Functions
import           NITTA.Intermediate.Types
import           NITTA.Model.Problems
import           NITTA.Model.Types



filterAddSub []             = []
filterAddSub (hf:hfs)
    | Just Add{} <- castF $ getF hf = Just hf : filterAddSub hfs
    | Just Sub{} <- castF $ getF hf = Just hf : filterAddSub hfs
    | Just Acc{} <- castF $ getF hf = Just hf : filterAddSub hfs
    | otherwise             = Nothing : filterAddSub hfs


-- |Check intersections between input and output, and create container
toOneContainer hfs hfs'
    | not $ null $ S.intersection
        (foldl1 S.union (map inputs fs))
        (foldl1 S.union (map outputs fs')) = S.fromList $ hfs ++ hfs'
    | not $ null $ S.intersection
        (foldl1 S.union (map inputs fs'))
        (foldl1 S.union (map outputs fs)) = S.fromList $ hfs ++ hfs'
    | otherwise                           = S.fromList hfs
        where
            fs = getFS hfs
            fs' = getFS hfs'


createContainers hfs
    | (length filtered) == 1 = containered
    | otherwise              = map S.toList listOfSets
    where
        listOfSets = S.toList $ S.fromList [toOneContainer hfs1 hfs2 | hfs1 <- containered , hfs2 <- containered, hfs1 /= hfs2]
        filtered = catMaybes $ filterAddSub hfs
        containered = map (\x -> [x]) filtered
        -- TODO: ADD HERE DIVIDING SUM https://nitta.io/nitta-corp/nitta/-/issues/75

refactorContainers containers = L.nub $ concatMap refactorContainer containers


-- |Create Map String (HistoryTree (F v x)), where Key is input label and Value is FU that contain this input label
containerMapCreate hfs = M.unions $
    map
    (\hf ->
       foldl
       (\dataMap k ->
          M.insertWith (++) k [hf] dataMap
       ) M.empty (S.toList $ inputs $ getF hf)
    ) $ hfs

-- |Takes container and refactor it, if it can be
refactorContainer [hf] = [hf]
refactorContainer hfs = concatMap refactored hfs
    where
        containerMap = containerMapCreate hfs

        refactored hf = concatMap
            (\o ->
                case M.findWithDefault [] o containerMap of
                    []         -> []
                    matchedFUs -> map (refactorFunction hf) matchedFUs
            ) (S.toList $ outputs $ getF hf)

deleteFromPull v (Pull (O s))
    | deleted == S.empty = Nothing
    | otherwise          = Just $ Pull $ O $ deleted
        where
            deleted = S.delete v s

deleteFromPull _ (Push _ _) = error "delete only Pull"

refactorFunction hf' hf
    | Just (Acc lst') <- castF $ getF hf'
    , Just (Acc lst ) <- castF $ getF hf
    , let
        multipleOutBool = (1 <) $ length $ outputs $ getF hf'
        isOutInpIntersect = any
            (\case
                Push _ (I v) -> elem v $ outputs $ getF hf'
                _            -> False
            ) lst
        makeRefactor = not multipleOutBool && isOutInpIntersect

    in
        makeRefactor = let
                refactorAcc _ _ (Pull o) = [Pull o]
                refactorAcc _lst' _hf' f@(Push s i@(I v))
                    | elem v $ outputs $ getF _hf' = mapMaybe
                        ( \f' ->
                            case (f, f') of
                                (Push Minus _, Push Plus x) -> Just $ Push Minus x
                                (Push Minus _, Push Minus x) -> Just $ Push Plus x
                                (Push Plus _, push@(Push _ _) ) -> Just push
                                (_, pull@(Pull _)) -> deleteFromPull v pull
                                (_, _) -> error "Pull can not be here"
                        ) _lst'
                    | s == Minus = [Push Minus i]
                    | s == Plus = [Push Plus i]
                refactorAcc _ _ (Push _ (I _)) = undefined
                newFS = packF
                    ( Acc $ concatMap (refactorAcc lst' hf') lst
                    ) `asTypeOf` (getF hf)
            in
                RefactoredFunc newFS [hf', hf]
    | otherwise = hf

data HistoryTree f = JustFunc f | RefactoredFunc f [HistoryTree f] deriving (Show, Eq, Ord)

toHistoryTree fs = map JustFunc fs

fromHistoryTree []                             = []
fromHistoryTree (JustFunc f : lstTail)         = f : fromHistoryTree lstTail
fromHistoryTree (RefactoredFunc f _ : lstTail) = f : fromHistoryTree lstTail

getF (JustFunc f)         = f
getF (RefactoredFunc f _) = f

getFS hfs = map getF hfs

refactorHfs hfs
    | startFS == newFS = newFS L.\\ funcsForDelete
    | otherwise        = refactorHfs newFS
    where
        startFS = hfs
        newFS = refactorContainers $ createContainers startFS

        containerMapRefactored = containerMapCreate newFS

        funcsForDelete = concatMap (findFuncForDelete containerMapRefactored) newFS


        findFuncForDelete outToFuncsMap hf = concatMap
            (\o ->
                case M.findWithDefault [] o outToFuncsMap of
                    []  -> []
                    lst -> hf : lst
            ) (S.toList $ outputs $ getF hf)

refactorSum dfg = refactorResult
    where
        refactorResult = [AlgSub $ fromHistoryTree refactored]
        refactored = refactorHfs $ toHistoryTree $ dataFlowGraphToFs dfg
