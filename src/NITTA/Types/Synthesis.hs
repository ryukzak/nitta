{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures -fno-warn-orphans #-}

{-|
Module      : NITTA.Types.Synthesis
Description : Types for describe synthesis process
Copyright   : (c) Aleksandr Penskoi, 2018
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Types.Synthesis
    ( -- *Synthesis graph
      Node(..)
    , Edge(..)
    , NId(..)
    , mkNodeIO
    , getNodeIO
    , getEdgesIO
      -- *Characteristics & synthesis decision type
    , SynthesisDT, synthesis
    , ChConf(..)
    , Characteristics(..)
      -- *Utils
    , option2decision
    ) where

import           Control.Arrow          (second)
import           Control.Concurrent.STM
import           Control.Monad          (forM, unless)
import           Data.Default
import           Data.List              (find)
import           Data.List.Split
import qualified Data.Map               as M
import           Data.Maybe
import           Data.Proxy
import           Data.Semigroup         (Semigroup, (<>))
import           Data.Set               (Set, fromList, intersection, member)
import qualified Data.Set               as S
import           Data.Typeable          (Typeable)
import           GHC.Generics
import           NITTA.BusNetwork
import           NITTA.DataFlow         (ModelState (..), isSchedulingComplete)
import           NITTA.Types
import           NITTA.Utils
import           NITTA.Utils.Lens
import           Numeric.Interval       (Interval, (...))


data Node title v x t
    = Node
        { nId         :: NId
        , nModel      :: ModelState title v x t
        , nIsComplete :: Bool
        , nOrigin     :: Maybe (Edge title v x t)
        , nEdges      :: TVar (Maybe [Edge title v x t])
        }
    deriving ( Generic )


data Edge title v x t
    = Edge
        { eNode            :: Node title v x t
        , eCharacteristic  :: Float
        , eCharacteristics :: Characteristics
        , eOption          :: Option (SynthesisDT title v x t)
        , eDecision        :: Decision (SynthesisDT title v x t)
        }
    deriving ( Generic )



-- |Create initial synthesis.
mkNodeIO nId model = atomically $ do
    nEdges <- newTVar Nothing
    return $ mkNode' nId model Nothing nEdges

mkNode' nId nModel nOrigin nEdges = Node
    { nId, nModel
    , nIsComplete=isSchedulingComplete nModel
    , nOrigin
    , nEdges
    }



getEdgesIO node@Node{ nEdges } = atomically $
    readTVar nEdges >>= \case
        Just edges -> return edges
        Nothing -> do
            edges <- mkEdges node
            writeTVar nEdges $ Just edges
            return edges



-- |Get specific by @nId@ node from a synthesis tree.
getNodeIO node (NId []) = return node
getNodeIO node nId@(NId (i:is)) = do
    edges <- getEdgesIO node
    unless (i < length edges) $ error $ "getNode - wrong nId: " ++ show nId
    getNodeIO (eNode $ edges !! i) (NId is)



-- |Synthesis identical.
newtype NId = NId [Int]
nIdSep = ':'

instance Show NId where
    show (NId []) = [nIdSep]
    show (NId is) = show' is
        where
            show' []     = ""
            show' (x:xs) = nIdSep : show x ++ show' xs

instance Read NId where
    readsPrec _ [x] | x == nIdSep    = [(NId [], "")]
    readsPrec d (x:xs)
        | x == nIdSep
        , let is = map (readsPrec d) $ splitOn [nIdSep] xs
        , all (not . null) is
        = [(NId $ map fst $ concat is, "")]
    readsPrec _ _ = []

instance Semigroup NId where
    (NId a) <> (NId b) = NId (a <> b)

instance Monoid NId where
    mempty = NId []
    mappend = (<>)



---------------------------------------------------------------------
-- *Compiler Decision Type


data SynthesisDT title v x t
synthesis = Proxy :: Proxy SynthesisDT


instance DecisionType (SynthesisDT title v x t) where
    data Option (SynthesisDT title v x t)
        = BindingOption (F v x) title
        | DataFlowOption (Source title (TimeConstrain t)) (Target title v (TimeConstrain t))
        deriving ( Generic, Show )

    data Decision (SynthesisDT title v x t)
        = BindingDecision (F v x) title
        | DataFlowDecision (Source title (Interval t)) (Target title v (Interval t))
        deriving ( Generic, Show )

isBinding = \case BindingOption{} -> True; _ -> False
isDataFlow = \case DataFlowOption{} -> True; _ -> False

specializeDataFlowOption (DataFlowOption s t) = DataFlowO s t
specializeDataFlowOption _ = error "Can't specialize non DataFlow option!"

generalizeDataFlowOption (DataFlowO s t) = DataFlowOption s t
generalizeBindingOption (BindingO s t) = BindingOption s t



instance ( Var v, Typeable x, Time t
         ) => DecisionProblem (SynthesisDT String v x t)
                  SynthesisDT (ModelState String v x t)
        where
    options _ f@Frame{ processor }
        =  map generalizeBindingOption (options binding f)
        ++ map generalizeDataFlowOption (options dataFlowDT processor)

    decision _ fr (BindingDecision f title) = decision binding fr $ BindingD f title
    decision _ fr@Frame{ processor } (DataFlowDecision src trg) = fr{ processor=decision dataFlowDT processor $ DataFlowD src trg }

option2decision (BindingOption fb title) = BindingDecision fb title
option2decision (DataFlowOption src trg)
    = let
        pushTimeConstrains = map snd $ catMaybes $ M.elems trg
        pullStart    = maximum $ (snd src^.avail.infimum) : map (\o -> o^.avail.infimum) pushTimeConstrains
        pullDuration = maximum $ map (\o -> o^.dur.infimum) $ snd src : pushTimeConstrains
        pullEnd = pullStart + pullDuration - 1
        pushStart = pullStart
        mkEvent (from_, tc) = Just (from_, pushStart ... (pushStart + tc^.dur.infimum - 1))
        pushs = map (second $ maybe Nothing mkEvent) $ M.assocs trg
    in DataFlowDecision ( fst src, pullStart ... pullEnd ) $ M.fromList pushs



-----------------------------------------------------------
-- *Characteristics


data Characteristics
    = -- |Binding Data Flow Characteristics
      BindCh
        { -- |Устанавливается для таких функциональных блоков, привязка которых может быть заблокирована
          -- другими. Пример - занятие Loop-ом адреса, используемого FramInput.
          critical         :: Bool
          -- |Колличество альтернативных привязок для функционального блока.
        , alternative      :: Float
          -- |Привязка данного функционального блока может быть активировано только спустя указанное
          -- колличество тактов.
        , restless         :: Float
          -- |Данная операция может быть привязана прямо сейчас и это приведёт к разрешению указанного
          -- количества пересылок.
        , allowDataFlow    :: Float
          -- |Если была выполнена привязка функции из серидины алгоритма, то это может
          -- привести к deadlock-у. В такой ситуации может быть активирована пересылка
          -- в вычислительный блок, в то время как часть из входных данных не доступна,
          -- так как требуемая функция ещё не привязана, а после привязки не сможет быть
          -- вычисленна, так как ресурс уже занят.
        , possibleDeadlock :: Bool
        }
    | -- |Data Flow Characteristics
      DFCh
        { waitTime       :: Float
        , restrictedTime :: Bool
        }
    deriving ( Show, Generic )



-- |Synthesis process setup, which determines next synthesis step selection.
newtype ChConf
    = ChConf
        { -- |Порог колличества вариантов, после которого пересылка данных станет приоритетнее, чем
          -- привязка функциональных блоков.
          threshhold :: Int
        }
    deriving ( Generic, Show, Eq, Ord )

instance Default ChConf where
    def = ChConf
        { threshhold=2
        }



mkEdges Node{ nId, nModel } = do
    let conf = def
        opts = options synthesis nModel
        bindedVars = unionsMap variables $ concat $ M.elems $ bnBinded $ processor nModel
        possibleDeadlockBinds = fromList
            [ f
            | (BindingOption f _) <- opts
            , Lock{ locked } <- locks f
            , locked `member` bindedVars
            ]
        cntx = ChCntx
            { nModel, possibleDeadlockBinds
            , bindingAlternative=foldl
                ( \st (BindingOption fb title) -> M.alter (collect title) fb st )
                M.empty
                $ filter isBinding opts
            , numberOfBindOptions=length $ filter isBinding opts
            , numberOfDFOptions=length $ filter isDataFlow opts
            }

    forM (zip [0..] opts) $ \(i, eOption) ->
        newTVar Nothing >>= \nEdges ->
        let
            eDecision = option2decision eOption
            eCharacteristics = measure conf cntx eOption
            eCharacteristic = integral conf cntx eCharacteristics

            eNode = mkNode' (nId <> NId [i]) (decision synthesis nModel eDecision) (Just origin) nEdges
            origin = Edge{ eOption, eDecision, eCharacteristics, eCharacteristic, eNode }
        in return origin



data ChCntx m title v x
    = ChCntx
        { nModel                :: m
        , possibleDeadlockBinds :: Set (F v x)
        , bindingAlternative    :: M.Map (F v x) [title]
        , numberOfBindOptions   :: Int
        , numberOfDFOptions     :: Int
        }



measure
        ChConf{}
        ChCntx{ possibleDeadlockBinds, bindingAlternative, nModel }
        (BindingOption f title)
    = BindCh
        { critical=isCritical f
        , alternative=fromIntegral $ length (bindingAlternative M.! f)
        , allowDataFlow=fromIntegral $ length $ unionsMap variables $ filter isTarget $ optionsAfterBind f title nModel
        , restless=fromMaybe 0 $ do
            (_var, tcFrom) <- find (\(v, _) -> v `elem` variables f) $ waitingTimeOfVariables nModel
            return $ fromIntegral tcFrom
        , possibleDeadlock=f `member` possibleDeadlockBinds
        }
measure ChConf{} ChCntx{} opt@DataFlowOption{}
    = DFCh
        { waitTime=fromIntegral (specializeDataFlowOption opt^.at.avail.infimum)
        , restrictedTime=fromEnum (specializeDataFlowOption opt^.at.dur.supremum) /= maxBound
        }



integral ChConf{} ChCntx{} BindCh{ possibleDeadlock=True }         = 2000000
integral ChConf{ threshhold } ChCntx{ numberOfDFOptions } DFCh{ waitTime }
    | numberOfDFOptions >= threshhold                              = 10000 + 200 - waitTime
integral ChConf{} ChCntx{} BindCh{ critical=True }                 = 2000
integral ChConf{} ChCntx{} BindCh{ alternative=1 }                 = 500
integral ChConf{} ChCntx{} BindCh{ allowDataFlow, restless }       = 200 + allowDataFlow * 10 - restless * 2
integral ChConf{} ChCntx{} DFCh{ restrictedTime } | restrictedTime = 200 + 100
integral ChConf{} ChCntx{} DFCh{ waitTime }                        = 200 - waitTime



-- *Internal

collect x (Just xs) = Just $ x : xs
collect x Nothing   = Just [ x ]


-- | Время ожидания переменных.
waitingTimeOfVariables net =
    [ (variable, tc^.avail.infimum)
    | DataFlowO{ dfoSource=(_, tc@TimeConstrain{}), dfoTargets } <- options dataFlowDT net
    , (variable, Nothing) <- M.assocs dfoTargets
    ]


-- | Оценить, сколько новых вариантов развития вычислительного процесса даёт привязка
-- функциоанльного блока.
optionsAfterBind f title Frame{ processor=BusNetwork{ bnPus } }
    = case tryBind f (bnPus M.! title) of
        Right pu' -> filter (\(EndpointO act _) -> act `optionOf` f) $ options endpointDT pu'
        _         -> []
    where
        act `optionOf` f' = not $ S.null (variables act `intersection` variables f')
