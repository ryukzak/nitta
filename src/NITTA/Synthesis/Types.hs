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
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints #-}
{-# OPTIONS -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Synthesis.Types
Description : Synthesis graph representation
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental

Synthesis can be represented as a graph (tree), where each 'Node' describes the
target system 'ModelState' and each 'Edge' synthesis decision.

A synthesis graph is very large and calculating and storing it in memory is very
bad idea. Also, working with synthesis graph usually making from the specific
node, not from the root. As a result, synthesis graph design as a explicit lazy
mutable structure implemented by 'TVar'.

From this point of view, the synthesis process is a finding of the best tree
leaf (lowest process duration for finished synthesis), and the best synthesis
method - a method which directly walks over the tree to the best leaf without
wrong steps.
-}
module NITTA.Synthesis.Types
    ( -- *Synthesis graph
      G, NId(..), Node(..), Edge(..)
    , mkRootNodeIO, getNodeIO, getEdgesIO, getPositiveEdgesIO
      -- *Synthesis decision type & Parameters
    , ObjectiveFunctionConf(..)
    , Parameters(..)
      -- *Utils
    , isSynthesisFinish
    ) where

import           Control.Concurrent.STM
import           Control.Monad                    (forM, unless)
import           Data.Default
import           Data.List                        (find)
import           Data.List.Split
import qualified Data.Map                         as M
import           Data.Maybe
import           Data.Semigroup                   (Semigroup, (<>))
import           Data.Set                         (Set, fromList, intersection,
                                                   member, (\\))
import qualified Data.Set                         as S
import           GHC.Generics
import           NITTA.Intermediate.Types
import           NITTA.Model.Networks.Bus
import           NITTA.Model.Problems.Dataflow
import           NITTA.Model.Problems.Endpoint
import           NITTA.Model.Problems.Refactor
import           NITTA.Model.Problems.Whole
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Model.TargetSystem         (ModelState (..))
import           NITTA.Model.Types
import           NITTA.Utils
import           NITTA.Utils.Lens
import           Numeric.Interval                 (Interval)
import           Numeric.Interval                 (inf, sup)


-- |Type alias for Graph parts, where `e` - graph element (Node or Edge) should be 'Node' or 'Edge';
type G e tag v x t
    = e
        (ModelState (BusNetwork tag v x t) v x)
        (SynthesisStatement tag v x (TimeConstrain t))
        (SynthesisStatement tag v x (Interval t))


-- |Synthesis graph ID. ID is a relative path, encoded as a sequence of an
-- option index.
newtype NId = NId [Int]

-- |NId separator for @Show NId@ and @Read NId@.
nIdSep = '-'

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


data Node m o d
    = Node
        { nId         :: NId
          -- |model of target processor
        , nModel      :: m
          -- |true when synthesis options are over and all algorithm are
          -- scheduled
        , nIsComplete :: Bool
          -- |if 'Node' is root - 'Nothing'; if 'Node' is not root - 'Just'
          -- input 'Edge'.
        , nOrigin     :: Maybe (Edge m o d)
          -- |lazy mutable field with different synthesis options and sub nodes
        , nEdges      :: TVar (Maybe [Edge m o d])
        }
    deriving ( Generic )


data Edge m o d
    = Edge
        { eNode                   :: Node m o d
        , eOption                 :: o
        , eDecision               :: d
          -- |parameters of the 'Edge'
        , eParameters             :: Parameters
          -- |objective function value for the 'Edge', which representing
          -- parameters as a number
        , eObjectiveFunctionValue :: Float
        }
    deriving ( Generic )


-- |Make synthesis graph (root node).
mkRootNodeIO model = atomically $ do
    nEdges <- newTVar Nothing
    return $ mkNode mempty model Nothing nEdges

mkNode nId nModel nOrigin nEdges = Node
    { nId, nModel
    , nIsComplete=isSynthesisFinish nModel
    , nOrigin
    , nEdges
    }


-- |Get all available edges for the node. Edges calculated only for the first
-- call.
getEdgesIO :: ( UnitTag tag, VarValTime v x t, Semigroup v
    ) => G Node tag v x t -> IO [ G Edge tag v x t ]
getEdgesIO node@Node{ nEdges } = atomically $
    readTVar nEdges >>= \case
        Just edges -> return edges
        Nothing -> do
            edges <- mkEdges node
            writeTVar nEdges $ Just edges
            return edges


-- |For synthesis method is more usefull, because throw away all useless edges
-- (objective function value less than zero).
getPositiveEdgesIO n = filter ((> 0) . eObjectiveFunctionValue) <$> getEdgesIO n


-- |Get specific by @nId@ node from a synthesis tree.
getNodeIO :: ( UnitTag tag, VarValTime v x t, Semigroup v
    ) => G Node tag v x t -> NId -> IO ( G Node tag v x t )
getNodeIO node (NId []) = return node
getNodeIO node nId@(NId (i:is)) = do
    edges <- getEdgesIO node
    unless (i < length edges) $ error $ "getNode - wrong nId: " ++ show nId
    getNodeIO (eNode $ edges !! i) (NId is)


mkEdges :: ( UnitTag tag, VarValTime v x t, Semigroup v
    ) => G Node tag v x t -> STM [ G Edge tag v x t ]
mkEdges n@Node{ nId, nModel, nOrigin } = do
    let conf = def
        cntx = prepareParametersCntx n

    forM (zip [0..] $ synthesisOptions nModel) $ \(i, opt) ->
        newTVar Nothing >>= \nEdges ->
        let
            eOption = opt
            eDecision = option2decision eOption
            eParameters = estimateParameters conf cntx eOption
            eObjectiveFunctionValue = objectiveFunction conf cntx eParameters

            eNode = mkNode (nId <> NId [i]) (synthesisDecision nModel eDecision) (Just origin `asTypeOf` nOrigin) nEdges
            origin = Edge{ eOption, eDecision, eParameters, eObjectiveFunctionValue, eNode }
        in return origin


prepareParametersCntx Node{ nModel } = let
        opts = synthesisOptions nModel
        bindableFunctions = [ f | (Binding f _) <- opts ]

        mkWaves n pool lockedVars
            | pool == S.empty = []
            | pool `intersection` lockedVars == S.empty = zip (S.elems lockedVars) (repeat n)
        mkWaves n pool lockedVars
            = let currentWave = pool `intersection` lockedVars
            in zip (S.elems currentWave) (repeat n)
                ++ mkWaves (n+1) (pool \\ currentWave) (currentWave `S.union` lockedVars)
    in ParametersCntx
        { nModel
        , possibleDeadlockBinds = fromList
            [ f
            | (Binding f tag) <- opts
            , Lock{ lockBy } <- locks f
            , lockBy `member` unionsMap variables (bindedFunctions tag $ mUnit nModel)
            ]
        , transferableVars = fromList
            [ v
            | (Dataflow _ targets) <- opts
            , (v, Just _) <- M.assocs targets
            ]
        , alreadyBindedVariables = variables $ mUnit nModel
        , waves = M.fromList $ mkWaves 0
            (unionsMap variables bindableFunctions)
            (fromList (map locked $ concatMap locks bindableFunctions))
        , bindingAlternative=foldl
            ( \st (Binding f tag) -> M.alter (collect tag) f st )
            M.empty
            $ filter isBinding opts
        , numberOfBindOptions=length $ filter isBinding opts
        , numberOfDFOptions=length $ filter isDataFlow opts
        , outputOfBindableFunctions=unionsMap outputs bindableFunctions
        }
    where
        collect x (Just xs) = Just $ x : xs
        collect x Nothing   = Just [ x ]


-----------------------------------------------------------
-- *Parameters

data Parameters
    = BindEdgeParameter
        { -- |Устанавливается для таких функциональных блоков, привязка которых может быть заблокирована
          -- другими. Пример - занятие Loop-ом адреса, используемого LoopOut.
          pCritical                :: Bool
          -- |Number of binding alternatives
        , pAlternative             :: Float
          -- |Привязка данного функционального блока может быть активировано только спустя указанное
          -- колличество тактов.
        , pRestless                :: Float
        , pOutputNumber            :: Float
          -- |Данная операция может быть привязана прямо сейчас и это приведёт к разрешению указанного
          -- количества пересылок.
        , pAllowDataFlow           :: Float
          -- |Если была выполнена привязка функции из серидины алгоритма, то это может
          -- привести к deadlock-у. В такой ситуации может быть активирована пересылка
          -- в вычислительный блок, в то время как часть из входных данных не доступна,
          -- так как требуемая функция ещё не привязана, а после привязки не сможет быть
          -- вычисленна, так как ресурс уже занят.
        , pPossibleDeadlock        :: Bool
        , pNumberOfBindedFunctions :: Float
        -- |number of binded input variables / number of all input variables
        , pPercentOfBindedInputs   :: Float
        , pWave                    :: Float
        }
    | DataFlowEdgeParameter
        { pWaitTime              :: Float
        , pRestrictedTime        :: Bool
        -- |Number of variables, which is not transferable for affected functions.
        , pNotTransferableInputs :: [Float]
        }
    | RefactorEdgeParameter
        { pRefactor :: Refactor () ()
        }
    deriving ( Show, Generic )


data ParametersCntx m tag v x
    = ParametersCntx
        { nModel                    :: m
        , possibleDeadlockBinds     :: Set (F v x)
        , transferableVars          :: Set v
        , bindingAlternative        :: M.Map (F v x) [tag]
        , numberOfBindOptions       :: Int
        , numberOfDFOptions         :: Int
        , alreadyBindedVariables    :: Set v
        , outputOfBindableFunctions :: Set v
        , waves                     :: M.Map v Int
        }


-- |Synthesis process setup, which determines next synthesis step selection.
newtype ObjectiveFunctionConf
    = ObjectiveFunctionConf
        { -- |Порог колличества вариантов, после которого пересылка данных станет приоритетнее, чем
          -- привязка функциональных блоков.
          threshold :: Int
        }
    deriving ( Generic, Show, Eq, Ord )

instance Default ObjectiveFunctionConf where
    def = ObjectiveFunctionConf
        { threshold=20
        }


estimateParameters
        ObjectiveFunctionConf{}
        ParametersCntx{ possibleDeadlockBinds, bindingAlternative, nModel, alreadyBindedVariables, waves }
        (Binding f tag)
    = BindEdgeParameter
        { pCritical=isInternalLockPossible f
        , pAlternative=fromIntegral $ length (bindingAlternative M.! f)
        , pAllowDataFlow=fromIntegral $ length $ unionsMap variables $ filter isTarget $ optionsAfterBind f tag nModel
        , pRestless=fromMaybe 0 $ do
            (_var, tcFrom) <- find (\(v, _) -> v `elem` variables f) $ waitingTimeOfVariables nModel
            return $ fromIntegral tcFrom
        , pOutputNumber=fromIntegral $ length $ S.elems $ outputs f
        , pPossibleDeadlock=f `member` possibleDeadlockBinds
        , pNumberOfBindedFunctions=fromIntegral $ length $ bindedFunctions tag $ mUnit nModel
        , pPercentOfBindedInputs = let
                is = inputs f
                n = fromIntegral $ length $ intersection is alreadyBindedVariables
                nAll = fromIntegral $ length is
            in if nAll == 0 then 1 else n / nAll
        , pWave=let
                    allInputs = S.elems $ inputs f
                    ns = map (\v -> fromMaybe 0 (waves M.!? v)) allInputs
                in fromIntegral $ maximum (0 : ns)
        }

estimateParameters
        ObjectiveFunctionConf{}
        ParametersCntx{ transferableVars, nModel }
        (Dataflow (_, TimeConstrain{ tcAvailable, tcDuration }) target )
    = DataFlowEdgeParameter
        { pWaitTime=fromIntegral (inf tcAvailable)
        , pRestrictedTime=fromEnum (sup tcDuration) /= maxBound
        , pNotTransferableInputs
            = let
                fs = functions nModel
                vs = fromList [ v | (v, Just _) <- M.assocs target ]
                affectedFunctions = filter (\f -> not $ null (inputs f `intersection` vs)) fs
                notTransferableVars = map (\f -> inputs f \\ transferableVars) affectedFunctions
            in map (fromIntegral . length) notTransferableVars
        }
estimateParameters ObjectiveFunctionConf{} ParametersCntx{} (Refactor InsertOutRegister{})
    = RefactorEdgeParameter $ InsertOutRegister def def
estimateParameters ObjectiveFunctionConf{} ParametersCntx{} (Refactor BreakLoop{})
    = RefactorEdgeParameter $ BreakLoop def def def


-- |Function, which map 'Parameters' to 'Float'.
objectiveFunction
        ObjectiveFunctionConf{ threshold }
        ParametersCntx{ numberOfDFOptions }
        params
    = case params of
        BindEdgeParameter{ pPossibleDeadlock=True } -> -1
        BindEdgeParameter{ pCritical, pAlternative, pAllowDataFlow, pRestless, pNumberOfBindedFunctions, pWave, pPercentOfBindedInputs, pOutputNumber }
            -> 1000
                + pCritical <?> 1000
                + (pAlternative == 1) <?> 500
                + pAllowDataFlow * 10
                + pPercentOfBindedInputs * 50
                - pWave * 50
                - pNumberOfBindedFunctions * 10
                - pRestless * 4
                + pOutputNumber * 2
        DataFlowEdgeParameter{ pWaitTime, pNotTransferableInputs, pRestrictedTime }
            ->  100
                + (numberOfDFOptions >= threshold) <?> 1000
                + pRestrictedTime <?> 200
                - sum pNotTransferableInputs * 5
                - pWaitTime
        (RefactorEdgeParameter InsertOutRegister{})
            -> 2000
        (RefactorEdgeParameter BreakLoop{})
            -> 2000
        _ -> -1

True <?> v = v
False <?> _ = 0


waitingTimeOfVariables net =
    [ (variable, tc^.avail.infimum)
    | DataFlowO{ dfoSource=(_, tc@TimeConstrain{}), dfoTargets } <- dataflowOptions net
    , (variable, Nothing) <- M.assocs dfoTargets
    ]


optionsAfterBind f tag ModelState{ mUnit=BusNetwork{ bnPus } }
    = case tryBind f (bnPus M.! tag) of
        Right pu' -> filter (\(EndpointO act _) -> act `optionOf` f) $ endpointOptions pu'
        _         -> []
    where
        act `optionOf` f' = not $ S.null (variables act `intersection` variables f')


-- |Synthesis process is finish when all variable from data flow are
-- transferred.

-- FIXME: Should be moved.
isSynthesisFinish :: ( ProcessorUnit u v x t ) => ModelState u v x -> Bool
isSynthesisFinish ModelState{ mUnit, mDataFlowGraph } = let
        inWork = transferred mUnit
        inAlg = variables mDataFlowGraph
    in inWork == inAlg
