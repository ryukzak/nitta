{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : NITTA.Model.Networks.Types
Description : Types for processor unit network description.
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Networks.Types (
    PU (..),
    unitType,
    PUClasses,
    IOSynchronization (..),
    PUPrototype (..),
    puInputPorts,
    puOutputPorts,
    puInOutPorts,
) where

import Data.Aeson
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Set qualified as S
import Data.Typeable
import GHC.Generics (Generic)
import NITTA.Intermediate.Types
import NITTA.Model.Problems
import NITTA.Model.ProcessorUnits.Types
import NITTA.Model.Time
import NITTA.Project.TestBench
import NITTA.Project.Types

type PUClasses pu v x t =
    ( ByTime pu t
    , Connected pu
    , IOConnected pu
    , EndpointProblem pu v t
    , BreakLoopProblem pu v x
    , ConstantFoldingProblem pu v x
    , OptimizeAccumProblem pu v x
    , OptimizeLutProblem pu v x
    , ResolveDeadlockProblem pu v x
    , ProcessorUnit pu v x t
    , Show (Instruction pu)
    , Typeable pu
    , UnambiguouslyDecode pu
    , TargetSystemComponent pu
    , Controllable pu
    , IOTestBench pu v x
    , Locks pu v
    , Typeable pu
    )

-- | Existential container for a processor unit .
data PU v x t where
    PU ::
        PUClasses pu v x t =>
        { unit :: pu
        , diff :: Changeset v
        , uEnv :: UnitEnv pu
        } ->
        PU v x t

unitType :: PU v x t -> TypeRep
unitType PU{unit} = typeOf unit

instance Ord v => EndpointProblem (PU v x t) v t where
    endpointOptions PU{diff, unit} =
        map (patch diff) $ endpointOptions unit

    endpointDecision PU{unit, diff, uEnv} d =
        PU
            { unit = endpointDecision unit $ patch (reverseDiff diff) d
            , diff
            , uEnv
            }

instance BreakLoopProblem (PU v x t) v x where
    breakLoopOptions PU{unit} = breakLoopOptions unit
    breakLoopDecision PU{diff, unit, uEnv} d =
        PU{unit = breakLoopDecision unit d, diff, uEnv}

instance OptimizeAccumProblem (PU v x t) v x where
    optimizeAccumOptions PU{unit} = optimizeAccumOptions unit
    optimizeAccumDecision PU{diff, unit, uEnv} d =
        PU{diff, unit = optimizeAccumDecision unit d, uEnv}

instance OptimizeLutProblem (PU v x t) v x where
    optimizeLutOptions PU{unit} = optimizeLutOptions unit
    optimizeLutDecision PU{diff, unit, uEnv} d =
        PU{diff, unit = optimizeLutDecision unit d, uEnv}

instance ResolveDeadlockProblem (PU v x t) v x where
    resolveDeadlockOptions PU{unit} = resolveDeadlockOptions unit
    resolveDeadlockDecision PU{diff, unit, uEnv} d =
        PU{unit = resolveDeadlockDecision unit d, diff, uEnv}

instance VarValTime v x t => ProcessorUnit (PU v x t) v x t where
    tryBind fb PU{diff, unit, uEnv} =
        case tryBind fb unit of
            Right unit' -> Right PU{unit = unit', diff, uEnv}
            Left err -> Left err
    process PU{unit, diff} =
        let p = process unit
         in p{steps = map (patch diff) $ steps p}
    parallelismType PU{unit} = parallelismType unit

instance Ord v => Patch (PU v x t) (Changeset v) where
    patch diff' PU{unit, diff, uEnv} =
        PU
            { unit
            , diff =
                Changeset
                    { changeI = changeI diff' `M.union` changeI diff
                    , changeO = changeO diff' `M.union` changeO diff
                    }
            , uEnv
            }

instance Ord v => Patch (PU v x t) (I v, I v) where
    patch (I v, I v') pu@PU{diff = diff@Changeset{changeI}} = pu{diff = diff{changeI = M.insert v v' changeI}}

instance Ord v => Patch (PU v x t) (O v, O v) where
    patch (O vs, O vs') pu@PU{diff = diff@Changeset{changeO}} =
        pu
            { diff =
                diff
                    { changeO =
                        foldl
                            (\s (v, v') -> M.insert v (S.singleton v') s)
                            changeO
                            $ [(a, b) | b <- S.elems vs', a <- S.elems vs]
                    }
            }

instance Var v => Locks (PU v x t) v where
    locks PU{unit, diff = diff@Changeset{changeI, changeO}}
        | not $ M.null changeI = error $ "Locks (PU v x t) with non empty changeI: " <> show diff
        | otherwise =
            let (locked', locks') = L.partition (\Lock{locked} -> locked `M.member` changeO) $ locks unit
                (lockBy', locks'') = L.partition (\Lock{lockBy} -> lockBy `M.member` changeO) locks'
             in concat
                    [ locks''
                    , L.nub $
                        concatMap
                            (\Lock{locked, lockBy} -> [Lock{locked, lockBy = v} | v <- S.elems (changeO M.! lockBy)])
                            lockBy'
                    , L.nub $
                        concatMap
                            (\Lock{locked, lockBy} -> [Lock{locked = v, lockBy} | v <- S.elems (changeO M.! locked)])
                            locked'
                    ]

instance TargetSystemComponent (PU v x t) where
    moduleName name PU{unit} = moduleName name unit
    hardware name PU{unit} = hardware name unit
    software name PU{unit} = software name unit
    hardwareInstance name pu = hardwareInstance name pu

instance IOTestBench (PU v x t) v x where
    testEnvironmentInitFlag tag PU{unit} = testEnvironmentInitFlag tag unit

    testEnvironment tag PU{unit, uEnv} _env cntxs = testEnvironment tag unit uEnv cntxs

data IOSynchronization
    = -- | IO cycle synchronously to process cycle
      Sync
    | -- | if IO cycle lag behiend - ignore them
      ASync
    | -- | defined by onboard signal (sync - false, async - true)
      OnBoard
    deriving (Show, Read, Typeable, Generic)

instance ToJSON IOSynchronization
instance FromJSON IOSynchronization

puInputPorts PU{uEnv} = envInputPorts uEnv
puOutputPorts PU{uEnv} = envOutputPorts uEnv
puInOutPorts PU{uEnv} = envInOutPorts uEnv

-- | PU and some additional information required for allocation on BusNetwork
data PUPrototype tag v x t where
    PUPrototype ::
        (UnitTag tag, PUClasses pu v x t) =>
        { pTag :: tag
        {- ^ Prototype tag. You can specify tag as a template by adding {x}.
        This will allow to allocate PU more than once by replacing {x} with index.
        When PU is allocated processUnitTag will look like bnName_pTag.
        -}
        , pProto :: pu
        -- ^ PU prototype
        , pIOPorts :: IOPorts pu
        -- ^ IO ports that will be used by PU
        } ->
        PUPrototype tag v x t
