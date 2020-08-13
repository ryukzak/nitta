{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies #-}

{-|
Module      : NITTA.Model.Networks.Types
Description : Types for processor unit network description.
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.Networks.Types
    ( PU(..)
    , PUClasses
    , IOSynchronization(..)
    ) where

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import           Data.Typeable
import           NITTA.Intermediate.Types
import           NITTA.Model.Problems
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Model.Types
import           NITTA.Project.Implementation
import           NITTA.Project.Parts.TestBench


type PUClasses pu v x t =
    ( ByTime pu t
    , Connected pu
    , IOConnected pu
    , EndpointProblem pu v t
    , RefactorProblem pu v x
    , ProcessorUnit pu v x t
    , Show (Instruction pu)
    , Simulatable pu v x
    , Typeable pu
    , UnambiguouslyDecode pu
    , TargetSystemComponent pu
    , Controllable pu
    , IOTestBench pu v x
    , Locks pu v
    )


-- |Existential container for a processor unit .
data PU v x t where
    PU :: ( PUClasses pu v x t ) =>
            { diff :: Changeset v -- FIXME: move to end of record
            , unit :: pu
            , ports :: Ports pu
            , ioPorts :: IOPorts pu
            , systemEnv :: TargetEnvironment
            } -> PU v x t


instance ( Ord v ) => EndpointProblem (PU v x t) v t where
    endpointOptions PU{ diff, unit }
        = map (patch diff) $ endpointOptions unit

    endpointDecision PU{ diff, unit, ports, ioPorts, systemEnv } d
        = PU
            { diff
            , unit=endpointDecision unit $ patch (reverseDiff diff) d
            , ports, ioPorts
            , systemEnv
            }

instance RefactorProblem (PU v x t) v x where
    refactorOptions PU{ unit } = refactorOptions unit
    refactorDecision PU{ diff, unit, ports, ioPorts, systemEnv } d
        = PU{ diff, unit=refactorDecision unit d, ports, ioPorts, systemEnv }

instance ( VarValTime v x t ) => ProcessorUnit (PU v x t) v x t where
    tryBind fb PU{ diff, unit, ports, ioPorts, systemEnv }
        = case tryBind fb unit of
            Right unit' -> Right PU{ diff, unit=unit', ports, ioPorts, systemEnv }
            Left err    -> Left err
    process PU{ diff, unit } = let
            p = process unit
        in p{ steps=map (patch diff) $ steps p }

instance ( Ord v ) => Patch (PU v x t) (Changeset v) where
    patch diff' PU{ diff, unit, ports, ioPorts, systemEnv }
        = PU
            { diff=Changeset
                { changeI=changeI diff' `M.union` changeI diff
                , changeO=changeO diff' `M.union` changeO diff
                }
            , unit, ports, ioPorts, systemEnv
            }

instance ( Ord v ) => Patch (PU v x t) (I v, I v) where
    patch (I v, I v') pu@PU{ diff=diff@Changeset{ changeI } } = pu{ diff=diff{ changeI=M.insert v v' changeI }}

instance ( Ord v ) => Patch (PU v x t) (O v, O v) where
    patch (O vs, O vs') pu@PU{ diff=diff@Changeset{ changeO } }
        = pu{ diff=diff
                { changeO=foldl (\s (v, v') -> M.insert v (S.singleton v') s)
                    changeO
                    $ [ (a, b) | b <- S.elems vs', a <- S.elems vs ]
                }
            }

instance ( Var v ) => Locks (PU v x t) v where
    locks PU{ unit, diff=Changeset{ changeI, changeO } }
        | not $ M.null changeI = error $ "Locks (PU v x t) with non empty changeI: " ++ show changeI
        | otherwise = let
                (locked', locks') = L.partition (\Lock{ locked } -> locked `M.member` changeO) $ locks unit
                (lockBy', locks'') = L.partition (\Lock{ lockBy } -> lockBy `M.member` changeO) locks'
            in concat
                [ locks''
                , L.nub $ concatMap
                    ( \Lock{ locked, lockBy } -> [ Lock{ locked, lockBy=v } | v <- S.elems (changeO M.! lockBy) ] )
                    lockBy'
                , L.nub $ concatMap
                    ( \Lock{ locked, lockBy } -> [ Lock{ locked=v, lockBy } | v <- S.elems (changeO M.! locked) ] )
                    locked'
                ]

instance Simulatable (PU v x t) v x where
    simulateOn cntx PU{ unit } fb = simulateOn cntx unit fb

instance TargetSystemComponent (PU v x t) where
    moduleName name PU{ unit } = moduleName name unit
    hardware name PU{ unit } = hardware name unit
    software name PU{ unit } = software name unit
    hardwareInstance name pu = hardwareInstance name pu

instance IOTestBench (PU v x t) v x where
    testEnvironmentInitFlag tag PU{ unit } = testEnvironmentInitFlag tag unit

    testEnvironment tag PU{ unit, systemEnv, ports, ioPorts } _systemEnv _ _ cntxs
        = testEnvironment tag unit systemEnv ports ioPorts cntxs


data IOSynchronization
    = Sync  -- ^IO cycle synchronously to process cycle
    | ASync -- ^if IO cycle lag behiend - ignore them
    | OnBoard -- ^defined by onboard signal (sync - false, async - true)
    deriving ( Show, Read, Typeable )
