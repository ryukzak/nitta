{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : NITTA.Model.ProcessorUnits.Types
Description : Set of types for process unit description
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.TypesText (
    -- *Processor unit
    UnitTag,
    ProcessorUnit (..),
    bind,
    allowToProcess,

    -- *Process description
    Process (..),
    ProcessStepID,
    Step (..),
    StepInfo (..),
    Relation (..),
    descent,
    whatsHappen,
    extractInstructionAt,

    -- *Control
    Controllable (..),
    SignalTag (..),
    UnambiguouslyDecode (..),
    Connected (..),
    ByTime (..),
    SignalValue (..),
    (+++),

    -- *IO
    IOConnected (..),
    InputPortTag (..),
    OutputPortTag (..),
    InoutPortTag (..),
) where

import Data.Aeson (ToJSON)
import Data.Default
import Data.Either
import Data.Ix
import Data.Kind
import qualified Data.List as L
import Data.Maybe
import Data.String.Interpolate (i)
import qualified Data.String.Utils as S
import qualified Data.Text as T
import Data.Typeable
import GHC.Generics (Generic)
import NITTA.Intermediate.Types
import NITTA.Model.Problems.Endpoint
import NITTA.Model.Types
import NITTA.Utils.CodeFormatText
import Numeric.Interval.NonEmpty
import qualified Numeric.Interval.NonEmpty as I

-- |Typeclass alias for processor unit tag or "name."
type UnitTag tag = (Typeable tag, Ord tag, Show tag)

{- |Process unit - part of NITTA process with can execute a function from
intermediate representation:

1. get function for execution ('tryBind');

2. store computational process description ('process');

3. other features implemented by different type classes (see above and in
   "NITTA.Model.Problems").
-}
class (VarValTime v x t) => ProcessorUnit u v x t | u -> v x t where
    -- If the processor unit can execute a function, then it will return the PU
    -- model with already bound function (only registeration, actual scheduling
    -- will be happening later). If not, it will return @Left@ value with a
    -- specific reason (e.g., not support or all internal resources is over).
    tryBind :: F v x -> u -> Either String u

    -- Get a computational process description. If the processor unit embedded
    -- another PUs (like "NITTA.Model.Networks.Bus"), the description should
    -- contain process steps for all PUs.
    --
    -- 'ProcessStepID' may change from one call to another.
    process :: u -> Process t (StepInfo v x t)

bind f pu = case tryBind f pu of
    Right pu' -> pu'
    Left err -> error $ "can't bind function: " <> err

allowToProcess f pu = isRight $ tryBind f pu

---------------------------------------------------------------------

{- |Computational process description. It was designed in ISO 15926 style, with
separated data and relations storage.
-}
data Process t i = Process
    { -- |All process steps desctiption.
      steps :: [Step t i]
    , -- |List of relationships between process steps (see 'Relation').
      relations :: [Relation]
    , -- |Next free tick.
      nextTick :: t
    , -- |Next process step ID
      nextUid :: ProcessStepID
    }
    deriving (Generic)

instance (Time t, Show i) => Show (Process t i) where
    show p =
        T.unpack $
            codeBlock
                [i|
        Process
            steps     =
                #{ inline $ listShow $ steps p }
            relations =
                #{ inline $ listShow $ relations p }
            nextTick  = #{ show ( nextTick p ) }
            nextUid   = #{ show ( nextUid p ) }
        |]
        where
            listShow list = T.unlines $ map (\(ind, value) -> [i|#{ind}) #{value}|]) $ zip [0 :: Integer ..] list

instance (ToJSON t, ToJSON i) => ToJSON (Process t i)

instance (Default t) => Default (Process t i) where
    def = Process{steps = [], relations = [], nextTick = def, nextUid = def}

instance (Ord t) => WithFunctions (Process t (StepInfo v x t)) (F v x) where
    functions Process{steps} = mapMaybe get $ L.sortOn (I.inf . pInterval) steps
        where
            get Step{pDesc} | FStep f <- descent pDesc = Just f
            get _ = Nothing

-- |Unique ID of a process step. Uniquity presented only inside PU.
type ProcessStepID = Int

-- |Process step representation
data Step t i = Step
    { -- |uniq (inside single the process unit) step ID
      pID :: ProcessStepID
    , -- |step time
      pInterval :: Interval t
    , -- |step description
      pDesc :: i
    }
    deriving (Show, Generic)

instance (ToJSON t, ToJSON i) => ToJSON (Step t i)

instance (Ord v) => Patch (Step t (StepInfo v x t)) (Changeset v) where
    patch diff step@Step{pDesc} = step{pDesc = patch diff pDesc}

-- |Informative process step description at a specific process level.
data StepInfo v x t where
    -- |CAD level step
    CADStep :: String -> StepInfo v x t
    -- |intermidiate level step (funcution execution)
    FStep :: F v x -> StepInfo v x t
    -- |endpoint level step (source or target)
    EndpointRoleStep :: EndpointRole v -> StepInfo v x t
    -- |process unit instruction (depends on process unit type)
    InstructionStep ::
        (Show (Instruction pu), Typeable (Instruction pu)) =>
        Instruction pu ->
        StepInfo v x t
    -- |wrapper for nested process unit step (used for networks)
    NestedStep :: (UnitTag tag) => {nTitle :: tag, nStep :: Step t (StepInfo v x t)} -> StepInfo v x t

descent (NestedStep _ step) = descent $ pDesc step
descent desc = desc

instance (Show (Step t (StepInfo v x t)), Show v) => Show (StepInfo v x t) where
    show (CADStep msg) = "CAD: " <> msg
    show (FStep F{fun}) = "Intermediate: " <> S.replace "\"" "" (show fun)
    show (EndpointRoleStep eff) = "Endpoint: " <> S.replace "\"" "" (show eff)
    show (InstructionStep instr) = "Instruction: " <> S.replace "\"" "" (show instr)
    show NestedStep{nTitle, nStep = Step{pDesc}} = S.replace "\"" "" ("@" <> show nTitle <> " " <> show pDesc)

instance (Ord v) => Patch (StepInfo v x t) (Changeset v) where
    patch diff (FStep f) = FStep $ patch diff f
    patch diff (EndpointRoleStep ep) = EndpointRoleStep $ patch diff ep
    patch diff (NestedStep tag nStep) = NestedStep tag $ patch diff nStep
    patch _ var = var

-- |Relations between process steps.
data Relation
    = -- |Vertical relationships (up and down). For example, the intermediate
      -- step (function execution) can be translated to a sequence of endpoint
      -- steps (receiving and sending variable), and process unit instructions.
      Vertical ProcessStepID ProcessStepID
    deriving (Show, Eq, Generic)

instance ToJSON Relation

whatsHappen t Process{steps} = filter (atSameTime t . pInterval) steps
    where
        atSameTime a ti = a `member` ti

extractInstructionAt pu t = mapMaybe (inst pu) $ whatsHappen t $ process pu
    where
        inst :: (Typeable (Instruction pu)) => pu -> Step t (StepInfo v x t) -> Maybe (Instruction pu)
        inst _ Step{pDesc = InstructionStep instr} = cast instr
        inst _ _ = Nothing

---------------------------------------------------------------------

{- |Type class for controllable units. Defines two level of a unit behaviour
representation (see ahead).
-}
class Controllable pu where
    -- Instruction describe unit behaviour on each mUnit cycle. If instruction
    -- not defined for some cycles - it should be interpreted as NOP. In future,
    -- Instruction should be extracted, because
    data Instruction pu :: Type

    -- |Microcode desctibe controll signals on each mUnit cycle (without exclusion).
    data Microcode pu :: Type

    -- |Map microcode to unit signal ports.
    mapMicrocodeToPorts :: Microcode pu -> Ports pu -> [(SignalTag, SignalValue)]

    -- |Get list of signals from Ports pu
    portsToSignals :: Ports pu -> [SignalTag]

    -- |Get Ports from list of signals
    signalsToPorts :: [SignalTag] -> pu -> Ports pu

-- |Getting microcode value at a specific time.
class ByTime pu t | pu -> t where
    microcodeAt :: pu -> t -> Microcode pu

instance
    ( Show (Instruction pu)
    , Default (Microcode pu)
    , ProcessorUnit pu v x t
    , UnambiguouslyDecode pu
    , Time t
    , Typeable pu
    ) =>
    ByTime pu t
    where
    microcodeAt pu t = case extractInstructionAt pu t of
        [] -> def
        [instr] -> decodeInstruction instr
        instrs -> error $ "instruction collision at " ++ show t ++ " tick: " ++ show instrs ++ show (process pu)

newtype SignalTag = SignalTag {signalTag :: Int} deriving (Show, Eq, Ord, Ix)

-- |Type class of processor units with control ports.
class Connected pu where
    -- |A processor unit control ports (signals, flags).
    data Ports pu :: Type

{- |Decoding microcode from a simple instruction (microcode don't change over
time).

TODO: Generalize that class for all process units, including networks.
-}
class UnambiguouslyDecode pu where
    decodeInstruction :: Instruction pu -> Microcode pu

-- |Control line value.
data SignalValue
    = -- |undefined by design (`x`)
      Undef
    | -- |boolean (`0` or `1`)
      Bool Bool
    | -- |broken value (`x`) by data colision
      BrokenSignal
    deriving (Eq)

instance Default SignalValue where
    def = Undef

instance Show SignalValue where
    show Undef = "x"
    show (Bool True) = "1"
    show (Bool False) = "0"
    show BrokenSignal = "B"

Undef +++ v = v
v +++ Undef = v
_ +++ _ = BrokenSignal

------------------------------------------------------------

-- |Type class of processor units with IO ports.
class IOConnected pu where
    data IOPorts pu :: Type

    -- |External input ports, which go outside of NITTA mUnit.
    inputPorts :: IOPorts pu -> [InputPortTag]
    inputPorts _ = []

    -- |External output ports, which go outside of NITTA mUnit.
    outputPorts :: IOPorts pu -> [OutputPortTag]
    outputPorts _ = []

    -- |External output ports, which go outside of NITTA mUnit.
    inoutPorts :: IOPorts pu -> [InoutPortTag]
    inoutPorts _ = []

newtype InputPortTag = InputPortTag {inputPortTag :: String} deriving (Show, Eq, Ord)
newtype OutputPortTag = OutputPortTag {outputPortTag :: String} deriving (Show, Eq, Ord)
newtype InoutPortTag = InoutPortTag {inoutPortTag :: String} deriving (Show, Eq, Ord)