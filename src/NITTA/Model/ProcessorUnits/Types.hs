{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Module      : NITTA.Model.ProcessorUnits.Types
Description : Set of types for process unit description
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Types (
    -- *Processor unit
    UnitTag (..),
    ProcessorUnit (..),
    bind,
    allowToProcess,
    NextTick (..),
    ParallelismType (..),

    -- *Process description
    Process (..),
    ProcessStepID,
    Step (..),
    StepInfo (..),
    Relation (..),
    isVertical,
    isHorizontal,
    descent,
    whatsHappen,
    extractInstructionAt,
    withShift,
    isRefactorStep,
    isAllocationStep,

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
import Data.Kind
import Data.List qualified as L
import Data.List.Utils (replace)
import Data.Maybe
import Data.Set qualified as S
import Data.String
import Data.String.Interpolate
import Data.String.ToString
import Data.Text qualified as T
import Data.Typeable
import GHC.Generics (Generic)
import NITTA.Intermediate.Types
import NITTA.Model.Problems.Endpoint
import NITTA.Model.Time
import Numeric.Interval.NonEmpty
import Numeric.Interval.NonEmpty qualified as I
import Prettyprinter
--import Data.Typeable

-- |Typeclass alias for processor unit tag or "name."
-- type UnitTag tag = (Typeable tag, Ord tag, ToString tag, IsString tag, Show tag)

-- |Class for processor unit tag or "name"
class (Typeable tag, Ord tag, ToString tag, IsString tag, Semigroup tag, Show tag) => UnitTag tag where
    -- |Whether the value can be used as a template or not
    isTemplate :: tag -> Bool

    -- |Create tag from the template and index
    fromTemplate :: tag -> String -> tag

instance UnitTag T.Text where
    isTemplate tag = T.isInfixOf (T.pack "{x}") tag
    fromTemplate tag index = T.replace (T.pack "{x}") (T.pack index) tag

instance UnitTag String where
    isTemplate tag = "{x}" `L.isInfixOf` tag
    fromTemplate tag index = replace "{x}" index tag

-- |Processor unit parallelism type
data ParallelismType
    = -- |All operations can be performed in parallel mode
      Full
    | -- |All operations can be performed in pipeline mode
      Pipeline
    | -- |Other processor units
      None
    deriving (Show, Generic, Eq)

instance ToJSON ParallelismType

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

    unitType :: u -> TypeRep
    
    -- |Indicates what type of parallelism is supported by 'ProcessorUnit'
    parallelismType :: u -> ParallelismType
    parallelismType _ = None

    -- |Provide the processor unit size. At the moment it's just the number of subprocessors
    puSize :: u -> Float
    puSize _ = 1

bind f pu = case tryBind f pu of
    Right pu' -> pu'
    Left err -> error $ "can't bind function: " <> err <> " " <> show (unitType pu)

allowToProcess f pu = isRight $ tryBind f pu

class NextTick u t | u -> t where
    nextTick :: u -> t

instance (ProcessorUnit u v x t) => NextTick u t where
    nextTick = nextTick . process

---------------------------------------------------------------------

{- |Computational process description. It was designed in ISO 15926 style, with
separated data and relations storage.
-}
data Process t i = Process
    { steps :: [Step t i]
    -- ^All process steps desctiption.
    , relations :: [Relation]
    -- ^List of relationships between process steps (see 'Relation').
    , nextTick_ :: t
    -- ^Next tick for instruction. Note: instruction /= endpoint.
    , nextUid :: ProcessStepID
    -- ^Next process step ID
    }
    deriving (Generic)

instance (Time t, Show i) => Pretty (Process t i) where
    pretty p =
        [__i|
            Process:
                steps: #{ showList' $ reverse $ steps p }
                relations: #{ showList' $ relations p }
                nextTick: #{ nextTick p }
                nextUid: #{ nextUid p }
        |]
        where
            showList' [] = pretty ""
            showList' xs = line <> indent 8 (vsep lst)
                where
                    lst =
                        map (pretty . (\(ix, value) -> [i|#{ ix }) #{ value }|] :: T.Text)) $
                            zip [0 :: Int ..] xs

instance (ToJSON t, ToJSON i) => ToJSON (Process t i)

instance (Default t) => Default (Process t i) where
    def = Process{steps = [], relations = [], nextTick_ = def, nextUid = def}

instance {-# OVERLAPS #-} NextTick (Process t si) t where
    nextTick = nextTick_

instance (Ord t) => WithFunctions (Process t (StepInfo v x t)) (F v x) where
    functions Process{steps} = mapMaybe get $ L.sortOn (I.inf . pInterval) steps
        where
            get Step{pDesc} | IntermediateStep f <- descent pDesc = Just f
            get _ = Nothing

-- |Unique ID of a process step. Uniquity presented only inside PU.
type ProcessStepID = Int

-- |Process step representation
data Step t i = Step
    { pID :: ProcessStepID
    -- ^uniq (inside single the process unit) step ID
    , pInterval :: Interval t
    -- ^step time
    , pDesc :: i
    -- ^step description
    }
    deriving (Show, Generic)

instance (ToJSON t, ToJSON i) => ToJSON (Step t i)

instance (Ord v) => Patch (Step t (StepInfo v x t)) (Changeset v) where
    patch diff step@Step{pDesc} = step{pDesc = patch diff pDesc}

-- |Informative process step description at a specific process level.
data StepInfo v x t where
    -- |CAD level step
    CADStep :: String -> StepInfo v x t
    -- |Apply refactoring
    RefactorStep :: (Typeable ref, Show ref, Eq ref) => ref -> StepInfo v x t
    -- |intermidiate level step (function execution)
    IntermediateStep :: F v x -> StepInfo v x t
    -- |endpoint level step (source or target)
    EndpointRoleStep :: EndpointRole v -> StepInfo v x t
    -- |process unit instruction (depends on process unit type)
    InstructionStep ::
        (Show (Instruction pu), Typeable (Instruction pu)) =>
        Instruction pu ->
        StepInfo v x t
    -- |wrapper for nested process unit step (used for networks)
    NestedStep :: (UnitTag tag) => {nTitle :: tag, nStep :: Step t (StepInfo v x t)} -> StepInfo v x t
    -- |Process unit allocation step
    AllocationStep :: (Typeable a, Show a, Eq a) => a -> StepInfo v x t

descent (NestedStep _ step) = descent $ pDesc step
descent desc = desc

isRefactorStep RefactorStep{} = True
isRefactorStep _ = False

isAllocationStep AllocationStep{} = True
isAllocationStep _ = False

instance (Var v, Show (Step t (StepInfo v x t))) => Show (StepInfo v x t) where
    show (CADStep msg) = "CAD: " <> msg
    show (AllocationStep alloc) = "Allocation: " <> show alloc
    show (RefactorStep ref) = "Refactor: " <> show ref
    show (IntermediateStep F{fun}) = "Intermediate: " <> show fun
    show (EndpointRoleStep eff) = "Endpoint: " <> show eff
    show (InstructionStep instr) = "Instruction: " <> show instr
    show NestedStep{nTitle, nStep = Step{pDesc}} = "@" <> toString nTitle <> " " <> show pDesc

instance (Ord v) => Patch (StepInfo v x t) (Changeset v) where
    patch diff (IntermediateStep f) = IntermediateStep $ patch diff f
    patch diff (EndpointRoleStep ep) = EndpointRoleStep $ patch diff ep
    patch diff (NestedStep tag nStep) = NestedStep tag $ patch diff nStep
    patch _ instr = instr

-- |Relations between process steps.
data Relation
    = -- |Vertical relationships (up and down). For example, the intermediate
      -- step (function execution) can be translated to a sequence of endpoint
      -- steps (receiving and sending variable), and process unit instructions.
      Vertical {vUp, vDown :: ProcessStepID}
    | -- |Horizontal relationships (on one level). For example, we bind the
      -- function and apply the refactoring. The binding step should be
      -- connected to refactoring steps, including new binding steps.
      Horizontal {hPrev, hNext :: ProcessStepID}
    deriving (Show, Generic, Ord, Eq)

isVertical Vertical{} = True
isVertical _ = False

isHorizontal Horizontal{} = True
isHorizontal _ = False

instance ToJSON Relation

whatsHappen t Process{steps} = filter (atSameTime t . pInterval) steps
    where
        atSameTime a ti = a `member` ti

extractInstructionAt pu t = mapMaybe (inst pu) $ whatsHappen t $ process pu
    where
        inst :: (Typeable (Instruction pu)) => pu -> Step t (StepInfo v x t) -> Maybe (Instruction pu)
        inst _ Step{pDesc = InstructionStep instr} = cast instr
        inst _ _ = Nothing

{- |Shift @nextTick@ value if it is not zero on a specific offset. Use case: The
processor unit has buffered output, so we should provide @oe@ signal for one
tick before data actually send to the bus. That raises the following cases:

1. First usage. We can receive value immediately on nextTick

    @
    tick | Endpoint     | Instruction |
     0   | Target "c"   | WR          | <- nextTick
    @

2. Not first usage. We need to wait for one tick from the last instruction due to the offset between instruction and data transfers.

    @
    tick | Endpoint     | Instruction |
      8  |              | OE          |
      9  | Source ["b"] |             | <- nextTick
     10  | Target "c"   | WR          |
    @
-}
0 `withShift` _offset = 0
tick `withShift` offset = tick + offset

---------------------------------------------------------------------

{- |Type class for controllable units. Defines two level of a unit behaviour
representation (see ahead).
-}
class Controllable pu where
    -- Instruction describe unit behaviour on each mUnit cycle. If instruction
    -- not defined for some cycles - it should be interpreted as NOP.
    data Instruction pu :: Type

    -- |Microcode desctibe controll signals on each mUnit cycle (without exclusion).
    data Microcode pu :: Type

    -- |Zip port signal tags and value.
    zipSignalTagsAndValues :: Ports pu -> Microcode pu -> [(SignalTag, SignalValue)]

    -- |Get list of used control signal tags.
    usedPortTags :: Ports pu -> [SignalTag]

    -- |Take signal tags from inifinite list of tags.
    takePortTags :: [SignalTag] -> pu -> Ports pu

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
        is -> error [i|instruction collision at #{ t } tick: #{ is } #{ pretty $ process pu }|]

newtype SignalTag = SignalTag {signalTag :: T.Text} deriving (Eq, Ord)

instance Show SignalTag where
    show = toString . signalTag

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
    inputPorts :: IOPorts pu -> S.Set InputPortTag
    inputPorts _ = S.empty

    -- |External output ports, which go outside of NITTA mUnit.
    outputPorts :: IOPorts pu -> S.Set OutputPortTag
    outputPorts _ = S.empty

    -- |External output ports, which go outside of NITTA mUnit.
    inoutPorts :: IOPorts pu -> S.Set InoutPortTag
    inoutPorts _ = S.empty

newtype InputPortTag = InputPortTag {inputPortTag :: T.Text} deriving (Eq, Ord)
instance Show InputPortTag where show = toString . inputPortTag

newtype OutputPortTag = OutputPortTag {outputPortTag :: T.Text} deriving (Eq, Ord)
instance Show OutputPortTag where show = toString . outputPortTag

newtype InoutPortTag = InoutPortTag {inoutPortTag :: T.Text} deriving (Eq, Ord)
instance Show InoutPortTag where show = toString . inoutPortTag
