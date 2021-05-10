{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : NITTA.Intermediate.Types
Description : Types for an algorithm intermediate representation
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Intermediate.Types (
    -- *Function interface
    I (..),
    O (..),
    X (..),

    -- *Function description
    F (..),
    FView (..),
    packF,
    castF,
    Function (..),
    Lock (..),
    Locks (..),
    inputsLockOutputs,
    WithFunctions (..),
    Label (..),

    -- *Functional simulation
    FunctionSimulation (..),
    CycleCntx (..),
    Cntx (..),
    log2md,
    log2json,
    log2csv,
    cntxReceivedBySlice,
    getCntx,
    updateCntx,

    -- *Patch
    Patch (..),
    Changeset (..),
    reverseDiff,
    module NITTA.Intermediate.Value,
    module NITTA.Intermediate.Variable,
) where

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.Csv as Csv
import Data.Default
import qualified Data.HashMap.Strict as HM
import Data.List (sort, sortOn, transpose)
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S hiding (split)
import Data.String.ToString
import qualified Data.String.Utils as S
import Data.Tuple
import Data.Typeable
import GHC.Generics
import NITTA.Intermediate.Value
import NITTA.Intermediate.Variable
import NITTA.UIBackend.ViewHelperCls
import NITTA.Utils.Base
import Text.PrettyPrint.Boxes hiding ((<>))

-- |Input variable.
newtype I v = I v
    deriving (Eq, Ord)

instance (ToString v) => Show (I v) where show (I v) = toString v

instance (Eq v) => Patch (I v) (v, v) where
    patch (v, v') i@(I v0)
        | v0 == v = I v'
        | otherwise = i

instance Variables (I v) v where
    variables (I v) = S.singleton v

-- |Output variable set.
newtype O v = O (S.Set v)
    deriving (Eq, Ord)

instance (Ord v) => Patch (O v) (v, v) where
    patch (v, v') (O vs) = O $ S.fromList $ map (\e -> if e == v then v' else e) $ S.elems vs

instance (ToString v) => Show (O v) where
    show (O vs)
        | S.null vs = "_"
        | otherwise = S.join " = " $ vsToStringList vs

instance Variables (O v) v where
    variables (O vs) = vs

-- |Value of variable (constant or initial value).
newtype X x = X x
    deriving (Show, Eq)

-----------------------------------------------------------

{- |Casuality of variable processing sequence in term of locks.

For example:
> c := a + b
> [ Lock{ locked=c, lockBy=a }, Lock{ locked=c, lockBy=b } ]
-}
class (Var v) => Locks x v | x -> v where
    locks :: x -> [Lock v]

-- |Variable casuality.
data Lock v = Lock
    { locked :: v
    , lockBy :: v
    }
    deriving (Eq, Ord, Generic)

instance (ToString v) => Show (Lock v) where
    show Lock{locked, lockBy} =
        "Lock{locked=" <> toString locked <> ", lockBy=" <> toString lockBy <> "}"

instance (ToJSON v) => ToJSON (Lock v)

-- |All input variables locks all output variables.
inputsLockOutputs f =
    [ Lock{locked = y, lockBy = x}
    | x <- S.elems $ inputs f
    , y <- S.elems $ outputs f
    ]

-----------------------------------------------------------

-- |Type class for application algorithm functions.
class Function f v | f -> v where
    -- |Get all input variables.
    inputs :: f -> S.Set v
    inputs _ = S.empty

    -- |Get all output variables.
    outputs :: f -> S.Set v
    outputs _ = S.empty

    -- |Sometimes, one function can cause internal process unit lock for another function.

    -- TODO: remove or move, because its depends from PU type
    isInternalLockPossible :: f -> Bool
    isInternalLockPossible _ = False

-- |Type class for making fine label for Functions.
class Label a where
    label :: a -> String

instance Label String where
    label s = s

-- |Type class of something, which is related to functions.
class WithFunctions a f | a -> f where
    -- |Get a list of associated functions.
    functions :: a -> [f]

-- |Box forall functions.
data F v x where
    F ::
        ( Function f v
        , Patch f (v, v)
        , Locks f v
        , Show f
        , Label f
        , FunctionSimulation f v x
        , Typeable f
        , Eq f
        ) =>
        { fun :: f
        , funHistory :: [F v x]
        } ->
        F v x

packF f = F{fun = f, funHistory = []}

instance Eq (F v x) where
    F{fun = a} == F{fun = b}
        | typeOf a == typeOf b = a == fromJust (cast b)
        | otherwise = False

instance Function (F v x) v where
    isInternalLockPossible F{fun} = isInternalLockPossible fun
    inputs F{fun} = inputs fun
    outputs F{fun} = outputs fun

instance FunctionSimulation (F v x) v x where
    simulate cntx F{fun} = simulate cntx fun

instance Label (F v x) where
    label F{fun} = S.replace "\"" "" $ label fun

instance (Var v) => Locks (F v x) v where
    locks F{fun} = locks fun

instance Ord (F v x) where
    F{fun = a} `compare` F{fun = b} = show a `compare` show b

instance Patch (F v x) (v, v) where
    patch diff fun0@F{fun, funHistory} =
        F
            { fun = patch diff fun
            , funHistory = fun0 : funHistory
            }

instance (Ord v) => Patch (F v x) (Changeset v) where
    patch Changeset{changeI, changeO} f0 =
        let changeI' =
                mapMaybe
                    ( \v -> case changeI M.!? v of
                        Just v' -> Just (v, v')
                        Nothing -> Nothing
                    )
                    $ S.elems $ inputs f0
            changeO' =
                concat $
                    mapMaybe
                        ( \v -> case changeO M.!? v of
                            Just vs -> Just [(v, v') | v' <- S.elems vs]
                            Nothing -> Nothing
                        )
                        $ S.elems $ outputs f0
         in foldl (\f diff -> patch diff f) f0 $ changeI' ++ changeO'

instance (Patch b v) => Patch [b] v where
    patch diff fs = map (patch diff) fs

instance Show (F v x) where
    show F{fun} = S.replace "\"" "" $ show fun

instance (Var v) => Variables (F v x) v where
    variables F{fun} = inputs fun `S.union` outputs fun

-- |Helper for extraction function from existential container 'F'.
castF :: (Typeable f, Typeable v, Typeable x) => F v x -> Maybe (f v x)
castF F{fun} = cast fun

-- |Helper for JSON serialization
data FView = FView
    { fvFun :: String
    , fvHistory :: [String]
    }
    deriving (Generic, Show)

instance Viewable (F v x) FView where
    view F{fun, funHistory} =
        FView
            { fvFun = S.replace "\"" "" $ show fun
            , fvHistory = map (S.replace "\"" "" . show) funHistory
            }

instance ToJSON FView

-----------------------------------------------------------

-- |The type class for function simulation.
class FunctionSimulation f v x | f -> v x where
    -- FIXME: CycleCntx - problem, because its prevent Receive simulation with
    -- data drop (how implement that?).

    -- |Receive a computational context and return changes (list of varible names and its new values).
    simulate :: CycleCntx v x -> f -> [(v, x)]

newtype CycleCntx v x = CycleCntx {cycleCntx :: HM.HashMap v x}
    deriving (Generic)

instance (ToString v, Show x) => Show (CycleCntx v x) where
    show CycleCntx{cycleCntx} =
        "{" <> S.join ", " (map (\(v, x) -> toString v <> ": " <> show x) $ HM.toList cycleCntx) <> "}"

instance Default (CycleCntx v x) where
    def = CycleCntx HM.empty

data Cntx v x = Cntx
    { -- |all variables on each process cycle
      cntxProcess :: [CycleCntx v x]
    , -- |sequences of all received values, one value per process cycle
      cntxReceived :: M.Map v [x]
    , cntxCycleNumber :: Int
    }

instance (Show x) => Show (Cntx String x) where
    show Cntx{cntxProcess} = log2md $ map (HM.map show . cycleCntx) cntxProcess

log2list cntxProcess =
    let header = sort $ HM.keys $ head cntxProcess
        body = map row cntxProcess
        row cntx = map snd $ zip header $ sortedValues cntx
     in map (uncurry (:)) $ zip header (transpose body)
    where
        sortedValues cntx = map snd $ sortOn fst $ HM.toList cntx

{- |
 >>> let records = map HM.fromList [[("x1"::String,"1.2"::String), ("x2","3.4")], [("x1","3.4"), ("x2","2.3")]]
 >>> putStr $ log2md records
 | Cycle  | x1   | x2   |
 |:-------|:-----|:-----|
 | 1      | 1.2  | 3.4  |
 | 2      | 3.4  | 2.3  |
-}
log2md records =
    let n = length records
        cntx2listCycle = ("Cycle" : map show [1 .. n]) : log2list records
        maxLength t = length $ foldr1 (\x y -> if length x >= length y then x else y) t
        cycleFormattedTable = map ((\x@(x1 : x2 : xs) -> x1 : ("|:" ++ replicate (maxLength x) '-') : x2 : xs) . map ("| " ++)) cntx2listCycle ++ [replicate (n + 2) "|"]
     in render
            ( hsep 0 left $
                map (vcat left . map text) cycleFormattedTable
            )

{- |
 >>> import qualified Data.ByteString.Lazy.Char8 as BS
 >>> let records = map HM.fromList [[("x1"::String,"1.2"::String), ("x2","3.4")], [("x1","3.4"), ("x2","2.3")]]
 >>> BS.putStr $ log2json records
 [
     {
         "x2": 3.4,
         "x1": 1.2
     },
     {
         "x2": 2.3,
         "x1": 3.4
     }
]
-}
log2json records =
    let listHashMap = transpose $ map (\(k : vs) -> map (\v -> (k, read v :: Double)) vs) $ log2list records
     in encodePretty $ map HM.fromList listHashMap

{- |
 >>> import qualified Data.ByteString.Lazy.Char8 as BS
 >>> let records = map HM.fromList [[("x1"::String,"1.2"::String), ("x2","3.4")], [("x1","3.4"), ("x2","2.3")]]
 >>> BS.putStr $ log2csv records
 x1,x2
 1.2,3.4
 3.4,2.3
-}
log2csv records = Csv.encode $ transpose $ log2list records

instance Default (Cntx v x) where
    def =
        Cntx
            { cntxProcess = def
            , cntxReceived = def
            , cntxCycleNumber = 5
            }

-- |Make sequence of received values '[ Map v x ]'
cntxReceivedBySlice :: (Ord v) => Cntx v x -> [M.Map v x]
cntxReceivedBySlice Cntx{cntxReceived} = cntxReceivedBySlice' $ M.assocs cntxReceived

cntxReceivedBySlice' received
    | not $ any (null . snd) received =
        let slice = M.fromList [(v, x) | (v, x : _) <- received]
            received' = [(v, xs) | (v, _ : xs) <- received]
         in slice : cntxReceivedBySlice' received'
    | otherwise = repeat M.empty

getCntx (CycleCntx cntx) v = case HM.lookup v cntx of
    Just x -> x
    Nothing -> error $ "variable not defined: " <> toString v

updateCntx cycleCntx [] = Right cycleCntx
updateCntx (CycleCntx cntx) ((v, x) : vxs)
    | HM.member v cntx = Left $ "variable value already defined: " <> toString v
    | otherwise = updateCntx (CycleCntx $ HM.insert v x cntx) vxs

-----------------------------------------------------------

-- |Patch class allows replacing one variable by another. Especially for algorithm refactor.
class Patch f diff where
    patch :: diff -> f -> f

{- |Change set for patch.

>>> Changeset (M.fromList [("a", "b"), ("c", "d")]) (M.fromList [("e", S.fromList ["f", "g"])]) :: Changeset String
Changeset{changeI=[(a, b), (c, d)], changeO=[(e, [f, g])]}
-}
data Changeset v = Changeset
    { -- |change set for input variables (one to one)
      changeI :: M.Map v v
    , -- |change set for output variables. Many to many relations:
      --
      -- > fromList [(a, {x}), (b, {x})] -- several output variables to one
      -- > fromList [(c, {y, z})] -- one output variable to many
      changeO :: M.Map v (S.Set v)
    }
    deriving (Eq)

instance (Var v) => Show (Changeset v) where
    show Changeset{changeI, changeO} =
        let changeI' = S.join ", " $ map (\(a, b) -> "(" <> toString a <> ", " <> toString b <> ")") $ M.assocs changeI
            changeO' = S.join ", " $ map (\(a, bs) -> "(" <> toString a <> ", [" <> S.join ", " (vsToStringList bs) <> "])") $ M.assocs changeO
         in "Changeset{changeI=[" <> changeI' <> "], changeO=[" <> changeO' <> "]}"

instance Default (Changeset v) where
    def = Changeset def def

-- |Reverse changeset for patch a process unit options / decision.
reverseDiff Changeset{changeI, changeO} =
    Changeset
        { changeI = M.fromList $ map swap $ M.assocs changeI
        , changeO =
            foldl
                ( \st (k, v) ->
                    let box' = case st M.!? k of
                            Just box -> box `S.union` S.singleton v
                            Nothing -> S.singleton v
                     in M.insert k box' st
                )
                def
                [ (b, a)
                | (a, bs) <- M.assocs changeO
                , b <- S.elems bs
                ]
        }
