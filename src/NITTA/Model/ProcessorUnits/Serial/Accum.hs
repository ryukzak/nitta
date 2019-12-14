{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}
{-|
Module      : NITTA.Model.ProcessorUnits.Serial.Accum
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Serial.Accum
  ( accum
  , Accum
  , Ports(..), IOPorts(..)
  ) where

import           Control.Monad                    (when)
import           Data.Bits                        (finiteBitSize)
import           Data.Default
import           Data.List                        (find, partition, (\\))
import           Data.Set                         (elems, fromList, member)
import qualified NITTA.Intermediate.Functions     as F
import           NITTA.Intermediate.Types
import           NITTA.Model.Problems.Endpoint
import           NITTA.Model.Problems.Refactor
import           NITTA.Model.ProcessorUnits.Time
import           NITTA.Model.Types
import           NITTA.Project.Implementation
import           NITTA.Project.Parts.TestBench
import           NITTA.Project.Snippets
import           NITTA.Utils
import           NITTA.Utils.ProcessDescription
import           Numeric.Interval                 (singleton, sup, (...))
import           Text.InterpolatedString.Perl6    (qc)

-- |Accumulator for each function
data FAccum v x =
    FAccum
        { model :: [[(Bool,v)]]
        , real  :: [[(Bool,v)]]
        , func  :: F v x
        } deriving (Eq, Show)

data Accum v x t = Accum
    { remain               :: [FAccum v x]
    , doneAt               :: Maybe t
    , currentWork          :: Maybe ( t, FAccum v x )
    , currentWorkEndpoints :: [ ProcessUid ]
    , process_             :: Process v x t
    , tick                 :: t
    , isInit               :: Bool
    }

instance (VarValTime v x t) => Show (Accum v x t) where
    show a = [qc|"
        Accum:
            remain               = {remain a}
            doneAt               = {doneAt a}
            currentWork          = {currentWork a}
            currentWorkEndpoints = {currentWorkEndpoints a}
            process_             = {process_ a}
            tick_                = {tick a}
            isInit               = {isInit a}|]


accum :: (VarValTime v x t) => Accum v x t
accum = Accum
    { remain=[]
    , doneAt=Nothing
    , currentWork=Nothing
    , currentWorkEndpoints=[]
    , process_=def
    , tick=def
    , isInit=True
    }


instance ( VarValTime v x t ) => Default (Accum v x t) where
    def = accum


setRemain f
    | Just (F.Acc (vs :: [F.Status v])) <- castF f = zip (F.pushStatusGroups vs) (F.pullStatusGroups vs)
    | otherwise                                    = error "Error! Function is not Acc"


tryBindFunc f = FAccum {model = functionModel, real = [], func = f}
    where
        functionModel = concatMap (\(i, o) -> [i, map (\x -> (False, x)) o]) (setRemain f)


endpointOptionsFunc FAccum {model=[]} = []

endpointOptionsFunc FAccum {model=(m:_), real=[]} = map snd m

endpointOptionsFunc FAccum {model=(m:ms), real=(r:_)}
    | null ( m \\ r) && null ms = []
    | null $ m \\ r             = map snd $ head ms
    | otherwise                 = map snd $ m \\ r


endpointDecisionFunc a@FAccum {model=[]} _ = a

endpointDecisionFunc a@FAccum {model=(m:_), real=[]} v = a {real=newRealCreate}
    where
        ([(neg, _)], _) = partition ((== v) . snd) m
        newRealCreate =[[(neg, v)]]

endpointDecisionFunc a@FAccum {model=model@(m:ms), real=(r:rs)} v
    | null $ m \\ r                               = endpointDecisionFunc a {model = ms} v
    | not ((==m) $ m \\ r) && length m > length r = a {model = newModel newRealInsert, real=newRealInsert}
    | otherwise                                   = a {model = newModel newRealAdd, real = newRealAdd}
        where
            ([val], _) = partition ((== v) . snd) m
            newRealInsert = (val : r) : rs
            newRealAdd = [val] : r : rs
            newModel []               = error "real is null"
            newModel (newReal:_)
                | null $ m \\ newReal = ms
                | otherwise           = model


instance ( VarValTime v x t, Num x) => ProcessorUnit (Accum v x t) v x t where
    tryBind f pu@Accum{remain}
        | Just (F.Add a b c)  <- castF f = Right pu{ remain = tryBindFunc ( F.acc [F.Push F.Plus a, F.Push F.Plus b, F.Pull c] )  : remain }
        | Just (F.Sub a b c)  <- castF f = Right pu{ remain = tryBindFunc ( F.acc [F.Push F.Plus a, F.Push F.Minus b, F.Pull c])  : remain }
        | Just F.Acc{}        <- castF f = Right pu{ remain = tryBindFunc f : remain}
        | otherwise = Left $ "The function is unsupported by Accum: " ++ show f

    process = process_

    setTime t pu@Accum{} = pu{ tick=t }


instance ( VarValTime v x t, Num x) => EndpointProblem (Accum v x t) v t where
    endpointOptions Accum{ currentWork = Just (_, a@FAccum {model}), tick }
        | even (length model)  = map (\v -> EndpointSt (Target v) $ TimeConstrain (tick ... maxBound) (singleton 1)) (endpointOptionsFunc a)
        | odd (length model) = [ EndpointSt (Source $ fromList (endpointOptionsFunc a) ) $ TimeConstrain (tick + 3 ... maxBound) (1 ... maxBound) ]

    endpointOptions p@Accum{ remain, currentWork = Nothing, tick } =
        concatMap (\a -> endpointOptions p {currentWork = Just (tick + 1, a)}) remain

    endpointOptions _ = error "Error in matching in endpointOptions function"

    endpointDecision pu@Accum{ currentWork=Just (t, a@FAccum {model}), currentWorkEndpoints, isInit } d@EndpointSt{ epRole=Target v, epAt }
        | not (null model) && even ( length model) = let
                fAccum@FAccum {model=newModel, real = (((neg, _):_):_)} = endpointDecisionFunc a v
                sel = if isInit then Init neg else Load neg
                (newEndpoints, process_') = runSchedule pu $ do
                    updateTick (sup epAt)
                    scheduleEndpoint d $ scheduleInstruction epAt sel
            in
                pu
                    { process_=process_'
                    , currentWork = Just(t, fAccum)
                    , currentWorkEndpoints=newEndpoints ++ currentWorkEndpoints
                    , doneAt=if null newModel
                        then Just $ sup epAt + 3
                        else Nothing
                    , tick=sup epAt
                    , isInit=null newModel
                    }

    endpointDecision pu@Accum{ currentWork=Just (t, a@FAccum {model, real, func}), currentWorkEndpoints, doneAt} d@EndpointSt{ epRole=Source v, epAt }
        | not (null real) && odd ( length model) = let
                fAccum@FAccum {model=newModel} = foldl endpointDecisionFunc a (elems v)
                (newEndpoints, process_') = runSchedule pu $ do
                    endpoints <- scheduleEndpoint d $ scheduleInstruction (epAt-1) Out
                    when (null newModel) $ do
                        high <- scheduleFunction (t ... sup epAt) func
                        let low = endpoints ++ currentWorkEndpoints
                        establishVerticalRelations high low

                    updateTick (sup epAt)
                    return endpoints
            in pu
                { process_=process_'
                , doneAt=if null newModel then Nothing else doneAt
                , currentWork=if null newModel then Nothing else Just(t, fAccum)
                , currentWorkEndpoints=if null newModel then [] else newEndpoints ++ currentWorkEndpoints
                , tick=sup epAt
                , isInit=null newModel
                }

    endpointDecision pu@Accum{remain, currentWork=Nothing, tick} d
        | let v = oneOf $ variables d
        , Just fAccum <- find (\FAccum {func} -> v `member` variables func) remain
        = endpointDecision pu {remain = remain \\ [fAccum], currentWork = Just (tick+1, fAccum), isInit = True } d


    endpointDecision pu  d = error $ "error in Endpoint Decision function" ++ show pu ++ show d

instance Connected (Accum v x t) where
    data Ports (Accum v x t)
        = AccumPorts{ resetAcc, load, neg, oe :: SignalTag } deriving ( Show )


instance IOConnected (Accum v x t) where
    data IOPorts (Accum v x t) = AccumIO


instance Controllable (Accum v x t) where
    data Instruction (Accum v x t) = Init Bool | Load Bool | Out deriving (Show)

    data Microcode (Accum v x t) =
        Microcode
            { oeSignal   :: Bool
            , resetAccSignal :: Bool
            , loadSignal :: Bool
            , negSignal  :: Maybe Bool
            } deriving ( Show, Eq, Ord )

    mapMicrocodeToPorts Microcode{..} AccumPorts{..} =
        [ (resetAcc, Bool resetAccSignal)
        , (load, Bool loadSignal)
        , (neg, maybe Undef Bool negSignal)
        , (oe, Bool oeSignal)
        ]

    portsToSignals AccumPorts{ resetAcc, load, neg, oe } = [resetAcc, load, neg, oe]

    signalsToPorts (resetAcc:load:neg:oe:_) _ = AccumPorts resetAcc load neg oe
    signalsToPorts _                    _ = error "pattern match error in signalsToPorts AccumPorts"


instance Default (Microcode (Accum v x t)) where
    def = Microcode
        { oeSignal=False
        , resetAccSignal=False
        , loadSignal=False
        , negSignal=Nothing
        }


instance UnambiguouslyDecode (Accum v x t) where
    decodeInstruction (Init neg) = def{ resetAccSignal=True, loadSignal=True, negSignal=Just neg }
    decodeInstruction (Load neg) = def{ resetAccSignal=False, loadSignal=True, negSignal=Just neg }
    decodeInstruction Out        = def{ oeSignal=True }


instance (VarValTime v x t, Num x) => Simulatable (Accum v x t) v x where
  simulateOn cntx _ f
    | Just f'@F.Add{} <- castF f = simulate cntx f'
    | Just f'@F.Sub{} <- castF f = simulate cntx f'
    | Just f'@F.Acc{} <- castF f = simulate cntx f'
    | otherwise = error $ "Can't simulate " ++ show f ++ " on Accum."


instance ( Var v ) => Locks (Accum v x t) v where
    locks Accum{ currentWork = Nothing }                  = []
    locks Accum{ currentWork = Just (_, fAccum), remain } = locks' fAccum remain
            where
                locks' FAccum{model=[]} _ = []
                locks' FAccum{real =[]} _ = []
                locks' FAccum{model=(m:ms), real =(r:_)} other =
                    [ Lock{ lockBy, locked }
                    | locked <- concatMap (map snd) ms ++ concatMap (concatMap (map snd) . model) other
                    , lockBy <- map snd (m \\ r)
                    ]


instance ( Val x, Default x ) => TargetSystemComponent (Accum v x t) where
    moduleName _ _ = "pu_accum"
    hardware tag pu = FromLibrary $ moduleName tag pu ++ ".v"
    software _ _ = Empty
    hardwareInstance tag _pu TargetEnvironment{ unitEnv=ProcessUnitEnv{..}, signalClk, signalRst } AccumPorts{..} AccumIO
        = codeBlock [qc|
            pu_accum #
                    ( .DATA_WIDTH( { finiteBitSize (def :: x) } )
                    , .ATTR_WIDTH( { show parameterAttrWidth } )
                    ) { tag }
                ( .clk( { signalClk } )
                , .rst( { signalRst } )
                , .signal_resetAcc( { signal resetAcc } )
                , .signal_load( { signal load } )
                , .signal_neg( { signal neg } )
                , .signal_oe( { signal oe } )
                , .data_in( { dataIn } )
                , .attr_in( { attrIn } )
                , .data_out( { dataOut } )
                , .attr_out( { attrOut } )
                );
            |]
    hardwareInstance _title _pu TargetEnvironment{ unitEnv=NetworkEnv{} } _ports _io
        = error "Should be defined in network."

instance IOTestBench (Accum v x t) v x

instance RefactorProblem (Accum v x t) v x
