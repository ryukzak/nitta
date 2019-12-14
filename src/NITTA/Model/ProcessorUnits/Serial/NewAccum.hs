{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE LambdaCase            #-}
-- # LANGUAGE DuplicateRecordFields #
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
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
import           Data.List                        (nub, init, find, partition, (\\))
import           Data.Set                         (elems, empty, fromList, toList, member, insert, intersection, Set)
import qualified NITTA.Intermediate.Functions     as F
import           NITTA.Intermediate.Types
import           NITTA.Model.Problems.Endpoint
import           NITTA.Model.Problems.Refactor
import           NITTA.Model.ProcessorUnits.Types
import           NITTA.Model.Types
import           NITTA.Project.Implementation
import           NITTA.Project.Parts.TestBench
import           NITTA.Project.Snippets
import           NITTA.Utils
import           NITTA.Utils.ProcessDescription
import           Numeric.Interval                 (singleton, sup, (...))
import           Text.InterpolatedString.Perl6    (qc)


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
{-
>>> :module +NITTA.Model.Types NITTA.Intermediate.Functions Numeric.Interval Data.Set
>>> :set prompt "\ESC[34mλ> \ESC[m"
>>> :l NITTA.Model.ProcessorUnits.Serial.Accum
-}

-- function for testing in ghci
go = let
        testAcc = [F.Push F.Plus (I "a"), F.Push F.Plus (I "b"), F.Pull (O $ fromList ["c", "z"]), F.Push F.Plus (I "x"), F.Push F.Minus (I "d"), F.Pull (O $ fromList ["f"])]
        test2Acc = [F.Push F.Plus (I "l"), F.Push F.Plus (I "e"), F.Pull (O $ fromList ["k"])]
        f = F.acc testAcc :: F String Int
        f2 = F.acc test2Acc :: F String Int
        st0 = accum :: Accum String Int Int
        Right st1 = tryBind f st0
        Right st22 = tryBind f2 st1

        st2 = endpointDecision st22 $ EndpointD (Target "l") (0...2)
        st3 = endpointDecision st2 $ EndpointD (Target "e") (0...2)
        st4 = endpointDecision st3 $ EndpointD (Source $ fromList ["k"]) (7...7)

        st5 = endpointDecision st4 $ EndpointD (Target "a") (8...8)
        st6 = endpointDecision st5 $ EndpointD (Target "b") (5...8)
        st8 = endpointDecision st6 $ EndpointD (Source $ fromList ["c","z"]) (7...9)

        st9 = endpointDecision st8 $ EndpointD (Target "x") (21...23)
        st10 = endpointDecision st9 $ EndpointD (Target "d") (25...28)
        st11 = endpointDecision st10 $ EndpointD (Source $ fromList ["f"]) (7...7)
    in
        (st4, endpointOptions st6)


go2 = let
        f = F.add "a1" "b1" ["c1"] :: F String Int
        st0 = accum :: Accum String Int Int
        Right st1 = tryBind f st0

        st2 = endpointDecision st1 $ EndpointD (Target "a1") (0...2)
        st3 = endpointDecision st2 $ EndpointD (Target "b1") (0...2)
        st4 = endpointDecision st3 $ EndpointD (Source $ fromList ["c1"]) (7...7)
    in
        (st4, endpointOptions st4)

------------------------------new version-----------------------------------------------

go' = let
        testAcc = [F.Push F.Plus (I "a1"), F.Push F.Plus (I "b1"), F.Pull (O $ fromList ["c1", "z1"]), F.Push F.Plus (I "x1"), F.Push F.Minus (I "d1"), F.Pull (O $ fromList ["f1"])]
        f = F.acc testAcc :: F String Int
        st0 = tryBindFunc f
        st1 = endpointDecisionFunc st0 "b1"
        st2 = endpointDecisionFunc st1 "a1"
        st3 = endpointDecisionFunc st2 "c1"
        st4 = endpointDecisionFunc st3 "z1"
        st5 = endpointDecisionFunc st4 "x1"
        st6 = endpointDecisionFunc st5 "d1"
        st7 = endpointDecisionFunc st6 "f1"
    in
        (st7, endpointOptionsFunc st1)


data FAccum v x =
    FAccum
        { model :: [[(Bool,v)]]
        , real  :: [[(Bool,v)]]
        , func  :: F v x
        } deriving (Eq, Show)


tryBindFunc f = FAccum {model = functionModel, real = [], func = f}
    where
        functionModel = concatMap (\(i, o) -> [i, map (\x -> (False, x)) o]) (setRemain f)

-- tryBindFunc' f f2 = FAccum {model = functionModel, real = [], func = f2}
--     where
--         functionModel = concatMap (\(i, o) -> [i, map (\x -> (False, x)) o]) (setRemain f)

endpointOptionsFunc FAccum {model=[]} = []

endpointOptionsFunc FAccum {model=(m:_), real=[]} = map snd m

endpointOptionsFunc FAccum {model=(m:ms), real=(r:rs)}
    | null ( m \\ r) && null ms = []
    | null $ m \\ r             = map snd $ head ms
    | otherwise                 = map snd $ m \\ r



endpointDecisionFunc a@FAccum {model=[], real} v = a

endpointDecisionFunc a@FAccum {model=model@(m:ms), real=[]} v = a {real=newRealCreate}
    where
        ([(neg, _)], _) = partition ((== v) . snd) m
        newRealCreate =[[(neg, v)]]

endpointDecisionFunc a@FAccum {model=model@(m:ms), real=real@(r:rs)} v
    | null $ m \\ r                               = endpointDecisionFunc a {model = ms} v
    | not ((==m) $ m \\ r) && length m > length r = a {model = newModel newRealInsert, real=newRealInsert}
    | otherwise                                   = a {model = newModel newRealAdd, real = newRealAdd}
        where
            ([val], _) = partition ((== v) . snd) m
            newRealInsert = (val : r) : rs
            newRealAdd = [val] : r : rs
            newModel (newReal:_)
                | null $ m \\ newReal = ms
                | otherwise           = model

----------------------------------------------------------------------------------------
instance ( VarValTime v x t, Num x
         ) => ProcessorUnit (Accum v x t) v x t where
    tryBind f pu@Accum{remain}
        | Just (F.Add a b c)  <- castF f = Right pu{ remain = tryBindFunc ( F.acc [F.Push F.Plus a, F.Push F.Plus b, F.Pull c] )  : remain }
        | Just (F.Sub a b c)  <- castF f = Right pu{ remain = tryBindFunc ( F.acc [F.Push F.Plus a, F.Push F.Minus b, F.Pull c])  : remain }
        | Just F.Acc{}        <- castF f = Right pu{ remain = tryBindFunc f : remain}
        | otherwise = Left $ "The function is unsupported by Accum: " ++ show f

    process = process_

    setTime t pu@Accum{} = pu{ tick=t }

setRemain f
    | Just (F.Acc (vs :: [F.Status v])) <- castF f = zip (F.pushStatusGroups vs) (F.pullStatusGroups vs)

--- NEW -----------------------------------------------------------------
instance ( VarValTime v x t, Num x) => EndpointProblem (Accum v x t) v t where
    endpointOptions Accum{ currentWork = Just (_, a@FAccum {real, model}), tick }
        | even (length model)  = map (\v -> EndpointO (Target v) $ TimeConstrain (tick ... maxBound) (singleton 1)) (endpointOptionsFunc a)
        | odd (length model) = [ EndpointO (Source $ fromList (endpointOptionsFunc a) ) $ TimeConstrain (tick + 3 ... maxBound) (1 ... maxBound) ]

    endpointOptions p@Accum{ remain, currentWork = Nothing, tick } =
        concatMap (\a -> endpointOptions p {currentWork = Just (tick + 1, a)}) remain

    endpointDecision pu@Accum{ currentWork=Just (t, a@FAccum {model, real}), currentWorkEndpoints, remain, isInit } d@EndpointD{ epdRole=Target v, epdAt }
        | not (null model) && even ( length model) = let
                fAccum@FAccum {model=newModel, real = (((neg, _):_):_)} = endpointDecisionFunc a v
                sel = if isInit then Init neg else Load neg
                (newEndpoints, process_') = runSchedule pu $ do
                    updateTick (sup epdAt)
                    scheduleEndpoint d $ scheduleInstruction epdAt sel
            in
                pu
                    { process_=process_'
                    , currentWork = Just(t, fAccum)
                    , currentWorkEndpoints=newEndpoints ++ currentWorkEndpoints
                    , doneAt=if null newModel
                        then Just $ sup epdAt + 3
                        else Nothing
                    , tick=sup epdAt
                    , isInit=null newModel
                    }

    endpointDecision pu@Accum{ currentWork=Just (t, a@FAccum {model, real, func}), currentWorkEndpoints, doneAt} d@EndpointD{ epdRole=Source v, epdAt }
        | not (null real) && odd ( length model) = let
                fAccum@FAccum {model=newModel} = foldl endpointDecisionFunc a (elems v)
                (newEndpoints, process_') = runSchedule pu $ do
                    endpoints <- scheduleEndpoint d $ scheduleInstruction (epdAt-1) Out
                    when (null newModel) $ do
                        high <- scheduleFunction (t ... sup epdAt) func
                        let low = endpoints ++ currentWorkEndpoints
                        establishVerticalRelations high low

                    updateTick (sup epdAt)
                    return endpoints
            in pu
                { process_=process_'
                -- , doneAt=if null newModel then Nothing else doneAt
                , doneAt=if null newModel then Nothing else doneAt
                , currentWork=if null newModel then Nothing else Just(t, fAccum)
                , currentWorkEndpoints=if null newModel then [] else newEndpoints ++ currentWorkEndpoints
                , tick=sup epdAt
                , isInit=null newModel
                }

    endpointDecision pu@Accum{remain, currentWork=Nothing, tick} d
        | let v = oneOf $ variables d
        , Just fAccum <- find (\FAccum {func} -> v `member` variables func) remain
        = endpointDecision pu {remain = remain \\ [fAccum], currentWork = Just (tick+1, fAccum), isInit = True } d


    endpointDecision pu  d = error $ "error in Endpoint Decision function" ++ show pu ++ show d

instance Controllable (Accum v x t) where
    data Instruction (Accum v x t) = Init Bool | Load Bool | Out deriving (Show)

    data Microcode (Accum v x t) =
        Microcode
            { oeSignal   :: Bool
            , initSignal :: Bool
            , loadSignal :: Bool
            , negSignal  :: Maybe Bool
            } deriving ( Show, Eq, Ord )

    mapMicrocodeToPorts Microcode{..} AccumPorts{..} =
        [ (init, Bool initSignal)
        , (load, Bool loadSignal)
        , (neg, maybe Undef Bool negSignal)
        , (oe, Bool oeSignal)
        ]

    portsToSignals AccumPorts{ init, load, neg, oe } = [init, load, neg, oe]
    --
    signalsToPorts (init:load:neg:oe:_) _ = AccumPorts init load neg oe
    signalsToPorts _                    _ = error "pattern match error in signalsToPorts AccumPorts"
--
instance Default (Microcode (Accum v x t)) where
    def = Microcode
        { oeSignal=False
        , initSignal=False
        , loadSignal=False
        , negSignal=Nothing
        }
--
instance UnambiguouslyDecode (Accum v x t) where
    decodeInstruction (Init neg) = def{ initSignal=False, loadSignal=True, negSignal=Just neg }
    decodeInstruction (Load neg) = def{ initSignal=True, loadSignal=True, negSignal=Just neg }
    decodeInstruction Out        = def{ oeSignal=True }
--
--
instance ( VarValTime v x t
         , Num x
         ) => Simulatable (Accum v x t) v x where
  simulateOn cntx _ f
    | Just f'@F.Add{} <- castF f = simulate cntx f'
    | Just f'@F.Sub{} <- castF f = simulate cntx f'
    | Just f'@F.Acc{} <- castF f = simulate cntx f'
    | otherwise = error $ "Can't simulate " ++ show f ++ " on Accum."
--
--
instance Connected (Accum v x t) where
    data Ports (Accum v x t)
        = AccumPorts{ init, load, neg, oe :: SignalTag } deriving ( Show )


instance IOConnected (Accum v x t) where
    data IOPorts (Accum v x t) = AccumIO

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
                , .signal_init( { signal init } )
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

instance ( VarValTime v x t ) => Default (Accum v x t) where
    def = accum

instance IOTestBench (Accum v x t) v x

instance RefactorProblem (Accum v x t) v x
