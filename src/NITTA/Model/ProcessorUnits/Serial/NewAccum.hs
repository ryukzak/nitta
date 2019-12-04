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
{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}
{-|
Module      : NITTA.Model.ProcessorUnits.Serial.Accum
Description :
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Model.ProcessorUnits.Serial.NewAccum
  ( accum
  , Accum
  , Ports(..), IOPorts(..)
  ) where

import           Control.Monad                    (when)
import           Data.Bits                        (finiteBitSize)
import           Data.Default
import           Data.List                        (nub, init, find, partition, (\\))
import           Data.Set                         (elems, fromList, member)
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
    { remain               :: [([(Bool, v)], [v])] -- [([Target], [Source])] for only one function
    , funcs                :: [F v x]
    , targets              :: [ (Bool, v) ]
    , sources              :: [ v ]
    , doneAt               :: Maybe t
    , currentWork          :: Maybe ( t, F v x )
    , currentWorkEndpoints :: [ ProcessUid ]
    , process_             :: Process v x t
    , tick                 :: t
    , isInit               :: Bool
    }
    -- deriving ( Show )

instance (VarValTime v x t) => Show (Accum v x t) where
    show a = "Accum:\n"
        ++ "\tremain               = " ++ show (remain a) ++ "\n"
        ++ "\tfuncs                = " ++ show (funcs a) ++ "\n"
        ++ "\ttargets              = " ++ show (targets a) ++ "\n"
        ++ "\tsources              = " ++ show (sources a) ++ "\n"
        ++ "\tdoneAt               = " ++ show (doneAt a) ++ "\n"
        ++ "\tcurrentWork          = " ++ show (currentWork a) ++ "\n"
        ++ "\tcurrentWorkEndpoints = " ++ show (currentWorkEndpoints a) ++ "\n"
        ++ "\tprocess_             = " ++ show (process_ a) ++ "\n"
        ++ "\ttick_                = " ++ show (tick a) ++ "\n"
        ++ "\tisInit               = " ++ show (isInit a) ++ "\n"


accum :: (VarValTime v x t) => Accum v x t
accum = Accum
    { remain=[]
    , funcs=[]
    , targets=[]
    , sources=[]
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
>>> :l NITTA.Model.ProcessorUnits.Serial.NewAccum
-}

-- function for testing in ghci
go = let
        testAcc = [F.Push F.Plus (I "a"), F.Push F.Plus (I "b"), F.Pull (O $ fromList ["c", "z"]), F.Push F.Plus (I "x"), F.Push F.Minus (I "d"), F.Pull (O $ fromList ["f"])]
        test2Acc = [F.Push F.Plus (I "l"), F.Push F.Plus (I "e"), F.Pull (O $ fromList ["c"])]
        f = F.acc testAcc :: F String Int
        f2 = F.acc test2Acc :: F String Int
        st0 = accum :: Accum String Int Int
        Right st1 = tryBind f st0
        Right st22 = tryBind f2 st1

        st2 = endpointDecision st22 $ EndpointD (Target "l") (0...2)
        st3 = endpointDecision st2 $ EndpointD (Target "e") (0...2)
        st4 = endpointDecision st3 $ EndpointD (Source $ fromList ["c"]) (7...7)

        st5 = endpointDecision st4 $ EndpointD (Target "a") (8...8)
        st6 = endpointDecision st5 $ EndpointD (Target "b") (5...8)
        st7 = endpointDecision st6 $ EndpointD (Source $ fromList ["c"]) (7...7)
        st8 = endpointDecision st7 $ EndpointD (Source $ fromList ["z"]) (7...7)

        st9 = endpointDecision st8 $ EndpointD (Target "x") (21...23)
        st10 = endpointDecision st9 $ EndpointD (Target "d") (25...28)
        st11 = endpointDecision st10 $ EndpointD (Source $ fromList ["f"]) (7...7)
    in
        (st11, endpointOptions st11)




instance ( VarValTime v x t
         ) => ProcessorUnit (Accum v x t) v x t where
    tryBind f pu@Accum{funcs}
        | Just (F.Acc (_ :: [F.Status v])) <- castF f = Right pu{ funcs = f : funcs}
        | otherwise = Left $ "The function is unsupported by Accum: " ++ show f

    process = process_

    setTime t pu@Accum{} = pu{ tick=t }


setRemain f
    | Just (F.Acc (vs :: [F.Status v])) <- castF f = zip (F.pushStatusGroups vs) (F.pullStatusGroups vs)

assignmentFunc pu@Accum{ currentWork=Nothing, funcs, tick } f =
    assignment pu
        { remain = setRemain f
        , funcs = funcs \\ [f]
        , currentWork=Just (tick + 1, f)
        }

assignmentFunc _ _ = error "Accum: internal assignment function error."

assignment pu@Accum{ targets=[], sources=[], remain=((targetsRem, sourceRem) : xs) } =
    pu
        { targets=targetsRem
        , sources=sourceRem
        , remain=xs
        }

assignment _ = error "Accum: internal assignment error."

instance ( VarValTime v x t) => EndpointProblem (Accum v x t) v t where
    endpointOptions Accum{ targets=vs@(_:_), tick }
        = map (\v -> EndpointO (Target v) $ TimeConstrain (tick ... maxBound) (singleton 1)) (map snd vs)

    endpointOptions Accum{ sources, doneAt=Just _, tick }
        | not $ null sources
        = [ EndpointO (Source $ fromList sources) $ TimeConstrain (tick + 3 ... maxBound) (1 ... maxBound) ]

    endpointOptions pu@Accum{ funcs, tick } = let
            getTarget f = concatMap fst (setRemain f)
        in
            concatMap (\f -> endpointOptions pu {targets = getTarget f, tick}) funcs


    endpointDecision pu@Accum{ targets=vs,currentWorkEndpoints, remain, isInit } d@EndpointD{ epdRole=Target v, epdAt }
        | ([(neg, _)], xs) <- partition ((== v) . snd) vs
        , let
            sel = if isInit then Init neg else Load neg
            (newEndpoints, process_') = runSchedule pu $ do
                updateTick (sup epdAt)
                scheduleEndpoint d $ scheduleInstruction epdAt sel
        = pu
            { process_=process_'
            , targets=xs
            , currentWorkEndpoints=newEndpoints ++ currentWorkEndpoints
            , doneAt=if null remain
                then Just $ sup epdAt + 3
                else Nothing
            , tick=sup epdAt
            , isInit=null remain && null vs
            }

    endpointDecision pu@Accum{ targets=[], sources, doneAt, currentWork=Just (a, f), currentWorkEndpoints, remain } d@EndpointD{ epdRole=Source v, epdAt }
        | not $ null sources
        , let sources' = sources \\ elems v
        , sources' /= sources
        , let (newEndpoints, process_') = runSchedule pu $ do
                endpoints <- scheduleEndpoint d $ scheduleInstruction (epdAt-1) Out
                when (null remain) $ do
                    high <- scheduleFunction (a ... sup epdAt) f
                    let low = endpoints ++ currentWorkEndpoints
                    establishVerticalRelations high low

                updateTick (sup epdAt)
                return endpoints
        = pu
            { process_=process_'
            , sources=sources'
            , doneAt=if null remain then Nothing else doneAt
            , currentWork=if null remain then Nothing else Just (a, f)
            , currentWorkEndpoints=if null remain then [] else newEndpoints ++ currentWorkEndpoints
            , tick=sup epdAt
            , isInit=null remain && null sources
            }

    endpointDecision pu@Accum{ targets=[], sources=[], funcs, currentWork=Nothing} d
        | let v = oneOf $ variables d
        , Just f <- find (\f -> v `member` variables f) funcs
        = endpointDecision (assignmentFunc pu {isInit = True} f) d

    endpointDecision pu@Accum{ targets=[], sources=[], currentWork=Just(_, f)} d
        = endpointDecision (assignment pu) d

    endpointDecision pu d = error $ "Accum decision error\npu: " ++ show pu ++ ";\n decison:" ++ show d

instance Controllable (Accum v x t) where
    data Instruction (Accum v x t) = Init Bool | Load Bool | Out deriving (Show)
    -- data Microcode (Accum v x t) =
    --     Microcode
    --         { oeSignal :: Bool
    --         , initSignal :: Bool
    --         , loadSignal :: Bool
    --         , negSignal :: Maybe Bool
    --         } deriving ( Show, Eq, Ord )
    --
    -- mapMicrocodeToPorts Microcode{..} AccumPorts{..} =
    --     [ (init, Bool initSignal)
    --     , (load, Bool loadSignal)
    --     , (neg, maybe Undef Bool negSignal)
    --     , (oe, Bool oeSignal)
    --     ]
    --
    -- portsToSignals AccumPorts{ init, load, neg, oe } = [init, load, neg, oe]
    --
    -- signalsToPorts (init:load:neg:oe:_) _ = AccumPorts init load neg oe
    -- signalsToPorts _                    _ = error "pattern match error in signalsToPorts AccumPorts"
--
-- instance Default (Microcode (Accum v x t)) where
--     def = Microcode
--         { oeSignal=False
--         , initSignal=False
--         , loadSignal=False
--         , negSignal=Nothing
--         }
--
-- instance UnambiguouslyDecode (Accum v x t) where
--     decodeInstruction (Init neg) = def{ initSignal=False, loadSignal=True, negSignal=Just neg }
--     decodeInstruction (Load neg) = def{ initSignal=True, loadSignal=True, negSignal=Just neg }
--     decodeInstruction Out        = def{ oeSignal=True }
--
--
-- instance ( VarValTime v x t
--          , Num x
--          ) => Simulatable (Accum v x t) v x where
--   simulateOn cntx _ f
--     | Just f'@F.Add{} <- castF f = simulate cntx f'
--     | Just f'@F.Sub{} <- castF f = simulate cntx f'
--     | otherwise = error $ "Can't simulate " ++ show f ++ " on Accum."
--
--
instance Connected (Accum v x t) where
    -- data Ports (Accum v x t)
    --     = AccumPorts{ init, load, neg, oe :: SignalTag } deriving ( Show )
--
-- instance IOConnected (Accum v x t) where
--     data IOPorts (Accum v x t) = AccumIO
--
--
-- instance ( Var v ) => Locks (Accum v x t) v where
--     locks Accum{ remain, sources, targets } =
--         [ Lock{ lockBy, locked }
--         | locked <- sources
--         , lockBy <- snds targets
--         ]
--
--         ++
--
--         [ Lock{ lockBy, locked }
--         | locked <- concatMap (elems . variables) remain
--         , lockBy <- sources ++ snds targets
--         ]
--
-- instance ( Val x, Default x ) => TargetSystemComponent (Accum v x t) where
--     moduleName _ _ = "pu_accum"
--     hardware tag pu = FromLibrary $ moduleName tag pu ++ ".v"
--     software _ _ = Empty
--     hardwareInstance tag _pu TargetEnvironment{ unitEnv=ProcessUnitEnv{..}, signalClk, signalRst } AccumPorts{..} AccumIO
--         = codeBlock [qc|
--             pu_accum #
--                     ( .DATA_WIDTH( { finiteBitSize (def :: x) } )
--                     , .ATTR_WIDTH( { show parameterAttrWidth } )
--                     ) { tag }
--                 ( .clk( { signalClk } )
--                 , .rst( { signalRst } )
--                 , .signal_init( { signal init } )
--                 , .signal_load( { signal load } )
--                 , .signal_neg( { signal neg } )
--                 , .signal_oe( { signal oe } )
--                 , .data_in( { dataIn } )
--                 , .attr_in( { attrIn } )
--                 , .data_out( { dataOut } )
--                 , .attr_out( { attrOut } )
--                 );
--             |]
--     hardwareInstance _title _pu TargetEnvironment{ unitEnv=NetworkEnv{} } _ports _io
--         = error "Should be defined in network."
--
-- instance ( VarValTime v x t ) => Default (Accum v x t) where
--     def = accum
--
-- instance IOTestBench (Accum v x t) v x
--
-- instance RefactorProblem (Accum v x t) v x
--
-- snds = map snd
