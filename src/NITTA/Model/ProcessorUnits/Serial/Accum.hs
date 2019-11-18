{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
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
import           Data.Maybe.HT                    (toMaybe)
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
    { remain               :: [ F v x ]
    , targets              :: [ (Bool, v) ]
    , sources              :: [ v ]
    , doneAt               :: Maybe t
    , currentWork          :: Maybe ( t, F v x )
    , currentWorkEndpoints :: [ ProcessUid ]
    , process_             :: Process v x t
    , tick                 :: t
    }
    deriving ( Show )

accum :: (VarValTime v x t) => Accum v x t
accum = Accum
    { remain=[]
    , targets=[]
    , sources=[]
    , doneAt=Nothing
    , currentWork=Nothing
    , currentWorkEndpoints=[]
    , process_=def
    , tick=def
    }

instance ( VarValTime v x t
         ) => ProcessorUnit (Accum v x t) v x t where
    tryBind f pu@Accum{ remain }
        | Just F.Add {} <- castF f = Right pu{ remain=f : remain }
        | Just F.Sub {} <- castF f = Right pu{ remain=f : remain }
        | otherwise = Left $ "The function is unsupported by Accum: " ++ show f

    process = process_

    setTime t pu@Accum{} = pu{ tick=t }


execution pu@Accum{ targets=[], sources=[], remain, tick } f
    | Just (F.Add (I a) (I b) (O c)) <- castF f
    = pu
        { targets=[(False, a), (False, b)]
        , currentWork=Just (tick + 1, f)
        , sources=elems c
        , remain=remain \\ [ f ]
        }
    | Just (F.Sub (I a) (I b) (O c)) <- castF f
    = pu
        -- mark True when value is negative
        { targets=[(False, a), (True, b)]
        , currentWork=Just (tick + 1, f)
        , sources=elems c
        , remain=remain \\ [ f ]
        }
execution _ _ = error "Accum: internal execution error."


instance ( VarValTime v x t
        ) => EndpointProblem (Accum v x t) v t
        where

    endpointOptions Accum{ targets=vs@(_:_), tick }
        = map (\(_, v) -> EndpointO (Target v) $ TimeConstrain (tick ... maxBound) (singleton 1)) vs

    endpointOptions Accum{ sources, doneAt=Just _, tick }
        | not $ null sources
        = [ EndpointO (Source $ fromList sources) $ TimeConstrain (tick + 3 ... maxBound) (1 ... maxBound) ]

    endpointOptions pu@Accum{ remain } = concatMap (endpointOptions . execution pu) remain


    endpointDecision pu@Accum{ targets=vs, currentWorkEndpoints } d@EndpointD{ epdRole=Target v, epdAt }
        | ([(neg, _)], xs) <- partition ((== v) . snd) vs
        , let sel = if null xs then Init neg else Load neg
              (newEndpoints, process_') = runSchedule pu $ do
                    updateTick (sup epdAt)
                    scheduleEndpoint d $ scheduleInstruction epdAt sel
        = pu
            { process_=process_'
            , targets=xs
            , currentWorkEndpoints=newEndpoints ++ currentWorkEndpoints
            , doneAt=toMaybe (null xs) (sup epdAt + 3)
            , tick=sup epdAt
            }

    endpointDecision pu@Accum{ targets=[], sources, doneAt, currentWork=Just (a, f), currentWorkEndpoints } d@EndpointD{ epdRole=Source v, epdAt }
        | not $ null sources
        , let sources' = sources \\ elems v
        , sources' /= sources
        , let (newEndpoints, process_') = runSchedule pu $ do
                endpoints <- scheduleEndpoint d $ scheduleInstruction (epdAt-1) Out
                when (null sources') $ do
                    high <- scheduleFunction (a ... sup epdAt) f
                    let low = endpoints ++ currentWorkEndpoints
                    establishVerticalRelations high low

                updateTick (sup epdAt)
                return endpoints
        = pu
            { process_=process_'
            , sources=sources'
            , doneAt=if null sources' then Nothing else doneAt
            , currentWork= toMaybe (not $ null sources') (a, f)
            , currentWorkEndpoints=if null sources' then [] else newEndpoints ++ currentWorkEndpoints
            , tick=sup epdAt
            }

    endpointDecision pu@Accum{ targets=[], sources=[], remain } d
        | let v = oneOf $ variables d
        , Just f <- find (\f -> v `member` variables f) remain
        = endpointDecision (execution pu f) d

    endpointDecision pu d = error $ "Accum decision error\npu: " ++ show pu ++ ";\n decison:" ++ show d


instance Controllable (Accum v x t) where
    data Instruction (Accum v x t) = Init Bool | Load Bool | Out deriving (Show)
    data Microcode (Accum v x t) =
        Microcode
            { oeSignal :: Bool
            , resetAccSignal :: Bool
            , loadSignal :: Bool
            , negSignal :: Maybe Bool
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
    decodeInstruction (Init neg) = def{ resetAccSignal=False, loadSignal=True, negSignal=Just neg }
    decodeInstruction (Load neg) = def{ resetAccSignal=True, loadSignal=True, negSignal=Just neg }
    decodeInstruction Out        = def{ oeSignal=True }


instance ( VarValTime v x t
         , Num x
         ) => Simulatable (Accum v x t) v x where
  simulateOn cntx _ f
    | Just f'@F.Add{} <- castF f = simulate cntx f'
    | Just f'@F.Sub{} <- castF f = simulate cntx f'
    | otherwise = error $ "Can't simulate " ++ show f ++ " on Accum."


instance Connected (Accum v x t) where
    data Ports (Accum v x t)
        = AccumPorts{ resetAcc, load, neg, oe :: SignalTag } deriving ( Show )

instance IOConnected (Accum v x t) where
    data IOPorts (Accum v x t) = AccumIO

instance ( Var v ) => Locks (Accum v x t) v where
    locks Accum{ remain, sources, targets } =
        [ Lock{ lockBy, locked }
        | locked <- sources
        , lockBy <- map snd targets
        ]
        ++
        [ Lock{ lockBy, locked }
        | locked <- concatMap (elems . variables) remain
        , lockBy <- sources ++ map snd targets
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
                , .signal_init( { signal resetAcc } )
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
