{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}

module NITTA.ProcessUnits.Divisor
    ( Pipeline(..)
    , PUPorts(..)
    , DivisorSt(..)
    , divisor
    ) where

import           Data.Default
import           Data.List                           (find, partition)
import           Data.Set                            (Set, difference,
                                                      isSubsetOf)
import qualified Data.Set                            as S
import           NITTA.FunctionBlocks                (castFB)
import qualified NITTA.FunctionBlocks                as FB
import           NITTA.ProcessUnits.Generic.Pipeline
import           NITTA.Types
import           NITTA.Utils
import           NITTA.Utils.Process
import           Numeric.Interval                    (inf, singleton, sup,
                                                      (...))


type Divisor v x t = Pipeline (DivisorSt v) v x t
instance ( Default t ) => Default (Divisor v x t) where
    def = Pipeline def def def def 4 1 DivisorSt{ mock=False }

divisor :: ( Default t ) => Divisor v x t
divisor = def


instance ( Var v
         , Integral x
         ) => Simulatable (Divisor v x t) v x where
    simulateOn cntx _ fb
        | Just fb'@FB.Division{} <- castFB fb = simulate cntx fb'
        | otherwise = error $ "Can't simulate " ++ show fb ++ " on Shift."



data InputDesc
    = Numer
    | Denom
    deriving ( Show, Eq )

data OutputDesc
    = Quotient
    | Remain
    deriving ( Show, Eq )


newtype DivisorSt v
    = DivisorSt
        { mock           :: Bool
        }
    deriving ( Show )

instance PipelineTF (DivisorSt v) where
    data InputSt (DivisorSt v) = DivisorIn ([(v, InputDesc)], (Set v, Set v))
    data OutputSt (DivisorSt v) = DivisorOut [(Set v, OutputDesc)]


instance PipelinePU (DivisorSt v) (Parcel v x) where
    bindPipeline fb
        | Just (FB.Division (I n) (I d_) (O q) (O r)) <- castFB fb
        , let inputSt = DivisorIn ( [(d_, Denom), (n, Numer)], (q, r) )
        , let expire = 2
        = Right ( inputSt, expire )
        | otherwise = Left $ "Unknown functional block: " ++ show fb


instance ( Time t, Var v ) => PipelinePU2 (DivisorSt v) v t where
    targetOptions nextTick (DivisorIn (vs, _)) = map (target . fst) vs
        where
            target v = EndpointO (Target v) $ TimeConstrain (nextTick ... maxBound) (singleton 1)

    targetDecision EndpointD{ epdRole=Target var, epdAt=at } D{ tick, latency, inputSt }
        | Just arg <- argType inputSt var
        , let ( inputSt', outputStTail ) = pipelineChanges inputSt var
        = Just ( inputSt', outputStTail, scheduleInput arg )
        | otherwise = error "Divisor: targetDecision."
        where
            argType (DivisorIn (inputs_, _)) v_ = snd <$> find ((==) v_ . fst) inputs_
            pipelineChanges (DivisorIn (inputs_, (q, r))) v
                | length inputs_ > 1 =
                    ( Just $ DivisorIn ( filter ((/=) v . fst) inputs_, (q, r) )
                    , Nothing
                    )
                | otherwise =
                    ( Nothing
                    , Just PipelineOut
                        { complete=tick + toEnum (latency + 3)
                        , expired=Nothing
                        , outputSt=DivisorOut [(q, Quotient), (r, Remain)]
                        }
                    )
            scheduleInput arg = do
                t <- nextScheduledTick
                scheduleNopAndUpdateTick t (inf at - 1)
                scheduleInstructionAndUpdateTick (inf at) (sup at) $ Load arg
    targetDecision _ _ = Nothing

    sourceOptions begin end maxDuration (DivisorOut outs)
        =
            [ EndpointO (Source vs) $ TimeConstrain (begin ... end) (1 ... maxDuration)
            | (vs, _) <- outs
            ]

    sourceDecision d@EndpointD{ epdRole=Source vs, epdAt } (DivisorOut outs)
        | ([(waitingVs, sel)], os) <- partition ((vs `isSubsetOf`) . fst) outs
        , let
            waitingVs' = waitingVs `difference` vs
            sch = scheduleEndpoint d $ do
                nextTick <- nextScheduledTick
                scheduleNopAndUpdateTick nextTick (inf epdAt - 1)
                scheduleInstructionAndUpdateTick (inf epdAt) (sup epdAt) $ Out sel
            os' = if S.null waitingVs'
                    then os
                    else (waitingVs', sel) : os
        = Just ( if null os' then Nothing else Just $ DivisorOut os'
            , sch
            )
    sourceDecision _ _ = Nothing



instance Controllable (Divisor v x t) where
    data Instruction (Divisor v x t)
        = Nop
        | Load InputDesc
        | Out OutputDesc
        deriving (Show)
    nop = Nop

    data Microcode (Divisor v x t)
        = Microcode
            { wrSignal :: Bool
            , wrSelSignal :: Bool
            , oeSignal :: Bool
            , oeSelSignal :: Bool
            } deriving ( Show, Eq, Ord )


instance UnambiguouslyDecode (Divisor v x t) where
    decodeInstruction Nop = Microcode
        { wrSignal=False
        , wrSelSignal=False
        , oeSignal=False
        , oeSelSignal=False
        }
    decodeInstruction (Load Numer)   = (decodeInstruction Nop){ wrSignal=True, wrSelSignal=False }
    decodeInstruction (Load Denom)   = (decodeInstruction Nop){ wrSignal=True, wrSelSignal=True }
    decodeInstruction (Out Quotient) = (decodeInstruction Nop){ oeSignal=True, oeSelSignal=False }
    decodeInstruction (Out Remain)   = (decodeInstruction Nop){ oeSignal=True, oeSelSignal=True }


instance Connected (Divisor v x t) where
    data PUPorts (Divisor v x t)
        = PUPorts{ wr, wrSel, oe, oeSel :: Signal }
        deriving ( Show )
    transmitToLink Microcode{..} PUPorts{..}
        =
            [ (wr, Bool wrSignal)
            , (wrSel, Bool wrSelSignal)
            , (oe, Bool oeSignal)
            , (oeSel, Bool oeSelSignal)
            ]


instance ( Time t, Var v
         ) => TargetSystemComponent (Divisor v x t) where
    moduleName _ _ = "pu_div"
    software _ _ = Empty
    hardware title pu@Pipeline{ state=DivisorSt{ mock } } = Aggregate Nothing
        [ if mock
            then FromLibrary "div/div_mock.v"
            else FromLibrary "div/div.v"
        , FromLibrary $ "div/" ++ moduleName title pu ++ ".v"
        ]
    hardwareInstance title Pipeline{ pipeline, state=DivisorSt{ mock } } Enviroment{ net=NetEnv{..}, signalClk, signalRst } PUPorts{..} = renderMST
        [ "pu_div"
        , "  #( .DATA_WIDTH( " ++ show parameterDataWidth ++ " )"
        , "   , .ATTR_WIDTH( " ++ show parameterAttrWidth ++ " )"
        , "   , .INVALID( 0 )" -- FIXME:
        , "   , .PIPELINE( " ++ show pipeline ++ " )" -- FIXME:
        , "   , .MOCK_DIV( " ++ bool2verilog mock ++ " )"
        , "   ) $name$"
        , "  ( .clk( " ++ signalClk ++ " )"
        , "  , .rst( " ++ signalRst ++ " )"
        , "  , .signal_wr( " ++ signal wr ++ " )"
        , "  , .signal_wr_sel( " ++ signal wrSel ++ " )"
        , "  , .data_in( " ++ dataIn ++ " )"
        , "  , .attr_in( " ++ attrIn ++ " )"
        , "  , .signal_oe( " ++ signal oe ++ " )"
        , "  , .signal_oe_sel( " ++ signal oeSel ++ " )"
        , "  , .data_out( " ++ dataOut ++ " )"
        , "  , .attr_out( " ++ attrOut ++ " )"
        , "  );"
        ] [("name", title)]

