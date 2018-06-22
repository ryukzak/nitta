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

module NITTA.ProcessUnits.Div
    ( Pipeline(..)
    , PUPorts(..)
    , DivSt(..)
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


type Div v x t = Pipeline (DivSt v) v x t
instance ( Default t ) => Default (Div v x t) where
    def = Pipeline def def def def 4 1 DivSt{ mock=False }

divisor :: ( Default t ) => Div v x t
divisor = def


instance ( Var v
         , Integral x
         ) => Simulatable (Div v x t) v x where
    simulateOn cntx _ fb
        | Just fb'@FB.Div{} <- castFB fb = simulate cntx fb'
        | otherwise = error $ "Can't simulate " ++ show fb ++ " on Shift."



data InputDesc
    = Numer
    | Denom
    deriving ( Show, Eq )

data OutputDesc
    = Quotient
    | Remain
    deriving ( Show, Eq )


newtype DivSt v
    = DivSt
        { mock           :: Bool
        }
    deriving ( Show )

instance PipelineTF (DivSt v) where
    data InputSt (DivSt v) = DivIn ([(v, InputDesc)], (Set v, Set v))
    data OutputSt (DivSt v) = DivOut [(Set v, OutputDesc)]


instance PipelinePU (DivSt v) (Parcel v x) where
    bindPipeline fb
        | Just (FB.Div (I n) (I d_) (O q) (O r)) <- castFB fb
        , let inputSt = DivIn ( [(d_, Denom), (n, Numer)], (q, r) )
        , let expire = 2
        = Right ( inputSt, expire )
        | otherwise = Left $ "Unknown functional block: " ++ show fb


instance ( Time t, Var v ) => PipelinePU2 (DivSt v) v t where
    targetOptions nextTick (DivIn (vs, _)) = map (target . fst) vs
        where
            target v = EndpointO (Target v) $ TimeConstrain (nextTick ... maxBound) (singleton 1)

    targetDecision EndpointD{ epdRole=Target var, epdAt=at } D{ tick, latency, inputSt }
        | Just arg <- argType inputSt var
        , let ( inputSt', outputStTail ) = pipelineChanges inputSt var
        = Just ( inputSt', outputStTail, scheduleInput arg )
        | otherwise = error "Div: targetDecision."
        where
            argType (DivIn (inputs_, _)) v_ = snd <$> find ((==) v_ . fst) inputs_
            pipelineChanges (DivIn (inputs_, (q, r))) v
                | length inputs_ > 1 =
                    ( Just $ DivIn ( filter ((/=) v . fst) inputs_, (q, r) )
                    , Nothing
                    )
                | otherwise =
                    ( Nothing
                    , Just PipelineOut
                        { complete=tick + toEnum (latency + 3)
                        , expired=Nothing
                        , outputSt=DivOut [(q, Quotient), (r, Remain)]
                        }
                    )
            scheduleInput arg = do
                t <- nextScheduledTick
                scheduleNopAndUpdateTick t (inf at - 1)
                scheduleInstructionAndUpdateTick (inf at) (sup at) $ Load arg
    targetDecision _ _ = Nothing

    sourceOptions begin end maxDuration (DivOut outs)
        =
            [ EndpointO (Source vs) $ TimeConstrain (begin ... end) (1 ... maxDuration)
            | (vs, _) <- outs
            ]

    sourceDecision d@EndpointD{ epdRole=Source vs, epdAt } (DivOut outs)
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
        = Just ( if null os' then Nothing else Just $ DivOut os'
            , sch
            )
    sourceDecision _ _ = Nothing



instance Controllable (Div v x t) where
    data Instruction (Div v x t)
        = Nop
        | Load InputDesc
        | Out OutputDesc
        deriving (Show)
    nop = Nop

    data Microcode (Div v x t)
        = Microcode
            { wrSignal :: Bool
            , wrSelSignal :: Bool
            , oeSignal :: Bool
            , oeSelSignal :: Bool
            } deriving ( Show, Eq, Ord )


instance UnambiguouslyDecode (Div v x t) where
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


instance Connected (Div v x t) where
    data PUPorts (Div v x t)
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
         ) => TargetSystemComponent (Div v x t) where
    moduleName _ _ = "pu_div"
    software _ _ = Empty
    hardware title pu@Pipeline{ state=DivSt{ mock } } = Aggregate Nothing
        [ if mock
            then FromLibrary "div/div_mock.v"
            else FromLibrary "div/div.v"
        , FromLibrary $ "div/" ++ moduleName title pu ++ ".v"
        ]
    hardwareInstance title Pipeline{ pipeline, state=DivSt{ mock } } Enviroment{ net=NetEnv{..}, signalClk, signalRst } PUPorts{..} = renderMST
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

