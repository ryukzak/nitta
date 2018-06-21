{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NamedFieldPuns         #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}

module NITTA.ProcessUnits.Div
    ( Pipeline(..)
    , PUPorts(..)
    , DivSt(..)
    , divisor
    ) where

import           Control.Monad.State
import           Data.Default
import           Data.Either          (rights)
import           Data.List            (find, partition)
import           Data.Maybe           (fromMaybe, maybeToList)
import           Data.Proxy           (asProxyTypeOf)
import           Data.Set             (Set, difference, isSubsetOf, member)
import qualified Data.Set             as S
import           Data.Typeable
import           NITTA.FunctionBlocks (castFB)
import qualified NITTA.FunctionBlocks as FB
import           NITTA.Types
import           NITTA.Utils
import           Numeric.Interval     (inf, singleton, sup, (...), Interval)


data InputDesc
    = Numer
    | Denom
    deriving ( Show, Eq )

data OutputDesc
    = Quotient
    | Remain
    deriving ( Show, Eq )

newtype DivIn v = DivIn ([(v, InputDesc)], (Set v, Set v))
newtype DivOut v = DivOut [(Set v, OutputDesc)]
newtype DivSt
    = DivSt
        { mock           :: Bool
        }
    deriving ( Show )


type Div v x t = Pipeline DivSt (DivIn v) (DivOut v) v x t
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




bindPipeline fb
    | Just (FB.Div (I n) (I d_) (O q) (O r)) <- castFB fb
    , let inputSt = DivIn ( [(d_, Denom), (n, Numer)], (q, r) )
    , let expire = 2
    = Right ( inputSt, expire )
    | otherwise = Left $ "Unknown functional block: " ++ show fb


targetOptions nextTick (DivIn (vs, _)) = map (target . fst) vs
    where
        target v = EndpointO (Target v) $ TimeConstrain (nextTick ... maxBound) (singleton 1)


sourceOptions begin end maxDuration (DivOut outs)
    =
        [ EndpointO (Source vs) $ TimeConstrain (begin ... end) (1 ... maxDuration)
        | (vs, _) <- outs
        ]




data PipelineOut o t
    = PipelineOut
        { expired  :: Maybe t
        , complete :: t
        , outputSt :: o
        }
    deriving ( Show )

data Pipeline st i o v x t
    = Pipeline
        { pipeInput      :: Maybe i
        , pipeOutput     :: [PipelineOut o t]
        , puRemain       :: [FB (Parcel v x)]
        , puProcess      :: Process (Parcel v x) t
        , pipeline       :: Int
        , frequencyRatio :: Int
        , state          :: st
        }
    deriving ( Show )



instance ( Var v, Time t
         ) => ProcessUnit (Div v x t) (Parcel v x) t where
    bind fb pu@Pipeline{ puRemain }
        = case bindPipeline fb of
            Right _  -> Right pu{ puRemain=fb : puRemain }
            Left err -> Left $ "Unknown functional block: " ++ err
    process = puProcess
    setTime t pu@Pipeline{ puProcess } = pu{ puProcess=puProcess{ nextTick=t } }


instance ( Var v, Time t
         , Typeable x
         ) => DecisionProblem (EndpointDT v t)
                   EndpointDT (Div v x t) where
    options _proxy pu@Pipeline{ puRemain, puProcess=Process{ nextTick } }
        = targets pu ++ sources pu
        where
            targets Pipeline{ pipeInput=Just pipelineIn } = targetOptions nextTick pipelineIn
            targets Pipeline{ pipeInput=Nothing } = concatMap (targetOptions nextTick . fst) $ rights $ map bindPipeline puRemain

            sources Pipeline{ pipeOutput=PipelineOut{ complete, expired, outputSt } : _ }
                = let
                    begin = max complete nextTick
                    end = fromMaybe maxBound expired
                    maxDuration = maybe maxBound (+ (-begin)) expired
                in
                    sourceOptions begin end maxDuration outputSt
            sources Pipeline{} = []

    decision
            proxy
            pu@Pipeline{ pipeInput=Nothing, puRemain, pipeOutput, pipeline }
            d@EndpointD{ epdRole=Target v, epdAt }
        | Just fb <- find (member v . inputs) puRemain
        , Right (inputSt, expire) <- bindPipeline fb
        = let
            pu' = pu
                { puRemain=filter (/= fb) puRemain
                , pipeInput=Just inputSt
                , pipeOutput=case pipeOutput of
                    out@PipelineOut{ expired=Nothing } : outs
                        -> let out' = out{ expired=Just $ inf epdAt + toEnum (pipeline + expire) }
                        in out' : outs
                    [] -> []
                    _ -> error "wrong internal state of Pipeline."
                }
        in
            decision proxy pu' d

    decision
            _proxy
            pu@Pipeline{ pipeInput=Just inputSt, pipeOutput, pipeline, puProcess=Process{ nextTick } }
            d@EndpointD{ epdRole=Target v, epdAt }
        | Just( inputSt', outputStTail, sch ) <- targetDecision D{ latency=pipeline, tick=nextTick, at=epdAt, var=v, inputSt } 
        = pu
            { puProcess=schedule pu $
                scheduleEndpoint d sch
            , pipeInput=inputSt'
            , pipeOutput=pipeOutput ++ maybeToList outputStTail
            }

    decision _proxy pu@Pipeline{ pipeOutput=po@PipelineOut{ outputSt=DivOut outs } : pos } d@EndpointD{ epdRole=Source vs, epdAt }
        | ([(waitingVs, sel)], os) <- partition ((vs `isSubsetOf`) . fst) outs
        = let
            waitingVs' = waitingVs `difference` vs
            pu' = pu
                { puProcess=schedule pu $
                    scheduleEndpoint d $ do
                        nextTick <- nextScheduledTick
                        scheduleNopAndUpdateTick nextTick (inf epdAt - 1)
                        scheduleInstructionAndUpdateTick (inf epdAt) (sup epdAt) $ Out sel
                , pipeOutput=po
                    { outputSt=DivOut ((waitingVs', sel) : os)
                    } : pos
                }
        in
            finalize pu'
    decision _ _ _ = error "Error in Div decision"


data TargetPipelineDecision i v t
    = D
        { latency :: Int
        , tick :: t
        , inputSt :: i
        , at :: Interval t 
        , var :: v
        }
    deriving ( Show )


targetDecision D{ tick, latency, inputSt, at, var }
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
            scheduleNopAndUpdateTick tick (inf at - 1)
            scheduleInstructionAndUpdateTick (inf at) (sup at) $ Load arg


finalize pu@Pipeline{ pipeOutput=po@PipelineOut{ outputSt=DivOut ((v, _):vs) } : os } | S.null v
    = finalize pu{ pipeOutput=po{ outputSt=DivOut vs }:os }
finalize pu@Pipeline{ pipeOutput=PipelineOut{ outputSt=DivOut [] } : os } = finalize pu{ pipeOutput=os }
finalize pu = pu


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
            [ (wr, B wrSignal)
            , (wrSel, B wrSelSignal)
            , (oe, B oeSignal)
            , (oeSel, B oeSelSignal)
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



-- * internal

data Schedule pu v x t
    = Schedule
        { schProcess :: Process (Parcel v x) t
        , iProxy     :: Proxy (Instruction pu)
        }

schedule pu st
    = schProcess $ execState st Schedule
        { schProcess=process pu
        , iProxy=ip pu
        }
    where
        ip :: pu -> Proxy (Instruction pu)
        ip _ = Proxy

nextScheduledTick :: ( Show t ) => State (Schedule pu v x t) t
nextScheduledTick = do
    Schedule{ schProcess=Process{ nextTick } } <- get
    return nextTick

updateTick tick = do
    sch@Schedule{ schProcess } <- get
    put sch
        { schProcess=schProcess
            { nextTick=tick
            }
        }

scheduleStep placeInTime stepInfo = do
    sch@Schedule{ schProcess=p@Process{ nextUid, steps } } <- get
    put sch
        { schProcess=p
            { nextUid=succ nextUid
            , steps=Step nextUid placeInTime stepInfo : steps
            }
        }

scheduleInstructionAndUpdateTick start finish instr = do
    Schedule{ iProxy } <- get
    scheduleStep (Activity $ start ... finish) $ InstructionStep (instr `asProxyTypeOf` iProxy)
    updateTick $ finish + 1

scheduleNopAndUpdateTick start finish =
    when (start <= finish) $ do
        Schedule{ iProxy } <- get
        scheduleStep (Activity $ start ... finish) $ InstructionStep (nop `asProxyTypeOf` iProxy)
        updateTick $ finish + 1

scheduleEndpoint EndpointD{ epdAt, epdRole } codeGen = do
    scheduleStep (Activity $ inf epdAt ... sup epdAt) $ EndpointRoleStep epdRole
    codeGen
