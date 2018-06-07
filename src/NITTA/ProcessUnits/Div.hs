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
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module NITTA.ProcessUnits.Div
    ( Div(..)
    , PUPorts(..)
    ) where

import           Control.Monad.State
import           Data.Default
import           Data.List            (find, partition)
import           Data.Maybe           (fromMaybe)
import           Data.Proxy           (asProxyTypeOf)
import           Data.Set             (Set, difference, elems, isSubsetOf,
                                       member)
import qualified Data.Set             as S
import           Data.Typeable
import           NITTA.FunctionBlocks (castFB)
import qualified NITTA.FunctionBlocks as FB
import           NITTA.Types
import           NITTA.Utils
import           Numeric.Interval     (inf, singleton, sup, (...))


data InputDesc
    = Numer
    | Denom
    deriving ( Show, Eq )

data OutputDesc
    = Quotient
    | Remain
    deriving ( Show, Eq )

data PipeOutput v t
    = PipeOutput
        { expired  :: Maybe t
        , complete :: t
        , outs     :: [(Set v, OutputDesc)]
        }
    deriving ( Show )

data Div v x t
    = Div
        { pipeInput      :: Maybe ([(v, InputDesc)], Set v, Set v)
        , pipeOutput     :: [PipeOutput v t]
        , puRemain       :: [FB (Parcel v x)]
        , puProcess      :: Process (Parcel v x) t
        , pipeLine       :: Int
        , frequencyRatio :: Int
        , mock           :: Bool
        }
    deriving ( Show )


instance ( Time t, Var v ) => Default (Div v x t) where
    def = Div def def def def 4 1 False


instance ( Var v, Time t
         ) => ProcessUnit (Div v x t) (Parcel v x) t where
    bind fb pu@Div{ puRemain }
        | Just FB.Div{} <- castFB fb = Right pu{ puRemain=fb : puRemain }
        | otherwise = Left $ "Unknown functional block: " ++ show fb
    process = puProcess
    setTime t pu@Div{ puProcess } = pu{ puProcess=puProcess{ nextTick=t } }

    
instance ( Var v
         , Integral x
         ) => Simulatable (Div v x t) v x where
    simulateOn cntx _ fb
        | Just fb'@FB.Div{} <- castFB fb = simulate cntx fb'
        | otherwise = error $ "Can't simulate " ++ show fb ++ " on Shift."


instance ( Var v, Time t
         , Typeable x
         ) => DecisionProblem (EndpointDT v t)
                   EndpointDT (Div v x t) where
    options _proxy pu@Div{ puRemain, puProcess=Process{ nextTick } }
        = targets pu ++ sources pu
        where
            target v = EndpointO (Target v) $ TimeConstrain (nextTick ... maxBound) (singleton 1)
            targets Div{ pipeInput=Just (vs, _, _) } = map (target . fst) vs
            targets Div{ pipeInput=Nothing } = map target $ concatMap (elems . inputs) puRemain

            sources Div{ pipeOutput=PipeOutput{ outs, expired, complete } : _ }
                = let
                    begin = max complete nextTick
                    expired' = fromMaybe maxBound expired
                    end = maybe maxBound (+ (-begin)) expired
                in
                    [ EndpointO (Source vs) $ TimeConstrain (begin ... expired') (1 ... end)
                    | (vs, _) <- outs
                    ]
            sources Div{} = []

    -- Система может "зависнуть", и если так случится, то как быть?
    decision
            proxy
            pu@Div{ pipeInput=Nothing, puRemain, pipeOutput, pipeLine }
            d@EndpointD{ epdRole=Target v, epdAt }
        | Just fb <- find (member v . inputs) puRemain
        , Just (FB.Div (I n) (I d_) (O q) (O r)) <- castFB fb
        = let
            pu' = pu
                { puRemain=filter (/= fb) puRemain
                , pipeInput=Just
                    ( [(d_, Denom), (n, Numer)]
                    , q
                    , r
                    )
                , pipeOutput=case pipeOutput of
                    [] -> []
                    po@PipeOutput{ expired=Nothing } : pos -> po{ expired=Just $ inf epdAt + toEnum (pipeLine + 2) } : pos
                    _ -> error "wrong internal state of Div."
                }
        in
            decision proxy pu' d

    decision _proxy pu@Div{ pipeInput=Just (vs, q, r), pipeOutput, puProcess=Process{ nextTick }, pipeLine } d@EndpointD{ epdRole=Target v, epdAt }
        | Just (_, argSel) <- find ((==) v . fst) vs
        = let
            pu' = pu
                { puProcess=schedule pu $
                    scheduleEndpoint d $ do
                        scheduleNopAndUpdateTick nextTick (inf epdAt - 1)
                        scheduleInstructionAndUpdateTick (inf epdAt) (sup epdAt) $ Load argSel
                }
        in
            case filter ((/=) v . fst) vs of
                [] -> pu'
                    { pipeInput=Nothing
                    , pipeOutput=pipeOutput ++
                        [ PipeOutput
                            { complete=nextTick + toEnum (pipeLine + 3)
                            , expired=Nothing
                            , outs=[(q, Quotient), (r, Remain)]
                            }
                        ]
                    }
                vs' -> pu'{ pipeInput=Just (vs', q, r) }

    decision _proxy pu@Div{ pipeOutput=po@PipeOutput{ outs } : pos } d@EndpointD{ epdRole=Source vs, epdAt }
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
                    { outs=(waitingVs', sel) : os
                    } : pos
                }
        in
            finalize pu'
    decision _ _ _ = error "Error in Div decision"


finalize pu@Div{ pipeOutput=po@PipeOutput{ outs=(v, _):vs } : os } | S.null v
    = finalize pu{ pipeOutput=po{ outs=vs }:os }
finalize pu@Div{ pipeOutput=PipeOutput{ outs=[] } : os } = finalize pu{ pipeOutput=os }
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
    hardware title pu@Div{ mock } = Aggregate Nothing
        [ if mock
            then FromLibrary "div/div_mock.v"
            else FromLibrary "div/div.v"
        , FromLibrary $ "div/" ++ moduleName title pu ++ ".v"
        ]
    hardwareInstance title Div{ pipeLine, mock } Enviroment{ net=NetEnv{..}, signalClk, signalRst } PUPorts{..} = renderMST
        [ "pu_div"
        , "  #( .DATA_WIDTH( " ++ show parameterDataWidth ++ " )"
        , "   , .ATTR_WIDTH( " ++ show parameterAttrWidth ++ " )"
        , "   , .INVALID( 0 )" -- FIXME:
        , "   , .PIPELINE( " ++ show pipeLine ++ " )" -- FIXME:
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
