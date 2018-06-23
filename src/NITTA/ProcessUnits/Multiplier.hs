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
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module NITTA.ProcessUnits.Multiplier
    ( Multiplier(..)
    , PUPorts(..)
    , multiplier
    ) where

import           Data.Default
import           Data.List            (find, (\\))
import           Data.Set             (elems, fromList, member)
import           Data.Typeable
import           NITTA.FunctionBlocks (castFB)
import qualified NITTA.FunctionBlocks as FB
import           NITTA.Types
import           NITTA.Utils
import           NITTA.Utils.Process
import           Numeric.Interval     (inf, sup, (...))


-- | Идентификатор загружаемого аргумента.
data ArgumentSelector
    = A
    | B
    deriving ( Show, Eq )

data Multiplier v x t
    = Multiplier
        { puTarget  :: [(ArgumentSelector, v)]
        , puSource  :: [v]
        , puRemain  :: [FB (Parcel v x)]
        , puProcess :: Process (Parcel v x) t
        , puMocked  :: Bool
        }
    deriving ( Show )

instance ( Default t ) => Default (Multiplier v x t) where
    def = Multiplier [] [] [] def False

multiplier :: ( Default t ) => Multiplier v x t
multiplier = def


instance ( Var v, Time t
         ) => ProcessUnit (Multiplier v x t) (Parcel v x) t where
    bind fb pu@Multiplier{ puRemain }
        | Just FB.Multiply{} <- castFB fb = Right pu{ puRemain=fb : puRemain }
        | otherwise = Left $ "Unknown functional block: " ++ show fb
    process = puProcess
    setTime t pu@Multiplier{ puProcess } = pu{ puProcess=puProcess{ nextTick=t } }

execute pu@Multiplier{ puTarget=[], puSource=[], puRemain } fb
    | Just (FB.Multiply (I a) (I b) (O c)) <- castFB fb = pu{ puTarget=[(A, a), (B, b)], puSource=elems c, puRemain=puRemain \\ [ fb ] }
execute _ _ = error ""


instance ( Var v, Time t
         , Typeable x
         ) => DecisionProblem (EndpointDT v t)
                   EndpointDT (Multiplier v x t)
        where

    options _proxy Multiplier{ puTarget, puProcess=Process{ nextTick } } | not $ null puTarget
        = map (
            \(_, v) -> EndpointO (Target v) $ TimeConstrain (nextTick ... maxBound) (1 ... maxBound)
            ) puTarget
    options _proxy Multiplier{ puSource, puProcess=Process{ nextTick } } | not $ null puSource
        = [ EndpointO (Source $ fromList puSource) $ TimeConstrain (nextTick + 2 ... maxBound) (1 ... maxBound) ]
    options proxy pu@Multiplier{ puRemain } = concatMap (options proxy . execute pu) puRemain

    decision _proxy pu@Multiplier{ puTarget=(sel, v'):xs, puProcess=Process{ nextTick } } d@EndpointD{ epdRole=Target v, epdAt }
        | v == v'
        = pu
            { puProcess=schedule pu $
                scheduleEndpoint d $ do
                    scheduleNopAndUpdateTick nextTick (inf epdAt - 1)
                    scheduleInstructionAndUpdateTick (inf epdAt) (sup epdAt) $ Load sel
            , puTarget=xs
            }
    decision _proxy pu@Multiplier{ puSource, puProcess=Process{ nextTick } } d@EndpointD{ epdRole=Source v, epdAt } 
        | not $ null puSource
        , let puSource' = puSource \\ elems v
        , puSource' /= puSource
        = pu
            { puProcess=schedule pu $
                scheduleEndpoint d $ do
                    scheduleNopAndUpdateTick nextTick (inf epdAt - 1)
                    scheduleInstructionAndUpdateTick (inf epdAt) (sup epdAt) Out
            , puSource=puSource'
            }
    decision proxy pu@Multiplier{ puTarget=[], puSource=[], puRemain } d
        | let v = oneOf $ variables d
        , Just fb <- find (\fb -> v `member` variables fb) puRemain
        = decision proxy (execute pu fb) d
    decision _ _ _ = error ""



instance Controllable (Multiplier v x t) where
    data Microcode (Multiplier v x t)
        = Microcode
            { wrSignal :: Bool
            , selSignal :: Bool
            , oeSignal :: Bool
            } 
        deriving ( Show, Eq, Ord )
    data Instruction (Multiplier v x t)
        = Nop
        | Load ArgumentSelector
        | Out
        deriving (Show)
    nop = Nop


instance UnambiguouslyDecode (Multiplier v x t) where
    decodeInstruction Nop       = Microcode
        { wrSignal=False
        , selSignal=False
        , oeSignal=False
        }
    decodeInstruction (Load A) = (decodeInstruction Nop){ wrSignal=True, selSignal=False }
    decodeInstruction (Load B) = (decodeInstruction Nop){ wrSignal=True, selSignal=True }
    decodeInstruction Out      = (decodeInstruction Nop){ oeSignal=True }



instance Connected (Multiplier v x t) where
    data PUPorts (Multiplier v x t)
        = PUPorts{ wr, wrSel, oe :: Signal } deriving ( Show )
    transmitToLink Microcode{..} PUPorts{..} 
        = 
            [ (wr, Bool wrSignal)
            , (wrSel, Bool selSignal)
            , (oe, Bool oeSignal)
            ]


instance ( Var v
         , Integral x
         ) => Simulatable (Multiplier v x t) v x where
    simulateOn cntx _ fb
        | Just fb'@FB.Multiply{} <- castFB fb = simulate cntx fb'
        | otherwise = error $ "Can't siMultate " ++ show fb ++ " on Shift."



instance ( Time t, Var v
         ) => TargetSystemComponent (Multiplier v x t) where
    moduleName _ _ = "pu_mult"
    software _ _ = Empty
    hardware title pu@Multiplier{ puMocked } 
        = Aggregate Nothing 
            [ if puMocked 
                then FromLibrary "mult/mult_mock.v"
                else FromLibrary "mult/mult_inner.v"
            , FromLibrary $ "mult/" ++ moduleName title pu ++ ".v"
            ]
    hardwareInstance title _ Enviroment{ net=NetEnv{..}, signalClk, signalRst } PUPorts{..} = renderMST
        [ "pu_mult"
        , "  #( .DATA_WIDTH( " ++ show parameterDataWidth ++ " )"
        , "   , .ATTR_WIDTH( " ++ show parameterAttrWidth ++ " )"
        , "   , .INVALID( 0 )" -- FIXME:
        , "   ) $name$"
        , "  ( .clk( " ++ signalClk ++ " )"
        , "  , .rst( " ++ signalRst ++ " )"
        , "  , .signal_wr( " ++ signal wr ++ " )"
        , "  , .signal_sel( " ++ signal wrSel ++ " )"
        , "  , .data_in( " ++ dataIn ++ " )"
        , "  , .attr_in( " ++ attrIn ++ " )"
        , "  , .signal_oe( " ++ signal oe ++ " )"
        , "  , .data_out( " ++ dataOut ++ " )"
        , "  , .attr_out( " ++ attrOut ++ " )"
        , "  );"
        ] [("name", title)]
