{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}

module NITTA.ProcessUnits.Divisor
    where

import           Control.Monad       (void)
import           Control.Monad.State
import           Data.Default
import           Data.Either         (rights)
import           Data.List           (find, minimumBy, partition)
import           Data.Maybe          (fromMaybe, maybeToList)
import           Data.Set            (Set, difference, isSubsetOf, member)
import qualified Data.Set            as S
import           Data.Typeable
import           NITTA.Functions     (castF)
import qualified NITTA.Functions     as F
import           NITTA.Types
import           NITTA.Utils
import           NITTA.Utils.Process
import           Numeric.Interval    (Interval, inf, intersection, singleton,
                                      sup, width, (...))



data InputDesc
    = Numer
    | Denom
    deriving ( Show, Eq )

data OutputDesc
    = Quotient
    | Remain
    deriving ( Show, Eq )



data Divisor v x t
    = Divisor
        { jobs            :: [Job v x t]
        , remains         :: [F (Parcel v x)]
        , inputIntervals  :: [Interval t]
        , outputIntervals :: [Interval t]
        , process'        :: Process (Parcel v x) t
        , pipeline        :: Int
        , mock            :: Bool
        }

divisor pipeline mock = Divisor
    { jobs=[]
    , remains=[]
    , inputIntervals=[]
    , outputIntervals=[]
    , process'=def
    , pipeline
    , mock
    }


data Job v x t
    = Input
        { function :: F (Parcel v x)
        , loadAt   :: t
        , inputSeq :: [v]
        }
    | InProgress
        { function :: F (Parcel v x)
        , startAt  :: t
        , finishAt :: t
        }
    | Output
        { function  :: F (Parcel v x)
        , rottenAt  :: Maybe t
        , outputRnd :: [Set v]
        }


nextInputTick Divisor{ inputIntervals=[] }  = 0
nextInputTick Divisor{ inputIntervals=i:_ } = sup i + 1

nextOutputTick Divisor{ outputIntervals=[] }  = 0
nextOutputTick Divisor{ outputIntervals=i:_ } = sup i + 1


remain2input nextTick f
    | Just (F.Division (I n) (I d) (O _q) (O _r)) <- castF f
    = Input{ function=f, loadAt=nextTick, inputSeq=[n, d] }


input2inProgress = undefined
inProgress2Output = undefined



instance ( Var v, Time t
         ) => ProcessUnit (Divisor v x t) (Parcel v x) t where
    tryBind f pu@Divisor{ remains }
        | Just (F.Division (I _n) (I _d) (O _q) (O _r)) <- castF f
        = Right pu
            { remains=f : remains
            }
        | otherwise = Left $ "Unknown functional block: " ++ show f
    process = process'
    setTime t pu@Divisor{ process' } = pu{ process'=process'{ nextTick=t } }



syncIO opt [] = [ opt ]
syncIO opt@EndpointO{ epoAt=tc@TimeConstrain{ tcAvailable } } intervals
    | all ((0 ==) . width . intersection tcAvailable) intervals
    = [ opt ]
    | otherwise  -- FIXME: get work done
    , let from = maximum $ map sup intervals
    = [ opt{ epoAt=tc{ tcAvailable=from ... inf tcAvailable } } ]


instance ( Var v, Time t
         , Typeable x
         ) => DecisionProblem (EndpointDT v t)
                   EndpointDT (Divisor v x t) where
    options _proxy pu@Divisor{ remains, jobs }
        = targets ++ sources
        where
            target v = EndpointO
                (Target v)
                $ TimeConstrain (nextInputTick pu ... maxBound) (singleton 1)
            targets
                | Just Input{ inputSeq=v:_ }
                        <- find (\case Input{} -> True; _ -> False) jobs
                = [ target v ]
                | otherwise = map (target . head . inputSeq . remain2input nextTick) remains

            source Output{ outputRnd, rottenAt }
                = map
                    ( \vs -> EndpointO
                        (Source vs)
                        $ TimeConstrain
                            (nextOutputTick pu ... fromMaybe maxBound rottenAt)
                            (singleton 1) )
                    outputRnd
            source _ = error "Divider internal error: source."

            sources
                | Just out <- find (\case Output{} -> True; _ -> False) jobs
                = source out
                | let inProgress = filter (\case InProgress{} -> True; _ -> False) jobs
                , not $ null inProgress
                , let out = minimumBy
                        ( \InProgress{ finishAt=a } InProgress{ finishAt=b } ->
                            a `compare` b )
                        inProgress
                = source out
                | otherwise = []

            -- sources Pipeline{ pipeOutput=PipelineOut{ complete, expired, outputSt } : _ }
            --     = let
            --         begin = max complete nextTick
            --         end = fromMaybe maxBound expired
            --         maxDuration = maybe maxBound (+ (-begin)) expired
            --     in
            --         sourceOptions begin end maxDuration outputSt
            -- sources Pipeline{} = []

    -- decision
    --         proxy
    --         pu@Pipeline{ pipeInput=Nothing, puRemain, pipeOutput, pipeline }
    --         d@EndpointD{ epdRole=Target v, epdAt }
    --     | Just fb <- find (member v . inputs) puRemain
    --     , Right (inputSt, expire) <- specificBind fb
    --     = let
    --         pu' = pu
    --             { puRemain=filter (/= fb) puRemain
    --             , pipeInput=Just inputSt
    --             , pipeOutput=case pipeOutput of
    --                 out@PipelineOut{ expired=Nothing } : outs
    --                     -> let out' = out{ expired=Just $ inf epdAt + toEnum (pipeline + expire) }
    --                     in out' : outs
    --                 [] -> []
    --                 _ -> error "wrong internal state of Pipeline."
    --             }
    --     in
    --         decision proxy pu' d

    -- decision
    --         _proxy
    --         pu@Pipeline{ pipeInput=Just inputSt, pipeOutput, pipeline, puProcess=Process{ nextTick } }
    --         d@EndpointD{ epdRole=Target v, epdAt }
    --     | Just( inputSt', outputStTail, sch ) <- targetDecision d D{ latency=pipeline, tick=nextTick, at=epdAt, var=v, inputSt }
    --     = pu
    --         { puProcess=execSchedule pu $
    --             void $ scheduleEndpoint d (sch >> return [])
    --         , pipeInput=inputSt'
    --         , pipeOutput=pipeOutput ++ maybeToList outputStTail
    --         }

    -- decision _proxy pu@Pipeline{ pipeOutput=po@PipelineOut{ outputSt } : pos } d@EndpointD{}
    --     | Just ( outputSt', sch ) <- sourceDecision d outputSt
    --     = pu
    --         { puProcess=execSchedule pu sch
    --         , pipeOutput=case outputSt' of
    --             Just st' -> po{ outputSt=st' } : pos
    --             Nothing  -> pos
    --         }
    -- decision _ _ _ = error "Error in pipeline decision"








-- targetDecision EndpointD{ epdRole=Target var, epdAt=at } D{ tick, latency, inputSt }
--     | Just arg <- argType inputSt var
--     , let ( inputSt', outputStTail ) = pipelineChanges inputSt var
--     = Just ( inputSt', outputStTail, scheduleInput arg )
--     | otherwise = error "Divisor: targetDecision."
--     where
--         argType (DivisorIn (inputs_, _)) v_ = snd <$> find ((==) v_ . fst) inputs_
--         pipelineChanges (DivisorIn (inputs_, (q, r))) v
--             | length inputs_ > 1 =
--                 ( Just $ DivisorIn ( filter ((/=) v . fst) inputs_, (q, r) )
--                 , Nothing
--                 )
--             | otherwise =
--                 ( Nothing
--                 , Just PipelineOut
--                     { complete=tick + toEnum (latency + 3)
--                     , expired=Nothing
--                     , outputSt=DivisorOut [(q, Quotient), (r, Remain)]
--                     }
--                 )
--         scheduleInput arg = do
--             updateTick $ sup at + 1
--             scheduleInstruction (inf at) (sup at) $ Load arg
--             return ()
-- targetDecision _ _ = Nothing



-- sourceDecision d@EndpointD{ epdRole=Source vs, epdAt } (DivisorOut outs)
--     | ([(waitingVs, sel)], os) <- partition ((vs `isSubsetOf`) . fst) outs
--     , let
--         waitingVs' = waitingVs `difference` vs
--         sch = scheduleEndpoint d $ do
--             updateTick $ sup epdAt + 1
--             scheduleInstruction (inf epdAt) (sup epdAt) $ Out sel
--         os' = if S.null waitingVs'
--                 then os
--                 else (waitingVs', sel) : os
--     = Just ( if null os' then Nothing else Just $ DivisorOut os'
--         , void sch
--         )
-- sourceDecision _ _ = Nothing



instance ( Var v
         , Integral x
         ) => Simulatable (Divisor v x t) v x where
    simulateOn cntx _ fb
        | Just fb'@F.Division{} <- castF fb = simulate cntx fb'
        | otherwise = error $ "Can't simulate " ++ show fb ++ " on Shift."




instance Controllable (Divisor v x t) where
    data Instruction (Divisor v x t)
        = Load InputDesc
        | Out OutputDesc
        deriving (Show)

    data Microcode (Divisor v x t)
        = Microcode
            { wrSignal :: Bool
            , wrSelSignal :: Bool
            , oeSignal :: Bool
            , oeSelSignal :: Bool
            } deriving ( Show, Eq, Ord )

instance Default (Microcode (Divisor v x t)) where
    def = Microcode
        { wrSignal=False
        , wrSelSignal=False
        , oeSignal=False
        , oeSelSignal=False
        }
instance UnambiguouslyDecode (Divisor v x t) where
    decodeInstruction (Load Numer)   = def{ wrSignal=True, wrSelSignal=False }
    decodeInstruction (Load Denom)   = def{ wrSignal=True, wrSelSignal=True }
    decodeInstruction (Out Quotient) = def{ oeSignal=True, oeSelSignal=False }
    decodeInstruction (Out Remain)   = def{ oeSignal=True, oeSelSignal=True }


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
    hardware title pu@Divisor{ mock } = Aggregate Nothing
        [ if mock
            then FromLibrary "div/div_mock.v"
            else FromLibrary "div/div.v"
        , FromLibrary $ "div/" ++ moduleName title pu ++ ".v"
        ]
    hardwareInstance title Divisor{ mock, pipeline }
            Enviroment
                { net=NetEnv
                    { signal
                    , parameterDataWidth, dataIn, dataOut
                    , parameterAttrWidth, attrIn, attrOut
                    }
                , signalClk
                , signalRst
                }
            PUPorts{ oe, oeSel, wr, wrSel } = renderMST
        [ "pu_div"
        , "  #( .DATA_WIDTH( " ++ show parameterDataWidth ++ " )"
        , "   , .ATTR_WIDTH( " ++ show parameterAttrWidth ++ " )"
        , "   , .INVALID( 0 )" -- FIXME: Сделать и протестировать работу с атрибутами.
        , "   , .PIPELINE( " ++ show pipeline ++ " )"
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

