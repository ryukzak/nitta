{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}

module NITTA.ProcessUnits.Divider
    where

import           Control.Monad       (void)
import           Control.Monad.State
import           Data.Default
import           Data.Either         (rights)
import           Data.List           (find, minimumBy, partition, sortBy)
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



data Divider v x t
    = Divider
        { jobs            :: [Job v x t]
        , remains         :: [F (Parcel v x)]
        , targetIntervals :: [Interval t]
        , sourceIntervals :: [Interval t]
        , process_        :: Process (Parcel v x) t
        , pipeline        :: Int
        , mock            :: Bool
        }

divisor pipeline mock = Divider
    { jobs=[]
    , remains=[]
    , targetIntervals=[]
    , sourceIntervals=[]
    , process_=def
    , pipeline
    , mock
    }


data Job v x t
    = Input
        { function :: F (Parcel v x)
        , startAt  :: t
        , inputSeq :: [(InputDesc, v)]
        }
    | InProgress
        { function :: F (Parcel v x)
        , startAt  :: t
        , finishAt :: t
        }
    | Output
        { function  :: F (Parcel v x)
        , rottenAt  :: Maybe t
        , outputRnd :: [(OutputDesc, Set v)]
        }
    deriving ( Eq )


nextTargetTick Divider{ targetIntervals=[] }  = 0
nextTargetTick Divider{ targetIntervals=i:_ } = sup i + 1

nextSourceTick Divider{ sourceIntervals=[] }  = 0
nextSourceTick Divider{ sourceIntervals=i:_ } = sup i + 1


remain2input nextTick f
    | Just (F.Division (I n) (I d) (O _q) (O _r)) <- castF f
    = Input{ function=f, startAt=nextTick, inputSeq=[(Numer, n), (Denom, d)] }


inProgress2Output rottenAt InProgress{ function, startAt, finishAt }
    | Just (F.Division _ _ (O q) (O r)) <- castF function
    = Output{ function, rottenAt, outputRnd=[(Quotient, q), (Remain, r)] }

instance ( Var v, Time t
         ) => ProcessUnit (Divider v x t) (Parcel v x) t where
    tryBind f pu@Divider{ remains }
        | Just (F.Division (I _n) (I _d) (O _q) (O _r)) <- castF f
        = Right pu
            { remains=f : remains
            }
        | otherwise = Left $ "Unknown functional block: " ++ show f
    process = process_
    setTime t pu@Divider{ process_ } = pu{ process_=process_{ nextTick=t } }



resolveColisions [] opt = [ opt ]
resolveColisions intervals opt@EndpointO{ epoAt=tc@TimeConstrain{ tcAvailable } }
    | all ((0 ==) . width . intersection tcAvailable) intervals
    = [ opt ]
    | otherwise  -- FIXME: we must prick out work point from intervals
    , let from = maximum $ map sup intervals
    = [ opt{ epoAt=tc{ tcAvailable=from ... inf tcAvailable } } ]



findJob f jobs
    = case partition f jobs of
        ([i], other) -> Just (i, other)
        ([], _)      -> Nothing
        _            -> error "findInput internal error"

findInput = findJob (\case Input{} -> True; _ -> False)
findOutput = findJob (\case Output{} -> True; _ -> False)

findNextInProgress jobs
    | let (inProgress, other) = partition (\case InProgress{} -> True; _ -> False) jobs
    , let inProgress' = sortBy
            ( \InProgress{ finishAt=a } InProgress{ finishAt=b } ->
                b `compare` a )
            inProgress
    = case inProgress' of
        []     -> Nothing
        (j:js) -> Just (j, js ++ other)



rottenTime Divider{ jobs, pipeline }
    | Just (InProgress{ startAt }, _) <- findNextInProgress jobs
    = Just (startAt + toEnum pipeline + 10 )
    | Just (Input{ startAt }, _) <- findOutput jobs
    = Just (startAt + toEnum pipeline + 10 )
    | otherwise = Nothing



instance ( Var v, Time t
         , Typeable x
         ) => DecisionProblem (EndpointDT v t)
                   EndpointDT (Divider v x t) where
    options _proxy pu@Divider{ targetIntervals, sourceIntervals, remains, jobs }
        = concatMap (resolveColisions sourceIntervals) targets
        ++ concatMap (resolveColisions targetIntervals) sources
        where
            target v = EndpointO
                (Target v)
                $ TimeConstrain (nextTargetTick pu ... maxBound) (singleton 1)
            targets
                | Just (Input{ inputSeq=(tag, v):_ }, _) <- findInput jobs
                = [ target v ]
                | otherwise = map (target . snd . head . inputSeq . remain2input nextTick) remains

            source Output{ outputRnd, rottenAt }
                = map
                    ( \(_tag, vs) -> EndpointO
                        (Source vs)
                        $ TimeConstrain
                            (nextSourceTick pu ... fromMaybe maxBound rottenAt)
                            (singleton 1) )
                    outputRnd
            source _ = error "Divider internal error: source."

            sources
                | Just (out, _) <- findOutput jobs = source out
                | Just (ij, _) <- findNextInProgress jobs
                 = source $ inProgress2Output (rottenTime pu) ij
                | otherwise = []

    decision
            proxy
            pu@Divider{ jobs, targetIntervals, pipeline, remains }
            d@EndpointD{ epdRole=Target v, epdAt }
        | ([f], fs) <- partition (\f -> v `member` variables f) remains
        = decision
            proxy
            pu
                { remains=fs
                , jobs=remain2input (sup epdAt) f : jobs
                }
            d

        | Just (i@Input{ inputSeq=((tag, nextV):vs), function, startAt }, other) <- findInput jobs
        , v == nextV
        , let finishAt = sup epdAt + toEnum pipeline + 10
        = pushOutput pu
            { targetIntervals=epdAt : targetIntervals
            , jobs=if null vs
                then InProgress{ function, startAt, finishAt } : other
                else i{ inputSeq=vs } : other
            , process_=execSchedule pu $ do
                endpoints <- scheduleEndpoint d $ scheduleInstruction (inf epdAt) (sup epdAt) $ Load tag
                -- костыль, необходимый для корректной работы автоматически сгенерированных тестов,
                -- которые берут информацию о времени из Process
                updateTick (sup epdAt)
            }

    decision
            _proxy
            pu@Divider{ jobs, sourceIntervals, pipeline }
            d@EndpointD{ epdRole=Source vs, epdAt }
        | Just (out@Output{ outputRnd }, other) <- findOutput jobs
        , (vss, [(tag, vs')]) <- partition (\(_tag, vs') -> null (vs `S.intersection` vs')) outputRnd
        , let vss' = let tmp = vs' `S.difference` vs
                in if S.null tmp
                    then vss
                    else (tag, tmp) : vss
        = pushOutput pu
            { sourceIntervals=epdAt : sourceIntervals
            , jobs=if null vss'
                then other
                else out{ outputRnd=vss' } : other
            , process_=execSchedule pu $ do
                endpoints <- scheduleEndpoint d $ scheduleInstruction (inf epdAt) (sup epdAt) $ Out tag
                -- костыль, необходимый для корректной работы автоматически сгенерированных тестов,
                -- которые берут информацию о времени из Process
                updateTick (sup epdAt)
            }


pushOutput pu@Divider{ jobs }
    | Just _ <- findOutput jobs = pu
    | Just (ij, other) <- findNextInProgress jobs
    = pu{ jobs=inProgress2Output (rottenTime pu) ij : other }
    | otherwise = pu



instance ( Var v
         , Integral x
         ) => Simulatable (Divider v x t) v x where
    simulateOn cntx _ fb
        | Just fb'@F.Division{} <- castF fb = simulate cntx fb'
        | otherwise = error $ "Can't simulate " ++ show fb ++ " on Shift."



instance Controllable (Divider v x t) where
    data Instruction (Divider v x t)
        = Load InputDesc
        | Out OutputDesc
        deriving (Show)

    data Microcode (Divider v x t)
        = Microcode
            { wrSignal :: Bool
            , wrSelSignal :: Bool
            , oeSignal :: Bool
            , oeSelSignal :: Bool
            } deriving ( Show, Eq, Ord )

instance Default (Microcode (Divider v x t)) where
    def = Microcode
        { wrSignal=False
        , wrSelSignal=False
        , oeSignal=False
        , oeSelSignal=False
        }
instance UnambiguouslyDecode (Divider v x t) where
    decodeInstruction (Load Numer)   = def{ wrSignal=True, wrSelSignal=False }
    decodeInstruction (Load Denom)   = def{ wrSignal=True, wrSelSignal=True }
    decodeInstruction (Out Quotient) = def{ oeSignal=True, oeSelSignal=False }
    decodeInstruction (Out Remain)   = def{ oeSignal=True, oeSelSignal=True }


instance Connected (Divider v x t) where
    data PUPorts (Divider v x t)
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
         ) => TargetSystemComponent (Divider v x t) where
    moduleName _ _ = "pu_div"
    software _ _ = Empty
    hardware title pu@Divider{ mock } = Aggregate Nothing
        [ if mock
            then FromLibrary "div/div_mock.v"
            else FromLibrary "div/div.v"
        , FromLibrary $ "div/" ++ moduleName title pu ++ ".v"
        ]
    hardwareInstance title Divider{ mock, pipeline }
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

