{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}

module NITTA.ProcessUnits.Divider
    ( divider
    , PUPorts(..)
    ) where

import           Control.Monad                 (void, when)
import           Data.Default
import           Data.List                     (partition, sortBy)
import           Data.Maybe                    (fromMaybe)
import           Data.Set                      (Set, member)
import qualified Data.Set                      as S
import           Data.Typeable
import           NITTA.Functions               (castF)
import qualified NITTA.Functions               as F
import           NITTA.Project
import           NITTA.Types
import           NITTA.Types.Project
import           NITTA.Utils
import           NITTA.Utils.Process
import           Numeric.Interval              (Interval, inf, intersection,
                                                singleton, sup, width, (...))
import           Text.InterpolatedString.Perl6 (qc)


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
        , remains         :: [F v x]
        , targetIntervals :: [Interval t]
        , sourceIntervals :: [Interval t]
        , process_        :: Process v x t
        , latency         :: t
        , pipeline        :: t
        , mock            :: Bool
        }

divider pipeline mock = Divider
    { jobs=[]
    , remains=[]
    , targetIntervals=[]
    , sourceIntervals=[]
    , process_=def
    , latency=1
    , pipeline
    , mock
    }

instance ( Time t ) => Default (Divider v x t) where
    def = divider 4 True

instance WithX (Divider v x t) x

instance ( Ord t ) => WithFunctions (Divider v x t) (F v x) where
    functions Divider{ process_, remains, jobs }
        = functions process_
        ++ remains
        ++ map function jobs

data Job v x t
    = Input
        { function :: F v x
        , startAt  :: t
        , inputSeq :: [(InputDesc, v)]
        }
    | InProgress
        { function :: F v x
        , startAt  :: t
        , finishAt :: t
        }
    | Output
        { function  :: F v x
        , startAt   :: t
        , rottenAt  :: Maybe t
        , finishAt  :: t
        , outputRnd :: [(OutputDesc, Set v)]
        }
    deriving ( Eq, Show )


nextTargetTick Divider{ targetIntervals=[] }  = 0
nextTargetTick Divider{ targetIntervals=i:_ } = sup i + 1

nextSourceTick Divider{ sourceIntervals=[] }  = 0
nextSourceTick Divider{ sourceIntervals=i:_ } = sup i + 1



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



remain2input nextTick f
    | Just (F.Division (I n) (I d) (O _q) (O _r)) <- castF f
    = Input{ function=f, startAt=nextTick, inputSeq=[(Numer, n), (Denom, d)] }
remain2input _ _ = error "divider inProgress2Output internal error"

inProgress2Output rottenAt InProgress{ function, startAt, finishAt }
    | Just (F.Division _ _ (O q) (O r)) <- castF function
    = Output{ function, rottenAt, startAt, finishAt, outputRnd=filter (not . null . snd) [(Quotient, q), (Remain, r)] }
inProgress2Output _ _ = error "divider inProgress2Output internal error"


resolveColisions [] opt = [ opt ]
resolveColisions intervals opt@EndpointO{ epoAt=tc@TimeConstrain{ tcAvailable } }
    | all ((0 ==) . width . intersection tcAvailable) intervals
    = [ opt ]
    | otherwise  -- FIXME: we must prick out work point from intervals
    , let from = maximum $ map sup intervals
    = [ opt{ epoAt=tc{ tcAvailable=from ... inf tcAvailable } } ]


rottenTime Divider{ pipeline, latency } jobs
    | Just (InProgress{ startAt }, _) <- findNextInProgress jobs
    = Just (startAt + pipeline + latency )
    | Just (Input{ startAt }, _) <- findOutput jobs
    = Just (startAt + pipeline + latency )
    | otherwise = Nothing


pushOutput pu@Divider{ jobs }
    | Just _ <- findOutput jobs = pu
    | Just (ij, other) <- findNextInProgress jobs
    = pu{ jobs=inProgress2Output (rottenTime pu other) ij : other }
    | otherwise = pu



instance ( Var v, Time t, Typeable x
         ) => ProcessUnit (Divider v x t) v x t where
    tryBind f pu@Divider{ remains }
        | Just (F.Division (I _n) (I _d) (O _q) (O _r)) <- castF f
        = Right pu
            { remains=f : remains
            }
        | otherwise = Left $ "Unknown functional block: " ++ show f
    process = process_
    setTime t pu@Divider{ process_ } = pu{ process_=process_{ nextTick=t } }


instance Locks (Divider v x t) v where
    -- FIXME:
    locks _ = []


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
                | Just (Input{ inputSeq=(_tag, v):_ }, _) <- findInput jobs
                = [ target v ]
                | otherwise = map (target . snd . head . inputSeq . remain2input nextTick) remains

            source Output{ outputRnd, rottenAt, finishAt }
                = map
                    ( \(_tag, vs) -> EndpointO
                        (Source vs)
                        $ TimeConstrain
                            (max finishAt (nextSourceTick pu) ... fromMaybe maxBound rottenAt)
                            (singleton 1) )
                    outputRnd
            source _ = error "Divider internal error: source."

            sources
                | Just (out, _) <- findOutput jobs = source out
                | Just (ij, other) <- findNextInProgress jobs
                 = source $ inProgress2Output (rottenTime pu other) ij
                | otherwise = []


    -- FIXME: vertical relations
    decision
            proxy
            pu@Divider{ jobs, targetIntervals, remains, pipeline, latency }
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
        , let finishAt = sup epdAt + pipeline + latency
        = pushOutput pu
            { targetIntervals=epdAt : targetIntervals
            , jobs=if null vs
                then InProgress{ function, startAt, finishAt } : other
                else i{ inputSeq=vs } : other
            , process_=execSchedule pu $ do
                _endpoints <- scheduleEndpoint d $ scheduleInstruction (inf epdAt) (sup epdAt) $ Load tag
                -- костыль, необходимый для корректной работы автоматически сгенерированных тестов,
                -- которые берут информацию о времени из Process
                updateTick (sup epdAt)
            }

    decision
            _proxy
            pu@Divider{ jobs, sourceIntervals }
            d@EndpointD{ epdRole=Source vs, epdAt }
        | Just (out@Output{ outputRnd, startAt, function }, other) <- findOutput jobs
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
                _endpoints <- scheduleEndpoint d $ scheduleInstruction (inf epdAt) (sup epdAt) $ Out tag
                when (null vss') $ void $ scheduleFunction startAt (sup epdAt) function
                -- костыль, необходимый для корректной работы автоматически сгенерированных тестов,
                -- которые берут информацию о времени из Process
                updateTick (sup epdAt)
            }

    decision _ _ _ = error "divider decision internal error"



instance ( Var v
         , Integral x
         , Typeable x
         ) => Simulatable (Divider v x t) v x where
    simulateOn cntx _ f
        | Just f'@F.Division{} <- castF f = simulate cntx f'
        | otherwise = error $ "Can't simulate " ++ show f ++ " on Shift."



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


instance ( Val x, Show t
         ) => TargetSystemComponent (Divider v x t) where
    moduleName _ _ = "pu_div"
    software _ _ = Empty
    hardware title pu@Divider{ mock } = Aggregate Nothing
        [ if mock
            then FromLibrary "div/div_mock.v"
            else FromLibrary "div/div.v"
        , FromLibrary $ "div/" ++ moduleName title pu ++ ".v"
        ]
    hardwareInstance title pu@Divider{ mock, pipeline }
            Enviroment
                { net=NetEnv
                    { signal
                    , dataIn, dataOut
                    , parameterAttrWidth, attrIn, attrOut
                    }
                , signalClk
                , signalRst
                }
            PUPorts{ oe, oeSel, wr, wrSel } =
        [qc|pu_div
    #( .DATA_WIDTH( { widthX pu } )
     , .ATTR_WIDTH( { parameterAttrWidth } )
     , .INVALID( 0 ) // FIXME: Сделать и протестировать работу с атрибутами
     , .PIPELINE( { pipeline } )
     , .SCALING_FACTOR_POWER( { scalingFactorPowerOfProxy $ proxyX pu } )
     , .MOCK_DIV( { bool2verilog mock } )
     ) { title }
    ( .clk( { signalClk } )
    , .rst( { signalRst } )
    , .signal_wr( { signal wr } )
    , .signal_wr_sel( { signal wrSel } )
    , .data_in( { dataIn } )
    , .attr_in( { attrIn } )
    , .signal_oe( { signal oe } )
    , .signal_oe_sel( { signal oeSel } )
    , .data_out( { dataOut } )
    , .attr_out( { attrOut } )
    );|]

instance IOTest (Divider v x t) v x


instance ( Var v, Time t
         , Typeable x, Show x, Integral x, Val x
         ) => TestBench (Divider v x t) v x where
    testBenchDescription prj@Project{ projectName, processorModel }
        = Immidiate (moduleName projectName processorModel ++ "_tb.v")
            $ snippetTestBench prj TestBenchSetup
                { tbcSignals=["oe", "oeSel", "wr", "wrSel"]
                , tbcPorts=PUPorts
                    { oe=Signal 0
                    , oeSel=Signal 1
                    , wr=Signal 2
                    , wrSel=Signal 3
                    }
                , tbcSignalConnect= \case
                    (Signal 0) -> "oe"
                    (Signal 1) -> "oeSel"
                    (Signal 2) -> "wr"
                    (Signal 3) -> "wrSel"
                    _ -> error "testBenchDescription wrong signal"
                , tbcCtrl= \Microcode{ oeSignal, oeSelSignal, wrSignal, wrSelSignal } ->
                    [qc|oe <= {bool2verilog oeSignal}; oeSel <= {bool2verilog oeSelSignal}; wr <= {bool2verilog wrSignal}; wrSel <= {bool2verilog wrSelSignal};|]
                }
