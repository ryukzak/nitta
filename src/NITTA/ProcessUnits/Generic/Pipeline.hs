{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-type-defaults #-}

module NITTA.ProcessUnits.Generic.Pipeline where

import           Control.Monad.State
import           Data.Either         (rights)
import           Data.List           (find)
import           Data.Maybe          (fromMaybe, maybeToList)
import           Data.Set            (member)
import           Data.Typeable
import           NITTA.Types
import           NITTA.Utils.Process
import           Numeric.Interval    (Interval, inf)


-- TODO: В настоящий момент был найден баг в моделировании времени. А именно: для pipeline есть два
-- параллельно существущих времени, время загрузки и время выгрузки значения. Причём оба должны
-- продвигаться вперёд независимо, но без и без коллизий.
--
-- Пахнет большой и лютой переделкой, так как требует изменения Process. Хранителем времени должен стать сам PU.


data PipelineOut st t
    = PipelineOut
        { expired  :: Maybe t
        , complete :: t
        , outputSt :: OutputSt st
        }

data Pipeline st v x t
    = Pipeline
        { pipeInput      :: Maybe (InputSt st)
        , pipeOutput     :: [PipelineOut st t]
        , puRemain       :: [FB (Parcel v x)]
        , puProcess      :: Process (Parcel v x) t
        , pipeline       :: Int
        , frequencyRatio :: Int
        , state          :: st
        }


class PipelineTF st where
    data InputSt st :: *
    data OutputSt st :: *


class ( PipelineTF st ) => PipelinePU st io where
    bindPipeline :: FB io -> Either String ( InputSt st, Int )

data TargetPipelineDecision st v t
    = D
        { latency :: Int
        , tick    :: t
        , inputSt :: InputSt st
        , at      :: Interval t
        , var     :: v
        }

class ( PipelineTF st ) => PipelinePU2 st v t where
    targetOptions :: t -> InputSt st -> [ Option (EndpointDT v t) ]
    targetDecision :: ( Typeable x )
        => Decision (EndpointDT v t) -> TargetPipelineDecision st v t
        -> Maybe ( Maybe (InputSt st), Maybe (PipelineOut st t), State (Schedule (Pipeline st v x t) v x t) () )

    sourceOptions :: t -> t -> t -> OutputSt st -> [ Option (EndpointDT v t) ]
    sourceDecision :: ( Typeable x )
        => Decision (EndpointDT v t) -> OutputSt st
        -> Maybe ( Maybe (OutputSt st)
                 , State (Schedule (Pipeline st v x t) v x t) () )



instance ( Var v, Time t, PipelinePU st (Parcel v x)
         ) => ProcessUnit (Pipeline st v x t) (Parcel v x) t where
    bind fb pu@Pipeline{ puRemain }
        = case bindPipeline fb of
            Right ( _ :: InputSt st, _ ) -> Right pu{ puRemain=fb : puRemain }
            Left err -> Left $ "Unknown functional block: " ++ err
    process = puProcess
    setTime t pu@Pipeline{ puProcess } = pu{ puProcess=puProcess{ nextTick=t } }



instance ( Var v, Time t
         , Typeable x
         , PipelinePU st (Parcel v x)
         , PipelinePU2 st v t
         ) => DecisionProblem (EndpointDT v t)
                   EndpointDT (Pipeline st v x t) where
    options _proxy pu@Pipeline{ puRemain, puProcess=Process{ nextTick } }
        = targets pu ++ sources pu
        where
            targets Pipeline{ pipeInput=Just pipelineIn } = targetOptions nextTick pipelineIn
            targets Pipeline{ pipeInput=Nothing }
                = concatMap (targetOptions nextTick) (map fst $ rights $ map bindPipeline puRemain :: [InputSt st])

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
        | Just( inputSt', outputStTail, sch ) <- targetDecision d D{ latency=pipeline, tick=nextTick, at=epdAt, var=v, inputSt }
        = pu
            { puProcess=schedule pu $
                scheduleEndpoint d sch
            , pipeInput=inputSt'
            , pipeOutput=pipeOutput ++ maybeToList outputStTail
            }

    decision _proxy pu@Pipeline{ pipeOutput=po@PipelineOut{ outputSt } : pos } d@EndpointD{}
        | Just ( outputSt', sch ) <- sourceDecision d outputSt
        = pu
            { puProcess=schedule pu sch
            , pipeOutput=case outputSt' of
                Just st' -> po{ outputSt=st' } : pos
                Nothing  -> pos
            }
    decision _ _ _ = error "Error in pipeline decision"

