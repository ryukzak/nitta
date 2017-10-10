{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}


module Mux where


import           Data.Array
import           Data.Bits
import           Data.Default
import           Data.Either
import           Data.Functor.Const
import           Data.Generics.Aliases (orElse)
import qualified Data.Graph            as G
import           Data.List             (find, minimumBy, sortBy)
import qualified Data.Map              as M
import           Data.Maybe
import           Data.Typeable
import           NITTA.FunctionBlocks
import           NITTA.TestBench
import           NITTA.Types
import           NITTA.Utils
import           Prelude               hiding (last)


data MuxPu ty v t = MuxPu
  { mCurrent :: Maybe (MicroCode v t)
  , mRemains :: [MicroCode v t]
  , mProcess :: Process v t
  } deriving ( Show )

data MicroCode v t = MicroCode
  { actions      :: [Effect v]
  , fb           :: FB v
  , workBegin    :: Maybe t
  , processParts :: [ProcessUid]
  } deriving ( Show )

instance ( Var v ) => Eq (MicroCode v t) where
  MicroCode{ fb=a } == MicroCode{ fb=b } = a == b

instance ( Var v, Time t ) => Default (MuxPu ty v t) where
  def = MuxPu{ mCurrent=Nothing
             , mRemains=[]
             , mProcess=def
             }

muxSize = 16 :: Int

instance ( Var v, Time t ) => PUClass MuxPu Passive v t where

  data Signal MuxPu
    = LOAD
    | SEL
    | MODE Int
    | OE

  data Instruction MuxPu v
    = Nop
    | Select
    | Input
    | Output
    deriving (Show)

  signal' MuxPu{..} sig time = undefined

  bind fb m@MuxPu{ mProcess=p@Process{..}, .. }
    | Just Mux{..} <- unbox fb
    = let (k, p') = modifyProcess p $ bindFB fb tick
      in Right m{ mRemains=MicroCode ( Push mSelect : [Push i | i <- mInputs] ++ [Pull mOutput]
                                     ) fb Nothing [k] : mRemains
                , mProcess=p'
                }

    | otherwise
    = Left $ "Unknown functional block: " ++ show fb

  options m@MuxPu{..}
    | isNothing mCurrent
    = map (\MicroCode{ actions=e : _ } -> EffectOpt e tc) mRemains

    | Just MicroCode{ actions=e : _ } <- mCurrent
    = [ EffectOpt e tc ]

    | otherwise
    = error $ "Wrong MuxPu state!" ++ show m

    where
      tc = TimeConstrain 1 0 maxBound

  step m@MuxPu{ mProcess=p0@Process{ tick=tick0 }, ..} act0@EffectAct{ eaAt=at@Event{..}, .. }
    | tick0 > eStart
    = error "You can't start work yesterday:)"

    | Just mc@MicroCode{ actions=x : _, ..} <- mCurrent, x << eaEffect
    = let (p', mc') = doAction p0 mc (const Input)
      in m{ mProcess=p', mCurrent=mc' }

    | Just mc@MicroCode{..} <- find ((<< eaEffect) . head . actions) mRemains
    = let (p', mc') = doAction p0 mc (const Select)
          m' = m{ mCurrent=mc'
                , mProcess=p'
                , mRemains=filter (/= mc) mRemains
                }
      in m'

    | otherwise
    = error $ "Can't found selected action: " ++ show act0

    where
      doAction p mc@MicroCode{..} instr =
        let (p', mc'@MicroCode{ actions=acts' }) = mkWork p mc instr
        in if null acts'
           then (finish p' mc', Nothing)
           else (p', Just mc')

      mkWork _p MicroCode{ actions=[] } _ = error "Mux internal error, mkWork"
      mkWork p mc@MicroCode{ actions=x:xs, ..} instr =
        let ((e, ss), p') = modifyProcess p $ do
              e <- add at eaEffect
              s1 <- add at (instr x)
              ss <- if tick0 < eStart
                then do
                  s2 <- add (Event tick0 (eStart - tick0)) Nop
                  return [ s1, s2 ]
                else return [ s1 ]
              mapM_ (relation . Vertical e) ss
              setTime (eStart + eDuration)
              return (e, ss)
        in (p', mc{ processParts=e : ss ++ processParts
                  , workBegin=workBegin `orElse` Just eStart
                  , actions=if x == eaEffect then xs else (x \\\ eaEffect) : xs
                  })

      finish p MicroCode{ workBegin=Just start, .. } = snd $ modifyProcess p $ do
        let duration = (eStart + eDuration) - start
        h <- add (Event start duration) fb
        mapM_ (relation . Vertical h) processParts

  process = mProcess

  varValue pu cntx vi@(v, _)
    | otherwise = error $ "can't find varValue for: " ++ show v

  variableValue (FB fb) MuxPu{..} cntx (v, i)
    | otherwise = error $ "Can't simulate " ++ show fb


-- act2Signal

