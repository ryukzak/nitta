{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module NITTA.BusNetwork where

import           Control.Monad.State
import           Data.Array
import           Data.Default
import           Data.Either
import           Data.List               (intersect, nub, sortBy, (\\))
import qualified Data.Map                as M
import           Data.Maybe              (catMaybes, fromMaybe, isJust)
import           Data.Typeable
import           NITTA.ProcessUnits.Fram
import           NITTA.TestBench
import           NITTA.Types
import           NITTA.Utils

import           Debug.Trace

class ( Typeable v, Eq v, Ord v, Show v ) => Title v
instance ( Typeable v, Eq v, Ord v, Show v ) => Title v



data BusNetwork title spu v t =
  BusNetwork
    { bnRemains            :: [FB Parcel v]
    , bnForwardedVariables :: [v]
    , bnBinded             :: M.Map title [FB Parcel v]
    , bnPus                :: M.Map title spu
    , bnProcess            :: Process v t
    , bnWires              :: Array Int [(title, S)]
    }

busNetwork pus wires = BusNetwork [] [] (M.fromList []) (M.fromList pus) def wires



instance ( Title title, Var v, Time t
         , PUClass Passive (PU Passive v t) v t
         ) => PUClass (Network title) (BusNetwork title (PU Passive v t) v t) v t where

  bind fb bn@BusNetwork{..}
    | any (\pu -> isRight $ bind fb pu) $ M.elems bnPus
    = Right bn{ bnRemains=fb : bnRemains }
  bind _fb _bn = Left "no"

  options BusNetwork{..} =
    let x = concat
          [
            [ TransportOpt fromPu pullAt $ M.fromList pushs
            | pushs <- sequence $ map pushOptionsFor pullVars
            , let pushTo = catMaybes $ map (fmap fst . snd) $ trace ("pushs: " ++ show pushs) pushs
            , length (nub pushTo) == length pushTo
            ]
          | (fromPu, opts) <- puOptions
          , EffectOpt (Pull pullVars) pullAt <-
            trace ("puOptions: " ++ concatMap ((++"\n") . show) puOptions) opts
          ]
    in trace ("BusNetwork options: " ++ show x ++ "\n" ++ "availableVars: " ++ show availableVars) x
    where
      pushOptionsFor v | v `notElem` availableVars = [(v, Nothing)]
      pushOptionsFor v = (v, Nothing) : pushOptionsFor' v

      pushOptionsFor' v = [ (v, Just (pushTo, pushAt))
                          | (pushTo, vars) <- puOptions
                          , EffectOpt (Push pushVar) pushAt <- vars
                          , pushVar == v
                          ]
      availableVars =
        let fbs = bnRemains ++ (concat $ M.elems bnBinded)
            alg = foldl
                  (\dict (a, b) -> M.adjust ((:) b) a dict)
                  (M.fromList [(v, []) | v <- concatMap variables fbs])
                  $ filter (\(_a, b) -> b `notElem` bnForwardedVariables)
                  $ concatMap dependency fbs
            notBlockedVariables = map fst $ filter (null . snd) $ M.assocs alg
        in notBlockedVariables \\ bnForwardedVariables

      puOptions = M.assocs $ M.map options bnPus

  select ni@BusNetwork{..} act@TransportAct{..} = ni
    { bnPus=foldl (\s n -> n s) bnPus steps
    , bnProcess=snd $ modifyProcess bnProcess $ do
        mapM_ (\(v, (title, _)) -> add
                (Event transportStartAt transportDuration)
                (InstructionStep
                  $ (Transport v taPullFrom title :: Instruction (BusNetwork title (PU Passive v t) v t)))
              ) $ M.assocs push'
        _ <- add (Event transportStartAt transportDuration) $ InfoStep $ show act -- $ Pull pullVars
        setProcessTime $ transportStartAt + transportDuration
    , bnForwardedVariables=pullVars ++ bnForwardedVariables
    }
    where
      transportStartAt = eStart taPullAt
      transportDuration = maximum $
        map ((\Event{..} -> (eStart - transportStartAt) + eDuration) . snd) $ M.elems push'
      -- if puTitle not exist - skip it...
      pullStep = M.adjust (\dpu -> select dpu $ EffectAct (Pull pullVars) taPullAt) taPullFrom
      pushStep (var, (dpuTitle, pushAt)) =
        M.adjust (\dpu -> select dpu $ EffectAct (Push var) pushAt) dpuTitle
      pushSteps = map pushStep $ M.assocs push'
      steps = pullStep : pushSteps

      push' = M.map (fromMaybe undefined) $ M.filter isJust taPush
      pullVars = M.keys push'


  process pu@BusNetwork{..} = let
    transportKey = M.fromList
      [ (v, sKey st)
      | st <- steps bnProcess
      , let instr = getInstruction (proxy pu) st
      , isJust instr
      , let (Just (Transport v _ _)) = instr
      ]
    p'@Process{ steps=steps' } = snd $ modifyProcess bnProcess $ do
      let pus = sortBy (\a b -> fst a `compare` fst b) $ M.assocs bnPus
      mapM (addSubProcess transportKey) pus

    in p'{ steps=reverse steps' }
    where
      addSubProcess transportKey (puTitle, pu') = do
        let subSteps = steps $ process pu'
        uids' <- foldM (\dict Step{..} -> do
                           k <- add sTime $ NestedStep puTitle sDesc
                           when (isFB sDesc) $ do
                             let FBStep fb = sDesc
                             mapM_ (\v -> when (v `M.member` transportKey)
                                          $ relation $ Vertical (transportKey M.! v) k
                                   ) $ variables fb
                           return $ M.insert sKey k dict
                       ) M.empty subSteps
        let subRelations = relations $ process pu'
        mapM (\r -> relation $ case r of
                 Vertical a b -> Vertical (uids' M.! a) (uids' M.! b)
             ) subRelations

  setTime t bn@BusNetwork{..} = bn{ bnProcess=bnProcess{ tick=t }
                                  , bnPus=M.map (setTime t) bnPus
                                  }




-- Сигналы - есть у всех управляемых ПУ.




instance ( Title title, Var v, Time t
         ) => Controllable (BusNetwork title (PU Passive v t) v t) where

  data Instruction (BusNetwork title (PU Passive v t) v t)
    = Transport v title title
    deriving (Typeable, Show)

  data Signals (BusNetwork title (PU Passive v t) v t) = Wire Int



instance ( Title title, Var v, Time t
         ) => ByTime (BusNetwork title (PU Passive v t) v t) t where
  signalAt BusNetwork{..} (Wire i) t = foldl (+++) X $ map (uncurry subSignal) $ bnWires ! i
    where
      subSignal puTitle s = case bnPus M.! puTitle of
        PU pu' -> gsignalAt pu' s t



instance ( PUClass (Network title) (BusNetwork title (PU Passive v t) v t) v t
         , Similatable (PU Passive v t) v Int
         , Typeable title, Typeable (PU Passive v t)
         , Ord title, Show title
         , Var v, Time t
         ) => Similatable (BusNetwork title (PU Passive v t) v t) v Int where

  variableValue _fb bn cntx vi = varValue bn cntx vi

  varValue bn@BusNetwork{..} cntx vi@(v, _) =
    let [Transport _ src _] =
          filter (\(Transport v' _ _) -> v == v')
          $ catMaybes $ map (getInstruction $ proxy bn)
          $ steps bnProcess
    in varValue (bnPus M.! src) cntx vi



----------------------------------------------------------------------



bindingOptions BusNetwork{..} =
  concatMap bindVariants' bnRemains
  where
    bindVariants' fb =
      [ (fb, puTitle) -- , newVariants pu fb)
      | (puTitle, pu) <- sortByLoad $ M.assocs bnPus
      , isRight $ bind fb pu
      , not $ selfTransport fb puTitle
      ]

    sortByLoad = sortBy (\(a, _) (b, _) -> load a `compare` load b)
    load = length . binded

    selfTransport fb puTitle =
      not $ null $ variables fb `intersect` (concatMap variables $ binded puTitle)

    binded puTitle | puTitle `M.member` bnBinded = bnBinded M.! puTitle
                   | otherwise = []



subBind fb puTitle bn@BusNetwork{ bnProcess=p@Process{..}, ..} = bn
  { bnPus=M.adjust (\pu -> fromRight undefined $ bind fb pu) puTitle bnPus
  , bnBinded=M.alter (\v -> case v of
                         Just fbs -> Just $ fb : fbs
                         Nothing  -> Just [fb]
                     ) puTitle bnBinded
  , bnProcess=snd $ modifyProcess p $
      add (Event tick 0) $ InfoStep $ "Bind " ++ show fb ++ " to " ++ puTitle
  , bnRemains=filter (/= fb) bnRemains
  }






--------------------------------------------------------------------------

instance TestBenchRun (BusNetwork title spu v t) where
  buildArgs _ = [ "hdl/dpu_fram.v"
                , "hdl/fram_net.tb.v"
                ]



instance ( Typeable title, Ord title, Show title, Var v, Time t
         , Typeable (PU Passive v t)
         , PUClass Passive (PU Passive v t) v t
         , Similatable (PU Passive v t) v Int
         ) => TestBench (BusNetwork title (PU Passive v t) v t) v Int where

  components _ =
    [ ( "hdl/fram_net_signals.v", testSignals )
    , ( "hdl/fram_net_outputs.v", testOutputs )
    ]
    where
      testSignals bn@BusNetwork{ bnProcess=Process{..}, ..} _cntx
        = concatMap ( (++ " @(negedge clk)\n") . showSignals . signalsAt ) [ 0 .. tick + 1 ]
        where
          wires = map Wire $ reverse $ range $ bounds bnWires
          signalsAt t = map (\w -> signalAt bn w t) wires
          showSignals = (\ss -> "wires <= 'b" ++ ss ++ ";" ) . concat . map show

      testOutputs BusNetwork{ bnProcess=p@Process{..}, ..} cntx
        = concatMap ( ("@(posedge clk); #1; " ++) . (++ "\n") . assert ) [ 0 .. tick + 1 ]
        where
          assert time = case effectAt time p of
            Just (Pull (v : _))
              | (v, 0) `M.member` cntx -> concat
                [ "if ( !(dp_data == " ++ show (cntx M.! (v, 0)) ++ ") ) "
                ,   "$display("
                ,     "\""
                ,       "FAIL wrong value of " ++ show' v ++ " the bus failed "
                ,       "(got: %h expect: %h)!"
                ,     "\", "
                , "dp_data, " ++ show (cntx M.! (v, 0)) ++ ");"
                ]
            _ -> "/* assert placeholder */"
          show' s = filter (/= '\"') $ show s

  simulateContext bn@BusNetwork{..} cntx =
    let transports = getInstructions (proxy bn) bnProcess
    in foldl ( \cntx' (Transport v src _dst) ->
                 M.insert (v, 0) (varValue (bnPus M.! src) cntx' (v, 0)) cntx'
             ) cntx transports
