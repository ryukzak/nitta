{-# OPTIONS -Wall -fno-warn-missing-signatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}

module NITTA.BusNetwork where

import           Control.Monad.State
import           Data.Array
import           Data.Default
import           Data.Either
import           Data.Functor.Const
import           Data.List               (find, intersect, nub, sortBy, (\\))
import qualified Data.Map                as M
import           Data.Maybe              (catMaybes, fromMaybe, isJust)
import           Data.Typeable
-- import           NITTA.FunctionBlocks    (unbox)
import           NITTA.ProcessUnits.Fram
import           NITTA.TestBench
import           NITTA.Types
import           NITTA.Utils




data BusNetwork title ty v t =
  BusNetwork
    { bnRemains            :: [FB v]
    , bnForwardedVariables :: [v]
    , bnBinded             :: M.Map title [FB v]
    , bnPus                :: M.Map title (PU Passive v t)
    , bnProcess            :: Process v t
    , bnWires              :: Array Int [(title, S)]
    }
-- deriving instance (Show title, Show ty, Show v, Show t, Show (PU Passive v t)) =>
-- Show( BusNetwork title ty v t )
busNetwork pus wires = BusNetwork [] [] (M.fromList []) (M.fromList pus) def wires



type II title v = Instruction (BusNetwork title) v



instance ( Typeable title, Ord title, Show title, Var v, Time t
         ) => PUClass (BusNetwork title) (Network title) v t where

  data Instruction (BusNetwork title) v = Transport v title title
    deriving (Typeable, Show)

  data Signals (BusNetwork title) = Wire Int

  signal' BusNetwork{..} (Wire i) t = foldl (+++) X $ map (uncurry subSignal) $ bnWires ! i
    where
      subSignal puTitle s = case bnPus M.! puTitle of
                                 PU pu -> signal pu s t

  bind fb bn@BusNetwork{..}
    | any (\pu -> isRight $ bind fb pu) $ M.elems bnPus
    = Right bn{ bnRemains=fb : bnRemains }
  bind _fb _bn = Left "no"

  options BusNetwork{..} = concat $
    [
      [ TransportOpt fromPu pullAt $ M.fromList pushs
      | pushs <- sequence $ map pushOptionsFor pullVars
      , let pushTo = catMaybes $ map (fmap fst . snd) pushs
      , length (nub pushTo) == length pushTo
      ]
    | (fromPu, vars) <- puOptions
    , EffectOpt (Pull pullVars) pullAt <- vars
    ]
    where
      pushOptionsFor v | v `notElem` availableVars = [(v, Nothing)]
      pushOptionsFor v = (v, Nothing) : pushOptionsFor' v

      pushOptionsFor' v = [ (v, Just (pushTo, pushAt))
                          | (pushTo, vars) <- puOptions
                          , EffectOpt (Push pushVar) pushAt <- vars
                          , pushVar == v
                          ]
      availableVars =
        let functionalBlocks = bnRemains ++ (concat $ M.elems bnBinded)
            alg = foldl
                  (\dict (a, b) -> M.adjust ((:) b) a dict)
                  (M.fromList [(v, []) | v <- concatMap variables functionalBlocks])
                  $ filter (\(_a, b) -> b `notElem` bnForwardedVariables)
                  $ concatMap dependency functionalBlocks
            notBlockedVariables = map fst $ filter (null . snd) $ M.assocs alg
        in notBlockedVariables \\ bnForwardedVariables

      puOptions = M.assocs $ M.map options bnPus

  step ni@BusNetwork{..} act@TransportAct{..} = ni
    { bnPus=foldl (\s n -> n s) bnPus steps
    , bnProcess=snd $ modifyProcess bnProcess $ do
        mapM_ (\(v, (title, _)) -> add (Event transportStartAt transportDuration)
                (Transport v taPullFrom title))
                $ M.assocs push'
        _ <- add (Event transportStartAt transportDuration) $ Const $ show act -- $ Pull pullVars
        setTime $ transportStartAt + transportDuration
    , bnForwardedVariables=pullVars ++ bnForwardedVariables
    }
    where
      transportStartAt = eStart taPullAt
      transportDuration = maximum $
        map ((\Event{..} -> (eStart - transportStartAt) + eDuration) . snd) $ M.elems push'
      -- if puTitle not exist - skip it...
      pullStep = M.adjust (\dpu -> step dpu $ EffectAct (Pull pullVars) taPullAt) taPullFrom
      pushStep (var, (dpuTitle, pushAt)) =
        M.adjust (\dpu -> step dpu $ EffectAct (Push var) pushAt) dpuTitle
      pushSteps = map pushStep $ M.assocs push'
      steps = pullStep : pushSteps

      push' = M.map (fromMaybe undefined) $ M.filter isJust taPush
      pullVars = M.keys push'


  process BusNetwork{..} = let
    transportKey = M.fromList
      [ (variable, uid)
      | (Just (Transport variable _ (_ :: v)), uid)
          <- map (\Step{..} -> (cast info, uid)) $ steps bnProcess
      ]
    p'@Process{ steps=steps' } = snd $ modifyProcess bnProcess $ do
      let pus = sortBy (\a b -> fst a `compare` fst b) $ M.assocs bnPus
      mapM (addSubProcess transportKey) pus

    in p'{ steps=reverse steps' }
    where
      addSubProcess transportKey (puTitle, pu) = do
        let subSteps = steps $ process pu
        uids' <- foldM (\dict Step{..} -> do
                           uid' <- add time $ Nested uid puTitle info
                           case cast info of
                             Just (fb :: FB v) ->
                               mapM_ (\v -> when (v `M.member` transportKey)
                                         $ relation $ Vertical (transportKey M.! v) uid'
                                     ) $ variables fb
                             Nothing -> return ()
                           return $ M.insert uid uid' dict
                       ) M.empty subSteps
        let subRelations = relations $ process pu
        mapM (\r -> relation $ case r of
                 Vertical a b -> Vertical (uids' M.! a) (uids' M.! b)
             ) subRelations

  variableValue _fb bn cntx vi = varValue bn cntx vi

  varValue bn@BusNetwork{..} cntx vi@(v, _) =
    let Just (_, Transport _ src _) =
          find (\(_, Transport v' _ _) -> v == v') $ filterSteps $ process bn
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
      add (Event tick 0) $ Const $ "Bind " ++ show fb ++ " to " ++ puTitle
  , bnRemains=filter (/= fb) bnRemains
  }






--------------------------------------------------------------------------

instance TestBenchFiles (BusNetwork title (Network title) v t) where
  fileName _ = "hdl/fram_net"

instance ( Typeable title, Ord title, Show title, Var v, Time t
         , TestBenchFiles (BusNetwork title (Network title) v t)
         ) => TestBench (BusNetwork title) (Network title) v t Int where

  testSignals bn@BusNetwork{ bnProcess=Process{..}, ..} _cntx =
    concatMap ( (++ " @(negedge clk)\n") . showSignals . signalsAt ) [ 0 .. tick + 1 ]
    where
      wires = map Wire $ reverse $ range $ bounds bnWires
      signalsAt t = map (\w -> signal' bn w t) wires

      showSignals = (\ss -> "wires <= 'b" ++ ss ++ ";" ) . concat . map show

  simulateContext bn@BusNetwork{..} cntx =
    let transports =
          [ transport
          | (_, transport) <-
              sortBy (\(a,_) (b,_) -> stepStart a `compare` stepStart b)
              $ filterSteps $ process bn
          ]
    in foldl ( \cntx' (Transport v src _dst) ->
                 M.insert (v, 0) (varValue (bnPus M.! src) cntx' (v, 0)) cntx'
             ) cntx transports

  testInputs _ _ = ""

  testOutputs BusNetwork{ bnProcess=p@Process{..}, ..} cntx =
    concatMap ( ("@(posedge clk); #1; " ++) . (++ "\n") . assert ) [ 0 .. tick + 1 ]
    where
      assert time = case infoAt time p of
        [Pull (v : _)]
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
