{-# LANGUAGE PartialTypeSignatures  #-}
-- {-# OPTIONS -Wall -fno-warn-missing-signatures #-}
{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE UndecidableInstances   #-}

module NITTA.Compiler where

-- import           Control.Lens
import           Control.Monad
import           Data.Array              (elems)
import           Data.Either
import           Data.List               (find, intersect, nub, partition,
                                          sortBy, (\\))
import qualified Data.Map                as M
import           Data.Maybe              (catMaybes, isJust)
import           Debug.Trace

import           Data.Typeable
import           NITTA.BusNetwork
import           NITTA.FunctionBlocks
import           NITTA.ProcessUnits.Fram
import           NITTA.Types
import           NITTA.Utils


bindAll pu alg = fromRight undefined $ foldl nextBind (Right pu) alg
  where
    nextBind (Right pu') fb = bind fb pu'
    nextBind (Left r) _     = error r

manualSteps pu acts = foldl (\pu' act -> step pu' act) pu acts

bindAllAndNaiveSteps pu0 alg = naive' $ bindAll pu0 alg
  where
    naive' pu
      | var:_ <- options pu =
          naive'
          -- $ trace (concatMap ((++ "\n") . show) $ elems $ frMemory pu)
          $ step pu $ effectVar2act var
      | otherwise = pu

effectVar2act EffectOpt{ eoAt=TimeConstrain{..}, .. } = EffectAct eoEffect $ Event tcFrom tcDuration






data Program v
  = Statement (FB v)
  | DataFlow [Program v]
  | Switch
    { conduction :: v
    , branchs    :: [(Int, Program v)]
    }
  deriving ( Show )

isSwitch Switch{} = True
isSwitch _        = False

isParallel (Parallel _) = True
isParallel _            = False



class WithFunctionalBlocks a v | a -> v where
  functionalBlocks :: a -> [FB v]



instance WithFunctionalBlocks (Program v) v where
  functionalBlocks (Statement fb) = [fb]
  functionalBlocks (DataFlow ss)  = concatMap functionalBlocks ss
  functionalBlocks Switch{..}     = concatMap (functionalBlocks . snd) branchs




isSplit (Split _ _ _) = True
isSplit _             = False

mkControlFlow (Statement fb) = Parallel $ map Atom $ variables fb
mkControlFlow s@Switch{..}
  = let inputs = inputsOfFBs $ functionalBlocks s
        branchs' = map (\(k, prog) -> SplitBranch
                         { sTag=show conduction ++ " = " ++ show k
                         , sForceInputs= --trace ("#" ++ show inputs ++ " "
                                               -- ++ show (variables prog)
                                              -- ) $
                                        inputs \\ variables prog
                         , sControlFlow=mkControlFlow prog
                         }
                       ) branchs
  in Split conduction inputs $ branchs'


mkControlFlow (DataFlow ss)
  = let cf = map mkControlFlow ss
        parallel = filter isParallel cf
        parallel' = nub $ concatMap (\(Parallel xs) -> xs) parallel
        withInputs = parallel' ++ nub (filter (not . isParallel) cf)
        inputsVariables = nub $ map Atom $ concatMap (\(Split _ vs _) -> vs)
                          $ filter isSplit withInputs
    in Parallel $ withInputs \\ inputsVariables




instance ( Var v ) => Show (ControlModel v) where
  show ControlModel{..} = "cf: " ++ show controlFlow
                          ++ " used: " ++ show usedVariables
                          ++ " tag: " ++ currentTag

currentPlace ControlModel{..} = foldr (\get s -> get s) controlFlow currentPlace'



data SplitBranch v
  = SplitBranch
  { sTag         :: String
  , sForceInputs :: [v]
  , sControlFlow :: ControlFlow v
  } deriving ( Show, Eq )

data ControlFlow v
  = Atom v
  | Parallel [ControlFlow v]
  | Split{ cond         :: v
         , inputs       :: [v]
         , splitOptions :: [SplitBranch v]
         }
  deriving ( Show, Eq )

instance ( Var v ) => Vars (ControlFlow v) v where
  variables (Atom v)       = [v]
  variables (Parallel cfs) = concatMap variables cfs
  variables Split{..}      = cond : concatMap (variables . sControlFlow) splitOptions

data ControlModel v
  = ControlModel
    { controlFlow   :: ControlFlow v
    , currentPlace' :: [ ControlFlow v -> ControlFlow v ]
    , currentTag    :: String
    , usedVariables :: [v]
    }




selectSplit tag (Parallel cfs)
  = let splits = filter isSplit cfs
        Just splitOpts = find (any ((== tag) . sTag)) $ map splitOptions splits
        Just branch = find ((== tag) . sTag) splitOpts
    in sControlFlow branch




controlFlowOptions cm
  = let cf = currentPlace cm
    in filter (`notElem` usedVariables cm) $ controlOptions' cf
  where
    controlOptions' (Atom v)     = [v]
    controlOptions' (Split v vs _) = [v] --  : vs
    controlOptions' cf@(Parallel cfs)
      | length (filter isParallel cfs) == 0
      = concatMap controlOptions' cfs
      | otherwise
      = error $ "Bad controlFlow: " ++ show cf

controlStep cm@ControlModel{..} v
  | let opts = controlFlowOptions cm
  -- , v `elem` opts --
  = if length opts == 1
    then cm{ usedVariables=v : usedVariables } --, currentPlace'=[] }
    else cm{ usedVariables=v : usedVariables }
  | otherwise = error $ "Wrong control step: " ++ show v
                ++ " opts: " ++ show (controlFlowOptions cm)



timeSplitOptions controlModel availableVars
  = let splits = filter isSplit $ (\(Parallel ss) -> ss) $ currentPlace controlModel
    in filter isAvalilable splits
  where
    isAvalilable (Split c vs _) = all (`elem` availableVars) $ c : vs



inputsOfFBs fbs
  = let deps0 = (M.fromList [(v, []) | v <- concatMap variables fbs])
        deps = foldl (\dict (a, b) -> M.adjust ((:) b) a dict) deps0 $ concatMap dependency fbs
    in map fst $ filter (null . snd) $ M.assocs deps

-- outputsOfFBs fbs
--   = let deps0 = (M.fromList [(v, []) | v <- concatMap variables fbs])
--         deps = foldl (\dict (a, b) -> M.adjust ((:) b) a dict) deps0 $ concatMap dependency fbs
--     in filter (\a -> all (not . (a `elem`)) deps) $ M.keys deps




data Forks tag v t
  = Forks
  { current   :: Forks tag v t
  , remains   :: [ Forks tag v t ]
  , completed :: [ Forks tag v t ]
  , merge     :: Forks tag v t
  }
  | Fork
  { net          :: BusNetwork String (Network String) v t
  , controlModel :: ControlModel v
  , timeTag      :: tag
  , forceInputs  :: [v]
  }



instance ( Var v ) => Vars (Program v) v where
  variables (DataFlow ps)  = concatMap variables ps
  variables (Statement fb) = variables fb
  -- fixme -- outputs and internal transfers...
  variables s@Switch{..}   = conduction : inputsOfFBs (functionalBlocks s)


updTime bn@BusNetwork{..} t
  = bn{ bnProcess=snd $ modifyProcess bnProcess $ setTime t }


splitProcess Forks{} _ = error "Can split only single process."
splitProcess f@Fork{..} s@(Split cond is branchs)
  = let ControlModel{..} = controlModel
        t = tick $ process net
        f : fs = map (\SplitBranch{..} -> Fork
                       { net=updTime net $ setTag sTag t
                       , controlModel=controlModel{ currentPlace'=selectSplit sTag : currentPlace'
                                                  , currentTag=sTag
                                                  }
                       , timeTag=sTag
                       , forceInputs=sForceInputs
                       }
                     ) branchs
        usedVariables' = nub $ concatMap (variables . sControlFlow) branchs
    in Forks{ current=f
            , remains=fs
            , completed=[]
            , merge=f{ controlModel=controlModel
                                    { usedVariables=usedVariables' ++ usedVariables
                                    }
                     }
            }


threshhold = 2

isOver Forks{..} = isOver current && null remains
isOver Fork{..}
  = let opts = sensibleOptions $ filterByControlModel controlModel $ options net
        bindOpts = bindingOptions net
    in null opts && null bindOpts


naive !f@Forks{..}
  = let current'@Fork{ net=net' } = naive current
        t = maximum $ map (tick . process . net) $ current' : completed
        parallelSteps = concatMap
          (\Fork{ net=n
                , timeTag=tag
                } -> filter (\Step{ time=Event{..} } ->
                               trace ("@ " ++ show tag ++ " " ++ show eStart) $ case eStart of
                                TaggetTime tag' _ -> tag == tag'
                                _                 -> False
                            ) $ steps $ process n
          ) completed
    in case (isOver current', remains) of
         (True, r:rs) -> f{ current=r, remains=rs, completed=current' : completed }
         (True, _)    -> let net''@BusNetwork{ bnProcess=p }
                               = updTime net' $ setTag (timeTag merge) t
                         in merge{ net=net''{ bnProcess=snd $ modifyProcess p $ do
                                                mapM_ (\Step{..} -> add time info)
                                                  $ trace (">" ++ show parallelSteps) parallelSteps
                                            }
                                 }
         (False, _)   -> f{ current=current' }


naive !f@Fork{..}
  = let opts = sensibleOptions $ filterByControlModel controlModel $ options net
        bindOpts = bindingOptions net
        splits = timeSplitOptions controlModel availableVars
    in case (splits, opts, bindOpts) of
         ([], [], [])    -> trace "over" f
         (_, _o : _, _)  | length opts >= threshhold -> afterStep
         (_bo, _, _ : _) -> f{ net=autoBind net }
         (_, _o : _, _)  -> afterStep
         (s : _, _, _)   -> trace ("split: " ++ show s) $ splitProcess f s
  where
    availableVars = nub $ concatMap (M.keys . toPush) $ options net
    afterStep
      = case sortBy (\a b -> start a `compare` start b)
             $ sensibleOptions $ filterByControlModel controlModel $ options net of
        v:_ -> let act = option2action v
                   cm' = controlModelStep controlModel
                         $ map fst $ filter (isJust . snd) $ M.assocs $ taPush act
               in trace ("step: " ++ show act)
                  f{ net=step net act
                   , controlModel=cm'
                   }
        _   -> error "No variants!"
    controlModelStep cm (v:[]) = controlStep cm v
    controlModelStep cm (v:vs) = controlModelStep (controlStep cm v) vs
    start = tcFrom . toPullAt
    -- mostly mad implementation
    option2action TransportOpt{ toPullAt=TimeConstrain{..}, ..}
      = TransportAct
      -- = trace (">" ++ show forceInputs) $ TransportAct
        { taPullFrom=toPullFrom
        , taPullAt=Event tcFrom tcDuration
        , taPush=M.fromList $ map (\(v, o) -> ( v
                                              , case o of
                                                  Just o' -> Just $ tc2e o'
                                                  Nothing | v `elem` forceInputs -> Just $ tc2e opt
                                                  _ -> tc2e <$> o
                                              )
                            ) $ M.assocs toPush
        }
      where
        tc2e (title, TimeConstrain{..}) = (title, Event pushStartAt tcDuration)
        pushStartAt = tcFrom + tcDuration
        (_, event) : _ = catMaybes $ M.elems toPush
        opt = ("", event)



filterByControlModel controlModel opts
  = let cfOpts = controlFlowOptions controlModel
    in map (\t@TransportOpt{..} -> t
             { toPush=M.fromList $ map (\(v, desc) -> (v, if v `elem` cfOpts
                                                          then desc
                                                          else Nothing)
                                       ) $ M.assocs toPush
             }) opts




sensibleOptions = filter $
  \TransportOpt{..} -> not $ null $ filter isJust $ M.elems toPush





data BindOption title v = BindOption
  { fb       :: FB v
  , puTitle  :: title
  , priority :: Maybe BindPriority
  } deriving (Show)

data BindPriority
  = Exclusive
  | Restless Int
  | Input Int
  -- Must be binded before a other, because otherwise can cause runtime error.
  | Critical
  deriving (Show, Eq)

instance Ord BindPriority where
  Critical `compare` _ = GT
  _ `compare` Critical = LT
  (Input    a) `compare` (Input    b) = a `compare` b -- чем больше, тем лучше
  (Input    _) `compare`  _           = GT
  (Restless _) `compare` (Input    _) = LT
  (Restless a) `compare` (Restless b) = b `compare` a -- чем меньше, тем лучше
  (Restless _) `compare`  _           = GT
  Exclusive `compare` Exclusive = EQ
  Exclusive `compare` _ = LT





autoBind net@BusNetwork{..} =
  let prioritized = sortBV $ map mkBV bOpts
  in case prioritized of
      (BindOption fb puTitle _) : _ -> trace ("bind: " ++ show fb ++ " " ++ show puTitle) $
                                       subBind fb puTitle net
      _                             -> error "Bind variants is over!"
  where
    bOpts = bindingOptions net
    mkBV (fb, titles) = prioritize $ BindOption fb titles Nothing
    sortBV = reverse . sortBy (\a b -> priority a `compare` priority b)

    mergedBOpts = foldl (\m (fb, puTitle) -> M.alter
                          (\case
                              Just puTitles -> Just $ puTitle : puTitles
                              Nothing -> Just [puTitle]
                          ) fb m
                   ) (M.fromList []) bOpts

    prioritize bv@BindOption{..}
      -- В настоящий момент данная операци приводит к тому, что часть FB перестают быть вычислимыми.
      -- | isCritical fb = bv{ priority=Just Critical }

      | dependency fb == []
      , pulls <- filter isPull $ optionsAfterBind bv
      , length pulls > 0
      = bv{ priority=Just $ Input $ sum $ map (length . variables) pulls}

      | Just (variable, tcFrom) <- find (\(v, _) -> v `elem` variables fb) restlessVariables
      = bv{ priority=Just $ Restless $ fromEnum tcFrom }
      -- = bv{ priority=Just $ Restless ((\(Time t) -> t) tcFrom) }

      | length (mergedBOpts M.! fb) == 1
      = bv{ priority=Just Exclusive }

      | otherwise = bv

    restlessVariables = [ (variable, tcFrom)
      | TransportOpt{ toPullAt=TimeConstrain{..}, ..} <- options net
      , (variable, Nothing) <- M.assocs toPush
      ]

    optionsAfterBind BindOption{..} = case bind fb $ bnPus M.! puTitle of
      Right pu' -> filter (\(EffectOpt act _) -> act `optionOf` fb) $ options pu'
      _  -> []
      where
        act `optionOf` fb = not $ null (variables act `intersect` variables fb)

    trace' vs = trace ("---------"++ show (restlessVariables)
               ++ "\n" ++ (concatMap (\v -> show v ++ "\n") vs)) vs
