-- {-# OPTIONS -Wall -fno-warn-missing-signatures #-}
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
import           Data.Maybe              (isJust)
import           Debug.Trace
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





-- data SplitTime tag = Single Int
                   -- | Splited SplitTime tag

data Program v
  = Statement (FB v)
  | DataFlow [Program v]
  | Switch
    { conduction :: v
    , inputs     :: [v]
    , outputs    :: [v]
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




data ControlFlow v
  = Atom v
  | Parallel [ControlFlow v]
  | Split v [v] [ControlFlow v]
  deriving ( Show, Eq )


isSplit (Split _ _ _) = True
isSplit _             = False

mkControlFlow (Statement fb) = Parallel $ map Atom $ variables fb
mkControlFlow Switch{..}
  = Split conduction inputs $ map (mkControlFlow . snd) branchs
mkControlFlow (DataFlow ss)
  = let cf = map mkControlFlow ss
        parallel = filter isParallel cf
        parallel' = nub $ concatMap (\(Parallel xs) -> xs) parallel
        withInputs = parallel' ++ nub (filter (not . isParallel) cf)
        inputsVariables = nub $ map Atom $ concatMap (\(Split _ vs _) -> vs)
                          $ filter isSplit withInputs
    in Parallel $ withInputs \\ inputsVariables



data ControlModel v
  = ControlModel
    { controlFlow   :: ControlFlow v
    , currentPlace  :: ControlModel v -> ControlModel v
    , usedVariables :: [v]
    }

instance ( Var v ) => Show (ControlModel v) where
  show ControlModel{..} = "cf: " ++ show controlFlow ++ " used: " ++ show usedVariables

currentCotrolFlow cm = controlFlow $ (currentPlace cm) cm




controlFlowOptions cm
  = let cf' = trace (show $ currentCotrolFlow cm) $ currentCotrolFlow cm
        tmp = controlOptions' cf'
        x = filter (`notElem` usedVariables cm) tmp
    in x
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
  , v `elem` opts
  = if length opts == 1
    then cm{ usedVariables=v : usedVariables, currentPlace=id }
    else cm{ usedVariables=v : usedVariables }
  | otherwise = error $ "Wrong control step: " ++ show v



-- splitVariants cm@ControlModel{..} = place cm



-- splitProcess v = undefined

-- Текущее состояние с точки зрения ControlFlow - определяет текущей положение в алгоритме,
-- в том числе и способ описания времени.
-- Описание текущего момента времени - беда.


-- dataFlow (DataFlow ps)  = concat dataFlow ps
-- dataFlow Switch{..}     = []
-- dataFlow (Statement fb) = [ variables ]

-- currentDataFlow =



instance ( Var v ) => Vars (Program v) v where
  variables (DataFlow ps)  = concatMap variables ps
  variables (Statement fb) = variables fb
  variables Switch{..}     = conduction : inputs ++ outputs




threshhold = 2

naive net controlModel
  = let opts = sensibleOptions $ filterByControlModel $ options net
        bindOpts = bindingOptions net
    in case trace (concatMap ((">"++) . (++"\n") . show) $ controlFlowOptions controlModel)
            (opts, bindOpts) of
         ([], [])   -> (net, controlModel)
         (_ : _, _) | length opts >= threshhold -> afterStep
         (_, _ : _) -> (autoBind net, controlModel)
         (_ : _, _) -> afterStep
  where
    filterByControlModel opts
      = let cfOpts = controlFlowOptions controlModel
        in map (\t@TransportOpt{..} -> t
                 { toPush=M.fromList $ map (\(v, desc) -> (v, if v `elem` cfOpts
                                                             then desc
                                                             else Nothing)
                                          ) $ M.assocs toPush
                }) opts

    afterStep -- pu@BusNetwork{..} cm
      = case sortBy (\a b -> start a `compare` start b) $ sensibleOptions $ options net of
        v:_ -> let act = option2action v
                   cm' = controlModelStep controlModel
                         $ map fst $ filter (isJust . snd) $ M.assocs $ taPush act
               in (step net act, cm')
        _   -> error "No variants!"
    controlModelStep cm (v:[]) = controlStep cm v
    controlModelStep cm (v:vs) = controlModelStep (controlStep cm v) vs
    start = tcFrom . toPullAt
    -- mostly mad implementation
    option2action TransportOpt{ toPullAt=TimeConstrain{..}, ..}
      = TransportAct
        { taPullFrom=toPullFrom
        , taPullAt=Event tcFrom tcDuration
        , taPush=M.map (fmap $ \(title, TimeConstrain{..}) ->
                                 (title, Event pushStartAt tcDuration)
                       ) toPush
        }
      where
        pushStartAt = tcFrom + tcDuration



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
  in case trace' prioritized of
      (BindOption fb puTitle _):_ -> subBind fb puTitle net
      _                           -> error "Bind variants is over!"
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
      = bv{ priority=Just $ Restless tcFrom }

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
