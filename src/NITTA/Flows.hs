{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE PartialTypeSignatures  #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module NITTA.Flows
  ( Program(..)
  , Forks(..)
  , ControlModel(..)
  , controlModelOptions, controlModelStep
  , ControlFlow(..), isSplit, isParallel, isSwitch, mkControlFlow
  , SplitBranch(..)
  )
where

import           Data.Default
import           Data.List        (nub, (\\))
import           NITTA.BusNetwork
import           NITTA.Types
import           NITTA.Utils

-- import           Debug.Trace


data Program v
  = Statement (FB v)
  | DataFlow [Program v]
  | Switch
    { conduction :: v
    , branchs    :: [(Int, Program v)]
    }
  deriving ( Show )

instance ( Var v ) => Vars (Program v) v where
  variables (DataFlow ps)  = concatMap variables ps
  variables (Statement fb) = variables fb
  -- fixme -- outputs and internal transfers...
  variables s@Switch{..}   = conduction : inputsOfFBs (functionalBlocks s)

instance WithFunctionalBlocks (Program v) v where
  functionalBlocks (Statement fb) = [fb]
  functionalBlocks (DataFlow ss)  = concatMap functionalBlocks ss
  functionalBlocks Switch{..}     = concatMap (functionalBlocks . snd) branchs



data ControlModel tag v
  = ControlModel
    { controlFlow   :: ControlFlow tag v
    , usedVariables :: [v]
    }

instance Default (ControlModel tag v) where
  def = ControlModel undefined []



data ControlFlow tag v
  = Atom v
  | Parallel [ControlFlow tag v]
  | Split{ cond         :: v
         , inputs       :: [v]
         , splitOptions :: [SplitBranch tag v]
         }
  deriving ( Show, Eq )


isSwitch Switch{} = True
isSwitch _        = False

isParallel (Parallel _) = True
isParallel _            = False

isSplit (Split _ _ _) = True
isSplit _             = False


mkControlFlow (Statement fb) = Parallel $ map Atom $ variables fb

mkControlFlow s@Switch{..}
  = let inputs = inputsOfFBs $ functionalBlocks s
        branchs' = map (\(k, prog) -> SplitBranch
                         { sTag=Just $ show conduction ++ " = " ++ show k
                         , sForceInputs=inputs \\ variables prog
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


controlModelOptions ControlModel{..}
  = filter (`notElem` usedVariables) $ controlOptions' controlFlow
  where
    controlOptions' (Atom v)     = [v]
    controlOptions' (Split v _ _) = [v] --  : vs
    controlOptions' cf@(Parallel cfs)
      | length (filter isParallel cfs) == 0
      = concatMap controlOptions' cfs
      | otherwise
      = error $ "Bad controlFlow: " ++ show cf


controlModelStep cm@ControlModel{..} v = cm{ usedVariables=v : usedVariables }



data SplitBranch tag v
  = SplitBranch
  { sTag         :: Maybe tag
  , sForceInputs :: [v]
  , sControlFlow :: ControlFlow tag v
  } deriving ( Show, Eq )

instance ( Var v ) => Vars (ControlFlow tag v) v where
  variables (Atom v)       = [v]
  variables (Parallel cfs) = concatMap variables cfs
  variables Split{..}      = cond : concatMap (variables . sControlFlow) splitOptions

instance ( Var v, Show tag ) => Show (ControlModel tag v) where
  show ControlModel{..} = "cf: " ++ show controlFlow
                          ++ " used: " ++ show usedVariables



data Forks tag v t
  = Forks
  { current   :: Forks tag v t
  , remains   :: [ Forks tag v t ]
  , completed :: [ Forks tag v t ]
  , merge     :: Forks tag v t
  }
  | Fork
  { net          :: BusNetwork String (PU Passive v t) v t
  , controlModel :: ControlModel tag v
  , timeTag      :: Maybe tag
  , forceInputs  :: [v]
  }



