{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

{-
TODO: Место для микростатьи по вопросу: как именно соотносятся между собой решения по привязке,
пересылкам и управлению потоком данных.
-}
module NITTA.Flows
  ( DataFlow(..), isPaths
  , ControlFlow(..), dataFlow2controlFlow, isChoice, isBlock
  , OptionCF(..)
  , BranchedProcess(..)
  , allowByControlFlow
  ) where

import           Data.Aeson
import           Data.Default
import           Data.List        (nub, (\\))
import qualified Data.Map         as M
import           Data.Text        (pack)
import           Data.Typeable
import           GHC.Generics
import           NITTA.BusNetwork
import           NITTA.Types
import           NITTA.Utils




-- * Поток управления и поток данных.


-- | Структура данных для представления прикладного программного кода.
--
-- Поток данных представляется в виде графа, описывающего взаимосвязи между функциональными блоками.
-- При этом в графе могут быть "условные фрагменты".
--
-- TODO: Сделать визуализацию с явным выделением Stage-ов.
data DataFlow v
  -- | Актор, вданном случае - функциональноый блок.
  = Actor (FB (Parcel v) v)
  -- | Стадия вычислительного процесса. Может иметь произвольное колличество прыжков от
  -- входа к выходу. Важным свойством стадии является возможность параллельного выполнения
  -- всех элементов, входящих в её состав. Исключение - стадия с несколькими вариантами потока
  -- данных.
  | Stage [DataFlow v]
  -- Ветвление вычислительного процесса. Осуществляется на основе одного значений. Количество
  -- вариантов переходов, потенциально, не ограничено.
  | Paths
    { dfCond  :: v -- ^ Ключ для перехода.
    , dfPaths :: [(Int, DataFlow v)] -- ^ Таблица переходов, ключ -> программа в данной ветке.
    }
  deriving ( Show )

instance ( Var v ) => Variables (DataFlow v) v where
  variables (Actor fb)  = variables fb
  variables (Stage ps)  = concatMap variables ps
  -- fixme -- outputs and internal transfers...
  variables s@Paths{..} = dfCond : inputsOfFBs (functionalBlocks s)

instance WithFunctionalBlocks (DataFlow v) (FB (Parcel v) v) where
  functionalBlocks (Actor fb) = [fb]
  functionalBlocks (Stage ss) = concatMap functionalBlocks ss
  functionalBlocks Paths{..}  = concatMap (functionalBlocks . snd) dfPaths




-- | Структура для описания потока управления с точки зрения пересылок данных.
data ControlFlow tag v
  -- | Атомарной операцией является пересылка данных.
  = Atom v
  -- | Блок операций пересылоккоторые должны быть выполнены группой, при этом реальная
  -- последовательность пересылок указанных в блоке данных не принципиальна.
  | Block [ControlFlow tag v]
  -- | Блок вариантивного развития вычислительного процесса. Рассматривается как атомарный,
  -- так как иначе не получится обеспечить гарантированное время исполнения и целостность
  -- вычислительного процесса.
  | Choice{ -- | Ключ выбора варианта развития вычислительного процесса. Принципиальное отличие от
            -- cfInputs заключается в том, что эта пересылка обязательно перед выбором.
            --
            -- При этом не очень понятно, почему она тут, а не в предыдущем блоке?
            cfCond    :: v
            -- | Набор входных данных для вариативного развития вычислительного процесса.
          , cfInputs  :: [v]
            -- | Варианты ветвления вычилсительного процесса.
          , cfOptions :: [OptionCF tag v]
          }
  deriving ( Show, Eq )

instance ( Var v ) => Variables (ControlFlow tag v) v where
  variables (Atom v)    = [v]
  variables (Block cfs) = concatMap variables cfs
  variables Choice{..}  = cfCond : concatMap (variables . oControlFlow) cfOptions

isPaths Paths{} = True
isPaths _       = False



-- | Ветка потока управления.
data OptionCF tag v
  = OptionCF
  { ocfTag       :: Maybe tag -- ^ Тег ветки времени.
  , ocfInputs    :: [v] -- ^ Входные переменные ветки потока управления.
  , oControlFlow :: ControlFlow tag v -- ^ Вложенный поток управления.
  } deriving ( Show, Eq )


dataFlow2controlFlow (Actor fb) = Block $ map Atom $ variables fb
dataFlow2controlFlow paths@Paths{..}
  = let inputs' = inputsOfFBs $ functionalBlocks paths
        dfPaths' = map (\(key, prog) -> OptionCF
                         { ocfTag=Just $ show dfCond ++ " == " ++ show key
                         , ocfInputs=inputs' \\ variables prog
                         , oControlFlow=dataFlow2controlFlow prog
                         }
                       ) dfPaths
    in Choice dfCond inputs' dfPaths'
dataFlow2controlFlow (Stage ss)
  = let cf = map dataFlow2controlFlow ss
        parallel = filter isBlock cf
        parallel' = nub $ concatMap (\(Block xs) -> xs) parallel
        withInputs = parallel' ++ nub (filter (not . isBlock) cf)
        inputsVariables = nub $ map Atom $ concatMap (\(Choice _ vs _) -> vs)
                          $ filter isChoice withInputs
    in Block $ withInputs \\ inputsVariables


isBlock Block{} = True
isBlock _       = False
isChoice Choice{} = True
isChoice _        = False



-- | При моделировании вычислительного процесса вычислительный процесс развивается не только в
-- рамках его потока данных, но и одновременно в рамках потока управления. При этом возможно
-- ситуация, когда какое-то действие допустимо с точки зрения потока данных (один вычислительный
-- блок готов выслать данный, а другой принять данные), но при этом данная пересылка должна
-- реализовываться только в случае выбора одной из веток вычислительного процесса. В таком случае
-- компилятору необходимо осуществлять проверку, можем ли мы с точки зрения потока управления
-- выполнить ту или иную пересылку данных. Для этого и служит эта фунция.
allowByControlFlow (Atom v)   = [ v ]
allowByControlFlow Choice{..} = [ cfCond ]
allowByControlFlow block@(Block cfs)
  | not $ any isBlock cfs = concatMap allowByControlFlow cfs
  | otherwise = error $ "Bad controlFlow: " ++ show block




-- | Описание ветвящегося вычислительного процесса. Из-за того что планирование вычислительного
-- процесса происходит глобально, то и ветвление времени происходит глобально.
-- Возможное от данного тезиса будет полезно абстрагироваться, если Choice целиком попадает на
-- отдельную подсеть, но пока до таких вопросов ещё очень далеко.
data BranchedProcess title tag v t
  -- | Описание ветки вычислительного процесса.
  = Branch
  { -- | Вычислительный блок, в рамках которого реализуется весь вычислительный процесс.
    --
    -- TODO: Убрать hardcode.
    topPU        :: BusNetwork title v t
    -- | Описание текущего потока управления (в случае если мы находимся в одной из веток
    -- вычислительного процесса, то описывается только её поток управления)
  , controlFlow  :: ControlFlow tag v
    -- | Тег времени, идентифицирующий данную ветку вычислиельного процесса.
  , branchTag    :: Maybe tag
    -- | Входные данные рассматриваемой ветки вычислительного процесса.
  , branchInputs :: [v]
  }
  -- | Куст вычислительного процесса. Процесс расщеплён на множество веток имеющих один корень и
  -- сходящихся в одну точку по их завершению.
  | Bush
  { -- | Ветка процесса, планируемая в текущий момент времени.
    currentBranch     :: BranchedProcess title tag v t
    -- | Ветки процесса, требующие планирования.
  , remainingBranches :: [ BranchedProcess title tag v t ]
    -- | Спланированные ветки процесса.
  , completedBranches :: [ BranchedProcess title tag v t ]
    -- | Исходная ветка, которая была расщеплена. Используется как база для слияния куста.
  , rootBranch        :: BranchedProcess title tag v t
  } deriving ( Generic )


instance ( ToJSON title, ToJSONKey title
         , ToJSON tag, ToJSON t
         , Show v, ToJSON v, Title title, Time t, Var v
         ) => ToJSON (BranchedProcess title tag v t)



instance ( Var v, ToJSONKey title, Time t, Title title, ToJSON title, ToJSON t
         ) => ToJSON (BusNetwork title v t) where
  toJSON n@BusNetwork{..}
             -- , bnSignalBusWidth     :: Int
    = object [ "width" .= bnSignalBusWidth
             --   bnRemains            :: [FB (Parcel v) v]
             , "remain" .= bnRemains
             -- , bnForwardedVariables :: [v]
             , "forwardedVariables" .= map (String . pack . show) bnForwardedVariables
             -- , bnBinded             :: M.Map title [FB (Parcel v) v]
             , "binds" .= bnBinded
             -- , bnProcess            :: Process v t
             , "processLength" .= nextTick (process n)
             -- , bnPus                :: M.Map title spu
             , "processUnits" .= M.keys bnPus
             ]

instance ( Show (FB (Parcel v) v) ) => ToJSON (FB (Parcel v) v) where
  toJSON = String . pack . show

instance ToJSON (ControlFlow tag v) where
  toJSON _ = String "Control Flow"



instance ( Var v ) => DecisionProblem (BindingDT String v)
                            BindingDT (BranchedProcess String tag v t)
         where
  options _ Branch{..} = options binding topPU
  options _ _          = undefined
  decision _ branch@Branch{..} act = branch{ topPU=decision binding topPU act }
  decision _ _ _                   = undefined

instance ( Typeable title, Ord title, Show title, Var v, Time t
         ) => DecisionProblem (DataFlowDT title v t)
                   DataFlowDT (BranchedProcess title tag v t)
         where
  options _ Branch{..} = options dataFlowDT topPU
  options _ _          = undefined
  decision _ branch@Branch{..} act = branch{ topPU=decision dataFlowDT topPU act }
  decision _ _ _                   = undefined
