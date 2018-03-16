{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

-- TODO: https://tools.ietf.org/id/draft-kelly-json-hal-03.txt
module NITTA.API where

import           Control.Concurrent.STM
import           Control.Monad
import           Control.Monad.Except
import           Data.Aeson
import           Data.Default
import           Data.Map               (Map, fromList)
import           Data.Maybe
import           GHC.Generics
import           ListT                  (toList)
import           NITTA.Compiler
import           NITTA.FlowGraph
import           NITTA.Types            hiding (Synthesis, steps)
import           NITTA.Utils.JSON       ()
import           Servant
import qualified STMContainers.Map      as M


data Synthesis
  = Synthesis{ parent :: Maybe (String, Int) -- ^ (name, tick)
             , childs :: [String] -- TODO: привязка к шагу процесса синтеза, а не к названию синтеза.
             , steps  :: [ST]
             } deriving ( Generic )


type T = TaggedTime String Int
type ST = CompilerStep String String String (TaggedTime String Int)
instance ToJSON ST
instance ToJSON Synthesis


instance Default Synthesis where
  def = Synthesis{ parent=Nothing
                 , childs=[]
                 , steps=[]
                 }



type SynthesisAPI
     = "synthesis" :> Get '[JSON] (Map String Synthesis)
  :<|> "synthesis" :> Capture "sId" String :>
       ( Get '[JSON] Synthesis
    :<|> QueryParam' '[Required] "parent" String :>
         QueryParam' '[Required] "step" Int :>
         PostNoContent '[JSON] ()
    :<|> StepsAPI
       )

synthesisServer stm
     = fmap fromList ( liftIO $ atomically $ toList $ M.stream stm )
  :<|> \ sId -> getSynthesis stm sId
           :<|> ( \ pId stepId -> liftSTM $ postSynthesis sId pId stepId )
           :<|> stepsServer stm sId
  where
    postSynthesis sId pId stepId = do
      syn <- M.lookup sId stm
      when ( isJust syn ) $ throwSTM err409{ errBody="Synthesis already exist." }
      parent <- M.lookup pId stm
      when ( isNothing parent ) $ throwSTM err404{ errBody="Parent not found." }
      let Just parent'@Synthesis{ childs=cs } = parent
      M.insert parent'{ childs=sId : cs } pId stm
      M.insert parent'{ parent=Just ( pId, stepId )
                      -- TODO: crop steps by stepId
                      } sId stm



type StepsAPI
     = "steps" :> Get '[JSON] [ST]
  :<|> "steps" :> Capture "step" Int :>
         -- TODO: step - необходимо обеспечить сохранность индексов для разных
         -- веток синтеза.
       ( Get '[JSON] ST
         -- Дублирование auto в path - костыль. Проблема в следующем - параметры
         -- и флаги не влияют на имя функции в автоматически генерируемом API
         -- для JS, что приводит к утере одного из методов. Что бы решить эту
         -- проблему - параметр был явно указан в path.
    :<|> "auto" :> QueryFlag "auto" :> Post '[JSON] ST
    :<|> QueryParam' '[Required] "manual" Int :> Post '[JSON] ST
    :<|> "config" :> Get '[JSON] NaiveOpt
    :<|> "decisions" :> Get '[JSON] [Decision (CompilerDT String String String T)]
    :<|> "options" :> Get '[JSON] [ ( Int
                                    , GlobalMetrics
                                    , SpecialMetrics
                                    , Option (CompilerDT String String String T)
                                    , Decision (CompilerDT String String String T)
                                    )
                                  ]
       )

stepsServer stm sId
     = ( reverse . steps <$> getSynthesis stm sId )
  :<|> \ stepId -> getStep stepId
              :<|> ( \ True -> liftSTM $ autoPostStep stepId )
              :<|> ( liftSTM . manualPostStep stepId )
              :<|> ( config <$> getStep stepId )
              :<|> ( map option2decision . options compiler <$> getStep stepId )
              :<|> ( optionsWithMetrics <$> getStep stepId )
  where
    getStep = liftSTM . getStep'
    getStep' stepId = do
      Synthesis{..} <- getSynthesis' stm sId
      unless ( length steps > stepId ) $ throwSTM err409{ errBody="Step not exists." }
      return $ reverse steps !! stepId
    autoPostStep stepId = do
      syn@Synthesis{..} <- getSynthesis' stm sId
      unless ( length steps <= stepId ) $ throwSTM err409{ errBody="Steps already exist." }
      let steps' = foldl (\(x:xs) _ -> mkStep x : x : xs) steps [length steps .. stepId]
      M.insert syn{ steps=steps' } sId stm
      return $ head steps'
    manualPostStep stepId decisionId = do
      syn@Synthesis{ steps=steps@(step:_) } <- getSynthesis' stm sId
      unless (length steps == stepId) $ throwSTM err409{ errBody="Only one manual step at a time." }
      let d = ((!! decisionId) . map option2decision . options compiler) step
      let step' = decision compiler step d
      let syn' = syn{ steps=step' : steps }
      M.insert syn' sId stm
      return step'
    mkStep step = fromMaybe (error "Synthesis is over.") $ naive' step


liftSTM stm = liftIO (atomically $ catchSTM (Right <$> stm) (return . Left)) >>= either throwError return

getSynthesis stm sId = liftIO $ atomically $ getSynthesis' stm sId
getSynthesis' stm sId = do
  syn <- M.lookup sId stm
  maybe (throwSTM err404) return syn


getDecision stm sId stepId = do
  Synthesis{..} <- getSynthesis stm sId
  return $ steps !! stepId


app root = do
  stm <- atomically $ do
    st <- M.new
    M.insert def{ steps=[root] } "root" st
    return st
  return $ serve (Proxy :: Proxy SynthesisAPI) $ synthesisServer stm
