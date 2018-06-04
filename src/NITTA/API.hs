{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
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
import           Data.Map                    (Map, fromList)
import           Data.Maybe
import           Data.Monoid
import           GHC.Generics
import           ListT                       (toList)
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors (simpleCors)
import           NITTA.Compiler
import           NITTA.DataFlow
import           NITTA.Types                 hiding (steps)
import           NITTA.Utils.JSON            ()
import           Servant
import           Servant.JS
import qualified Servant.JS                  as SJS
import qualified STMContainers.Map           as M
import           System.Directory
import           System.FilePath.Posix       (joinPath)


data Synthesis
  = Synthesis{ parent :: Maybe (String, Int) -- ^ (name, tick)
             , childs :: [(String, Int)] -- ^ [(sId, stepId)]
             , steps  :: [ST]
             } deriving ( Generic )


type T = TaggedTime String Int
type ST = CompilerStep String String String Int (TaggedTime String Int)
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
    :<|> QueryParam' '[Required] "parentId" String :>
         QueryParam' '[Required] "stepId" Int :>
         PostNoContent '[JSON] ()
    :<|> StepsAPI
       )

synthesisServer stm
     = fmap fromList ( liftIO $ atomically $ toList $ M.stream stm )
  :<|> \ sId -> getSynthesis stm sId
           :<|> ( \ pId stepId -> liftSTM $ forktSynthesis sId pId stepId )
           :<|> stepsServer stm sId
  where
    forktSynthesis sId pId stepId = do
      syn <- M.lookup sId stm
      when ( isJust syn ) $ throwSTM err409{ errBody="Synthesis already exist." }
      p@Synthesis{..} <- getSynthesis' stm pId
      M.insert p{ childs=(sId, stepId) : childs } pId stm
      M.insert def{ parent=Just ( pId, stepId )
                  , steps=reverse $ take (stepId + 1) $ reverse steps
                  } sId stm



type StepsAPI
     = "steps" :> Get '[JSON] [ST]
  :<|> "steps" :> QueryParam' '[Required] "toEnd" Bool :> Post '[JSON] ST
  :<|> "steps" :> Capture "stepId" Int :>
       ( Get '[JSON] ST
         -- Дублирование auto в path - костыль. Проблема в следующем - параметры
         -- и флаги не влияют на имя функции в автоматически генерируемом API
         -- для JS, что приводит к утере одного из методов. Что бы решить эту
         -- проблему - параметр был явно указан в path.
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
     = ( steps <$> getSynthesis stm sId )
  :<|> ( liftSTM . autoPostStep )
  :<|> \ stepId -> getStep stepId
              :<|> ( liftSTM . manualPostStep stepId )
              :<|> ( config <$> getStep stepId )
              :<|> ( map option2decision . options compiler <$> getStep stepId )
              :<|> ( optionsWithMetrics <$> getStep stepId )
  where
    getStep = liftSTM . getStep'
    getStep' stepId = do
      Synthesis{ steps } <- getSynthesis' stm sId
      unless ( length steps > stepId ) $ throwSTM err409{ errBody="Step not exists." }
      return $ reverse steps !! stepId
    autoPostStep toEnd = do
      syn@Synthesis{..} <- getSynthesis' stm sId
      let steps' = if toEnd
          then mkStepToEnd steps
          else mkStep (head steps) : steps
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
    mkStepToEnd ss@(s:_)
      | Just s' <- naive' s = mkStepToEnd (s':ss)
      | otherwise = ss
    mkStepToEnd _ = error "Empty CompilerState."


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


backendServer frame = do
  let prefix = "import axios from 'axios';\n\
                \var api = {}\n\
                \export default api;"
  let axios' = axiosWith defAxiosOptions defCommonGeneratorOptions{ urlPrefix="http://localhost:8080"
                                                                  , SJS.moduleName="api"
                                                                  }
  createDirectoryIfMissing True $ joinPath ["web", "src", "gen"]
  writeJSForAPI (Proxy :: Proxy SynthesisAPI) ((prefix <>) . axios') $ joinPath ["web", "src", "gen", "nitta-api.js"]
  app def{ state=frame } >>= run 8080 . simpleCors
