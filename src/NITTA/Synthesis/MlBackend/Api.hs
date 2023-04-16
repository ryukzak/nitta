{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : NITTA.Synthesis.MlBackend.Api
Description : ML backend API client (DTOs, request wrappers)
Copyright   : (c) Ilya Burakov, 2023
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Synthesis.MlBackend.Api (
    ScoringInput (..),
    ScoringTarget (..),
    predictScoresIO,
) where

import Data.Aeson
import Data.String.ToString
import Data.Text.Encoding qualified as T
import GHC.Generics
import NITTA.Synthesis.Types hiding (scores)
import NITTA.UIBackend.ViewHelper (NodeView)
import Network.HTTP.Client.Conduit
import Network.HTTP.Simple

data ScoringTarget = ScoringTargetSid Sid | ScoringTargetAll

instance ToJSON ScoringTarget where
    toJSON (ScoringTargetSid sid) = toJSON sid
    toJSON ScoringTargetAll = toJSON ("all" :: String)

data ScoringInput tag v x t = ScoringInput
    { scoringTarget :: ScoringTarget
    , nodes :: [NodeView tag v x t]
    }
    deriving (Generic, ToJSON)

-- instance (VarValTimeJSON v x t, ToJSON tag) => ToJSON (ScoringInput tag v x t)

newtype PostScoreRequestBody tag v x t = PostScoreRequestBody
    { inputs :: [ScoringInput tag v x t]
    }
    deriving (Generic, ToJSON)

newtype MlBackendResponse d = MlBackendResponse
    { responseData :: d
    }
    deriving (Show, Generic)

instance FromJSON d => FromJSON (MlBackendResponse d) where
    parseJSON = withObject "MlBackendResponse" $ \obj ->
        MlBackendResponse <$> obj .: "data" -- we are using the parameter's FromJSON

instance ToJSON d => ToJSON (MlBackendResponse d) where
    toJSON :: MlBackendResponse d -> Value
    toJSON (MlBackendResponse{responseData}) = object ["data" .= responseData]

newtype PostScoreResponseData = PostScoreResponseData
    {scores :: [[Float]]}
    deriving (Show, Generic, ToJSON, FromJSON)

getDefaultRequestIO baseUrl = do
    parseRequest (toString baseUrl)

getScoreRequestIO baseUrl modelName scoringInputs =
    let path = T.encodeUtf8 $ "/models/" <> modelName <> "/score"
        body = PostScoreRequestBody{inputs = scoringInputs}
     in do
            setRequestMethod "POST"
                . setRequestPath path
                . setRequestBodyJSON body
                <$> getDefaultRequestIO baseUrl

predictScoresIO modelName baseUrl inputs = do
    request <- getScoreRequestIO baseUrl modelName inputs
    response <- httpJSON request
    return $ scores $ responseData $ getResponseBody response
