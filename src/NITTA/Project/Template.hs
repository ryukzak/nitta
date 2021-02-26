{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS -fno-warn-orphans #-}

{- |
Module      : NITTA.Project.Template
Description : Generate target project by specific templates
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Project.Template (
    writeRenderedTemplates,
    collectNittaPath,
) where

-- TODO: Fix imports inside template

import Control.Exception
import Control.Monad.Identity (runIdentity)
import Data.Aeson
import Data.Default
import Data.Foldable
import qualified Data.HashMap.Strict as M
import Data.Hashable
import Data.Maybe
import qualified Data.String.Utils as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import GHC.Generics
import NITTA.Project.TestBench
import NITTA.Project.Types
import System.Directory
import System.FilePath
import System.Log.Logger
import System.Path.WildMatch
import Text.Ginger
import Text.Toml

data Conf = Conf
    { template :: TemplateConf
    -- , signals :: M.HashMap String String
    }
    deriving (Show)

data TemplateConf = TemplateConf
    { nittaPath :: Maybe FilePath
    , ignore :: Maybe [FilePath]
    }
    deriving (Generic, Show)

defNittaPath = "."
templateConfFileName = "template.toml"

instance Default TemplateConf where
    def =
        TemplateConf
            { nittaPath = Just defNittaPath
            , ignore = Just [templateConfFileName]
            }

instance (Eq k, Hashable k) => Default (M.HashMap k v) where
    def = M.fromList []

instance FromJSON TemplateConf
instance ToJSON TemplateConf

{- |collectNittaPath - read nittaPath from all provided target templates and
return it if all of them are the same.
-}
collectNittaPath :: [FilePath] -> IO (Either String FilePath)
collectNittaPath templates = do
    paths <- mapM (\fn -> (fn,) . getNittaPath <$> readTemplateConfDef fn) $ map (</> templateConfFileName) templates
    let path = if null paths then defNittaPath else snd $ head paths
        err =
            "inconsistency of nittaPath: "
                <> S.join ", " (map (\(f, p) -> f <> " -> '" <> p <> "'") paths)
    return $
        if all ((== path) . snd) paths
            then Right path
            else Left err
    where
        getNittaPath = fromMaybe (error "internal error") . nittaPath . template

readTemplateConfDef fn = do
    text <-
        doesFileExist fn >>= \case
            True -> T.readFile fn
            False -> return ""
    let conf = either (error . show) id $ parseTomlDoc (fn <> ": parse error: ") text
    return
        Conf
            { template = confLookup "template" conf
            -- , signals = confLookup "signals" conf
            }
    where
        confLookup sec conf =
            maybe
                def
                (unwrap (fn <> " in section [" <> T.unpack sec <> "]: ") . fromJSON . toJSON)
                $ M.lookup sec conf
        unwrap _prefix (Success a) = a
        unwrap prefix (Error msg) = error $ prefix <> msg

writeRenderedTemplates prj@Project{pTargetProjectPath, pTemplates, pUnitEnv} = do
    createDirectoryIfMissing True pTargetProjectPath
    for_ pTemplates $ \tPath -> do
        infoM "NITTA" $ "process template: " <> tPath
        Conf
            { template = TemplateConf{ignore}
            } <-
            readTemplateConfDef $ tPath </> templateConfFileName
        let notIgnored fn = not $ all (\wc -> wildCheckCase wc fn) $ fromMaybe [] ignore
        tFiles <- filter notIgnored <$> findAllFiles tPath
        for_ tFiles $ \tFile -> do
            writeRendedTemplate (projectContext prj) pTargetProjectPath tPath tFile

writeRendedTemplate context opath tPath tFile = do
    -- Why we use Text and unpack it immidiatly? We need to avoid lazyness.
    try (T.readFile $ tPath </> tFile) >>= \case
        Left (e :: IOException) ->
            warningM "NITTA" $ "template problem SKIP: " <> show e
        Right src -> writeRendedTemplate' context opath tPath tFile $ T.unpack src

writeRendedTemplate' context opath tPath tFile src = do
    let raiseError err = error $ tPath </> tFile <> ": " <> formatParserError (Just src) err
    template <-
        either raiseError return <$> runIdentity $
            parseGinger (const $ return Nothing) Nothing src
    createDirectoryIfMissing True $ opath </> takeDirectory tFile
    T.writeFile (opath </> tFile) $ runGinger context template

-- |List all files inside path
findAllFiles root = findAllFiles' ""
    where
        findAllFiles' path = do
            items <- map (path </>) <$> listDirectory (root </> path)
            concat
                <$> mapM
                    ( \item -> do
                        isDir <- doesDirectoryExist (root </> item)
                        if isDir
                            then findAllFiles' item
                            else return [item]
                    )
                    items

projectContext prj@Project{pName, pUnit, pUnitEnv} = makeContextText $ \case
    "nitta" ->
        dict
            [ ("instance", toGVal $ hardwareInstanceT (moduleNameT (T.pack pName) pUnit) pUnit pUnitEnv)
            , ("files", toGVal $ projectFiles prj)
            ,
                ( "testbench"
                , dict
                    [ ("module_name", toGVal $ testBenchTopModuleName prj)
                    ]
                )
            ]
    unknown -> error $ "template error, variable '" <> T.unpack unknown <> "' not defined (see 'NITTA.Project.Template')"
