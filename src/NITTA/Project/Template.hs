{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
    projectContext,
) where

-- TODO: Fix imports inside template

import Control.Exception
import Control.Monad.Identity (runIdentity)
import Data.Aeson
import Data.Default
import Data.Foldable
import Data.HashMap.Strict qualified as M
import Data.Hashable
import Data.Maybe
import Data.String.Interpolate
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Yaml
import GHC.Generics hiding (moduleName)
import NITTA.Project.Context
import NITTA.Project.Types
import System.Directory
import System.FilePath
import System.Log.Logger
import System.Path.WildMatch
import Text.Ginger

data Conf = Conf
    { template :: TemplateConf
    , signals :: M.HashMap T.Text T.Text
    }
    deriving (Generic, Show)

instance FromJSON Conf
instance ToJSON Conf

data TemplateConf = TemplateConf
    { nittaPath :: Maybe FilePath
    , ignore :: Maybe [FilePath]
    }
    deriving (Generic, Show)

defNittaPath = "."
templateConfFileName = "template.yml"

instance Default TemplateConf where
    def =
        TemplateConf
            { nittaPath = Just defNittaPath
            , ignore = Just [templateConfFileName]
            }

instance Hashable k => Default (M.HashMap k v) where
    def = M.fromList []

instance FromJSON TemplateConf
instance ToJSON TemplateConf

{- | collectNittaPath - read nittaPath from all provided target templates and
return it if all of them are the same.
-}
collectNittaPath :: [FilePath] -> IO (Either T.Text FilePath)
collectNittaPath templates = do
    paths <- mapM (\fn -> (fn,) . getNittaPath <$> readTemplateConfDef (fn </> templateConfFileName)) templates
    let path = if null paths then defNittaPath else snd $ head paths
        err =
            "inconsistency of nittaPath: "
                <> T.intercalate ", " (map (\(f, p) -> [i|#{f} -> '#{p}'|]) paths)
    return $
        if all ((== path) . snd) paths
            then Right path
            else Left err
    where
        getNittaPath = fromMaybe (error "internal error") . nittaPath . template

readTemplateConfDef :: FilePath -> IO Conf
readTemplateConfDef fn = do
    doesFileExist fn >>= \case
        True -> decodeFileThrow fn :: IO Conf
        False -> return Conf{template = def, signals = def}

applyCustomSignal
    signals
    env@UnitEnv{sigClk, sigRst} =
        env
            { sigClk = fromMaybe sigClk $ M.lookup "clk" signals
            , sigRst = fromMaybe sigRst $ M.lookup "rst" signals
            -- , sigCycleBegin = fromMaybe sigCycleBegin $ M.lookup "cycleBegin" signals
            -- , sigInCycle = fromMaybe sigInCycle $ M.lookup "inCycle" signals
            -- , sigCycleEnd = fromMaybe sigCycleEnd $ M.lookup "cycleEnd" signals
            }

writeRenderedTemplates prj@Project{pTargetProjectPath, pTemplates, pUnitEnv} = do
    createDirectoryIfMissing True pTargetProjectPath
    for_ pTemplates $ \tPath -> do
        infoM "NITTA" $ "process template: " <> tPath
        Conf
            { template = TemplateConf{ignore}
            , signals
            } <-
            readTemplateConfDef $ tPath </> templateConfFileName
        let notIgnored fn = not $ all (\wc -> wildCheckCase wc fn) $ fromMaybe [] ignore
            context = projectContext $ prj{pUnitEnv = applyCustomSignal signals pUnitEnv}
        tFiles <- filter notIgnored <$> findAllFiles tPath
        for_ tFiles $ \tFile -> do
            writeRendedTemplate context pTargetProjectPath tPath tFile

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

-- | List all files inside path
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
