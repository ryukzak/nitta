{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

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
) where

-- TODO: Improve error msg on bad template
-- TODO: Improve error on undefined on wrong variable usage
-- TODO: Fix work with subpath inside project template
-- TODO: Add template config (where to put nitta project)
-- TODO: Fix imports inside template

import Control.Monad.Identity (runIdentity)
import Data.Foldable
import qualified Data.Text as T
import qualified Data.Text.IO as T
import NITTA.Project.TestBench
import NITTA.Project.Types
import System.Directory
import System.FilePath
import Text.Ginger

writeRenderedTemplates prj@Project{pPath, pTemplates} = do
    createDirectoryIfMissing True pPath
    for_ pTemplates $ \pTemplate -> do
        templates <- findAllFiles pTemplate
        mapM_ (writeRendedTemplate (projectContext prj) pPath) templates

writeRendedTemplate context opath file = do
    src <- readFile file
    template <-
        either (error . show) return <$> runIdentity $
            parseGinger (const $ return Nothing) Nothing src
    T.writeFile (joinPath [opath, takeFileName file]) $ runGinger context template

-- |List all files insede path
findAllFiles path = do
    items <- map (path </>) <$> listDirectory path
    concat
        <$> mapM
            ( \item -> do
                isDir <- doesDirectoryExist item
                if isDir
                    then findAllFiles item
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
    unknown -> error $ "can't find variable: " <> T.unpack unknown
