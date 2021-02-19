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
    for_ pTemplates $ \tPath -> do
        tFiles <- findAllFiles tPath
        for_ tFiles $ \tFile -> do
            writeRendedTemplate (projectContext prj) pPath $ tPath </> tFile

writeRendedTemplate context opath tFile = do
    src <- readFile tFile
    let raiseError err = error $ tFile <> ": " <> formatParserError (Just src) err
        path = opath </> takeDirectory tFile
        file = opath </> tFile
    template <-
        either raiseError return <$> runIdentity $
            parseGinger (const $ return Nothing) Nothing src
    createDirectoryIfMissing True path
    T.writeFile file $ runGinger context template

-- |List all files insede path
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
