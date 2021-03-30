{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{- |
Module      : NITTA.Project.Context
Description : Context for project template
Copyright   : (c) Aleksandr Penskoi, 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Project.Context (
    projectContext,
    implementationContext,
) where

import qualified Data.List as L
import qualified Data.Text as T
import NITTA.Project.TestBench
import NITTA.Project.Types
import NITTA.Utils
import Text.Ginger

nittaContextDict
    prj@Project
        { pName
        , pUnit
        , pUnitEnv
        , pTargetProjectPath
        , pInProjectNittaPath
        , pAbsTargetProjectPath
        , pAbsNittaPath
        } =
        dict
            [ ("instance", toGVal $ doc2text $ hardwareInstance (moduleName pName pUnit) pUnit pUnitEnv)
            ,
                ( "paths"
                , dict
                    [ ("abs_project", toGVal pAbsTargetProjectPath)
                    , ("project", toGVal pTargetProjectPath)
                    , ("abs_nitta", toGVal pAbsNittaPath)
                    , ("nitta", toGVal pInProjectNittaPath)
                    ]
                )
            , ("files", toGVal $ filter (".v" `L.isSuffixOf`) $ projectFiles prj)
            ,
                ( "testbench"
                , dict
                    [ ("module_name", toGVal $ testBenchTopModuleName prj)
                    ]
                )
            ]

-- |projectContext - used for template generation
projectContext prj = makeContextText $ \case
    "nitta" -> nittaContextDict prj
    unknown -> error $ "template error, variable '" <> T.unpack unknown <> "' not defined (see 'NITTA.Project.Template')"

-- |projectContext - used for Implementation generation
implementationContext prj nest = makeContextText $ \case
    "nitta" -> nittaContextDict prj
    "impl" -> dict [("paths", dict [("nest", toGVal nest)])]
    unknown -> error $ "template error, variable '" <> T.unpack unknown <> "' not defined (see 'NITTA.Project.Template')"
