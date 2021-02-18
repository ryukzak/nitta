{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : NITTA.Project.Parts.Quartus
Description : Quartus project for DE0Nano platform
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Project.Parts.Quartus (
    writeQuartusProject,
) where

import NITTA.Project.Template
import NITTA.Project.Types

writeQuartusProject prj = writeRenderedTemplates prj{pTemplates = ["board/DE0-Nano"]}
