{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module      : NITTA.Project.Parts.Utils
Description : Utils for target system parts generation.
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Project.Parts.Utils (
    writeImplementation,
    copyLibraryFiles,
) where

import qualified Data.List as L
import qualified Data.String.Utils as S
import NITTA.Project.Implementation
import NITTA.Project.Types
import System.Directory
import System.FilePath.Posix (joinPath, pathSeparator, takeDirectory)

{- |Write 'Implementation' to the file system.

The $path$ placeholder is used for correct addressing between nested files. For
example, the PATH contains two files f1 and f2, and f1 imports f2 into itself.
To do this, you often need to specify its address relative to the working
directory, which is done by inserting this address in place of the $path$
placeholder.
-}
writeImplementation pwd = writeImpl ""
    where
        writeImpl p (Immediate fn src) =
            writeFile (joinPath [pwd, p, fn]) $ S.replace "$path$" (if null p then "" else p ++ [pathSeparator]) src
        writeImpl p (Aggregate p' subInstances) = do
            let path = joinPath $ maybe [p] (\x -> [p, x]) p'
            createDirectoryIfMissing True $ joinPath [pwd, path]
            mapM_ (writeImpl path) subInstances
        writeImpl _ (FromLibrary _) = return ()
        writeImpl _ Empty = return ()

-- |Copy library files to target path.
copyLibraryFiles prj = mapM_ (copyLibraryFile prj) $ libraryFiles prj
    where
        copyLibraryFile Project{pPath, pLibPath} file = do
            source <- makeAbsolute $ joinPath [pLibPath, file]
            target <- makeAbsolute $ joinPath [pPath, "lib", file]
            createDirectoryIfMissing True $ takeDirectory target
            copyFile source target

        libraryFiles Project{pName, pUnit} =
            L.nub $ concatMap (args "") [hardware pName pUnit]
            where
                args p (Aggregate (Just p') subInstances) = concatMap (args $ joinPath [p, p']) subInstances
                args p (Aggregate Nothing subInstances) = concatMap (args p) subInstances
                args _ (FromLibrary fn) = [fn]
                args _ _ = []
