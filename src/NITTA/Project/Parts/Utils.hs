{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

{-|
Module      : NITTA.Project.Parts.Utils
Description : Utils for target system parts generation.
Copyright   : (c) Aleksandr Penskoi, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Project.Parts.Utils
    ( writeImplementation
    , copyLibraryFiles
    ) where

import qualified Data.List as L
import qualified Data.String.Utils as S
import           NITTA.Project.Implementation
import           NITTA.Project.Types
import           System.Directory
import           System.FilePath.Posix ( joinPath, pathSeparator )


-- |Записать реализацию на диск. Данные размещаются в указанном рабочем каталоге.
--
-- Ключ $path$ используется для корректной адресации между вложенными файлами. К примеру, в папке
-- DIR лежит два файла f1 и f2, и при этом f1 импортирует в себя f2. Для этого, зачастую, необходимо
-- указать его адресс относительно рабочего каталога, что осуществляется путём вставки этого адреса
-- на место ключа $path$.
writeImplementation pwd = writeImpl ""
    where
        writeImpl p (Immediate fn src)
            = writeFile (joinPath [pwd, p, fn]) $ S.replace "$path$" (if null p then "" else p ++ [pathSeparator]) src
        writeImpl p (Aggregate p' subInstances) = do
            let path = joinPath $ maybe [p] (\x -> [p, x]) p'
            createDirectoryIfMissing True $ joinPath [ pwd, path ]
            mapM_ (writeImpl path) subInstances
        writeImpl _ (FromLibrary _) = return ()
        writeImpl _ Empty = return ()


-- |Скопировать файл в lib, если он находится в pLibPath
copyLibraryFiles prj = mapM_ (copyLibraryFile prj) $ libraryFiles prj
    where
        copyLibraryFile Project{ pPath } file = do
            pLibPath' <- makeAbsolute $ joinPath [pPath, "lib"]
            createDirectoryIfMissing True pLibPath'
            let fileName = last $ S.split "/" file
            from <- makeAbsolute $ joinPath [pPath, file]
            to <- makeAbsolute $ joinPath [pPath, "lib", fileName]
            copyFile from to

        libraryFiles Project{ pName, pLibPath, pUnit }
            = L.nub $ concatMap (args "") [ hardware pName pUnit ]
            where
                args p (Aggregate (Just p') subInstances) = concatMap (args $ joinPath [p, p']) subInstances
                args p (Aggregate Nothing subInstances)   = concatMap (args $ joinPath [p]) subInstances
                args _ (FromLibrary fn)                   = [ joinPath [ pLibPath, fn ] ]
                args _ _                                  = []
