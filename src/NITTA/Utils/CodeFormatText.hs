{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : NITTA.Utils.CodeFormatText
Description : Functions for output code.
Copyright   : (c) Daniil Prohorov, Artyom Kostyuchik 2021
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}
module NITTA.Utils.CodeFormatText (
    codeBlock,
    inline,
    codeLine,
    comment,
    space2tab,
) where

import Data.Maybe
import qualified Data.Text as T
import Safe (headDef, minimumMay)

inlineMarker = "###" :: T.Text
commentMarker = "// " :: T.Text

-- |For right indentations of multiple lines
inline txt = T.intercalate "\n" $ map (inlineMarker <>) $ T.lines txt

-- |Add comment for each line
comment txt = T.unlines $ map (commentMarker <>) $ T.lines txt

-- |Function for more comfortable formatting code
codeBlock txt = reformatLine lines_ [] Nothing
    where
        reformatLine [] buff _indentM = delInline $ T.intercalate "\n" $ reverse buff
        reformatLine (x : xs) buff indentM = reformatLine xs buff' indentM'
            where
                buffHead = headDef T.empty buff
                inlineSpacesCount = T.length $ T.takeWhile (== ' ') buffHead
                inlineSpaces = T.replicate inlineSpacesCount " "

                indent = fromMaybe inlineSpaces indentM

                (line, indentM') =
                    if isInline x
                        then (indent <> x, Just indent)
                        else (T.drop minIndent x, Nothing)

                buff' = line : buff

        isInline = T.isPrefixOf inlineMarker
        delInline = T.replace inlineMarker T.empty

        lines_ = drop 1 $ T.lines txt

        minIndent =
            let spaces =
                    filter (> 0) $
                        map (T.length . T.takeWhile (== ' ')) $
                            filter (not . isInline) lines_
             in fromMaybe 0 $ minimumMay spaces

-- |Simple function for writing just one line of something
codeLine txt = T.stripStart txt <> "\n"

-- |Convert space sequence with specific length to tabs
space2tab n = T.replace (T.replicate n " ") "\t"
