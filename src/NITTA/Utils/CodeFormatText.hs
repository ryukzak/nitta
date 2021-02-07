{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : NITTA.Utils.CodeFormat
Description : Functions for output code. Examples in Test.CodeBlock.hs
Copyright   : (c) Daniil Prohorov, 2019
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
codeBlock txt = reformatLine linesList [] (minIndentCalc linesList)
    where
        reformatLine [] buff _ = delInline $ T.intercalate "\n" $ reverse buff
        reformatLine (x : xs) buff minIndent = reformatLine xs buff' minIndent
            where
                buffHead = headDef T.empty buff
                inlineSpacesCount = T.length $ T.takeWhile (== ' ') buffHead
                inlineSpaces = T.replicate inlineSpacesCount " "

                line
                    | isInline x = inlineSpaces <> x
                    | otherwise = T.drop minIndent x

                buff' = line : buff

        isInline = T.isPrefixOf inlineMarker
        delInline = T.replace inlineMarker T.empty

        minIndentCalc inp = fromMaybe 0 $ minimumMay spaces
            where
                spaces = filter (> 0) $ map (T.length . T.takeWhile (== ' ')) inlines
                inlines = filter (not . isInline) inp

        linesList = drop 1 $ T.lines txt

-- |Simple function for writing just one line of something
codeLine txt = T.stripStart txt <> "\n"

-- |Convert space sequence with specific length to tabs
space2tab n = T.replace (T.replicate n " ") "\t"
