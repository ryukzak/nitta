{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Utils.CodeFormat
Description : Functions for output code. Examples in Test.CodeBlock.hs
Copyright   : (c) Daniil Prohorov, 2019
License     : BSD3
Maintainer  : aleksandr.penskoi@gmail.com
Stability   : experimental
-}

module NITTA.Utils.CodeFormat
    ( codeBlock, inline, codeLine, comment
    ) where


import qualified Data.String.Utils               as S
import           Safe                            (headDef, minimumDef)


inlineMarker = "###"

-- |For right indentations of multiple lines
inline str = unlines $ map (inlineMarker ++ ) $ lines str

-- |Add comment for each line
comment str = unlines $ map ("// " ++ ) $ lines str

-- |Function for more comfortable formatting code
codeBlock str = codeBlock' linesList [] (minIndentCalc linesList)
    where
        codeBlock' []     buff _         = delInline $ S.join "\n" $ reverse buff
        codeBlock' (x:xs) buff minIndent = codeBlock' xs buff' minIndent
            where
                buffHead = headDef "" buff
                inlineSpacesCount = length $ takeWhile (== ' ') buffHead
                inlineSpaces = replicate inlineSpacesCount ' '
                line
                    | isInline x = inlineSpaces ++ x
                    | otherwise  = drop minIndent x

                buff' = line : buff

        isInline = S.startswith inlineMarker
        delInline = S.replace inlineMarker ""

        minIndentCalc inp = minimumDef 0 spaces
            where
                spaces = filter (> 0 ) $ map (length . takeWhile (== ' ')) inlines
                inlines = filter (not . isInline) inp

        linesList = drop 1 $ lines str

-- |Simple function for writing just one line of something
codeLine str = dropWhile (== ' ') str ++ "\n"
