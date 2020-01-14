{-# OPTIONS -Wall -Wcompat -Wredundant-constraints -fno-warn-missing-signatures #-}

{-|
Module      : NITTA.Utils.CodeFormat
Description : functions for more comfortable work with output codestyle
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

-- |Code format functions
inlineMarker = "###"

inline str = unlines $ map (inlineMarker ++ ) $ lines str

comment str = unlines $ map ("// " ++ ) $ lines str

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


codeLine str = dropWhile (== ' ') str ++ "\n"
