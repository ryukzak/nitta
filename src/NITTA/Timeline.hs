{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures -fno-warn-orphans #-}

module NITTA.Timeline
  ( timeline
  ) where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import           Data.List            (nub, takeWhile)
import           NITTA.Types
import           NITTA.Utils
import           NITTA.Utils.JSON     ()
import           Numeric.Interval     (inf, sup)


instance ( Time t
         , ToJSON t
         ) => ToJSON (Step String t) where
  toJSON st@Step{..} =
    object $ [ "id" .= sKey
             , "start" .= (case sTime of Event t -> t; Activity t -> inf t)
             , "content" .= show' sDesc
             , "group" .= group sDesc
             , "title" .= show st
             , "inside_out" .= isInsideOut st
             ]
    ++ case sTime of
         Event _    -> [ "type" .= ("point" :: String) ]
         Activity t -> [ "end" .= (sup t + 1) ]
    where
      isInsideOut _
        | Just fb <- getFB st = insideOut fb
        | otherwise = False
      show' (NestedStep _ i) = show i
      show' i                = show i


instance ToJSON Relation where
  toJSON (Vertical a b) =
    object [ "type" .= ("Vertical" :: String)
           , "a" .= a
           , "b" .= b
           ]

data Group = Group String [String]
  deriving (Eq, Ord)
instance ToJSON Group where
  toJSON (Group g [])     = object [ "id" .= g ]
  toJSON (Group g nested) = object [ "id" .= g, "nestedGroups" .= nested, "showNested" .= False ]



timeline filename pu = do
  let Process{..} = process pu
  let groups0 = nub $ map (\Step{..} -> (group sDesc, upperGroup sDesc)) steps
  let groups = map ( (\ g -> Group g $ nub [ng | (ng, Just ug) <- groups0, ug == g]) . fst )
                 groups0
  BS.writeFile filename $ BS.concat [
    "relations_data = ", encode relations, ";\n",
    "groups_data = ", encode groups, ";\n",
    "items_data = ", encode steps, ";\n"
    ]



group (NestedStep title i) = show title ++ "/" ++ level i
group i                    = level i


upperGroup (NestedStep _ i)
  | isFB i = Nothing
  | otherwise = Just $ takeWhile (/= '/') (group i) ++ "/Function block"
upperGroup _ = Nothing
