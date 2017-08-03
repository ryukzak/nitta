{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module NITTA.Timeline(timeline) where

import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import           Data.List            (find, groupBy, nub, sortBy, takeWhile)
import           Data.Typeable        (cast, typeOf)
import           NITTA.FunctionBlocks
import           NITTA.Types
import           NITTA.Utils



instance ( ToJSON t, Time t ) => ToJSON (Step String t) where
  toJSON st@Step{ time=Event{..}, ..} =
    object $ [ "id" .= uid
             , "start" .= eStart
             , "content" .= show' info
             , "group" .= group info
             , "title" .= show st
             , "inside_out" .= isInsideOut info
             ]
    ++ case eDuration of
         0 -> [ "type" .= ("point" :: String) ]
         x -> [ "end" .= (eStart + eDuration) ]
    where
      isInsideOut i
        | Just (Nested _ title i' :: Nested String String) <- cast i
        , Just (fb :: FB String) <- cast i'
        = insideOut fb
        | otherwise = False
      show' i
        | Just (Nested _ title i' :: Nested String String) <- cast i =
            show i'
        | otherwise = show i

instance ToJSON Relation where
  toJSON (Vertical a b) =
    object [ "type" .= ("Vertical" :: String)
           , "a" .= a
           , "b" .= b
           ]

instance ( ToJSON t ) => ToJSON (SplitTime t) where
  toJSON (Time t)           = toJSON t
  toJSON (TaggetTime tag t) = toJSON t

data Group = Group { id :: String, nestedGroups :: [String] }
  deriving (Eq, Ord)
instance ToJSON Group where
  toJSON (Group g [])     = object $ [ "id" .= g ]
  toJSON (Group g nested) = object $ [ "id" .= g, "nestedGroups" .= nested, "showNested" .= False ]



timeline filename pu = do
  let Process{..} = process pu
  let groups0 = nub $ map (\Step{..} -> (group info, upperGroup info)) $ steps
  let groups = map (\g -> Group g $ nub [ng | (ng, Just ug) <- groups0, ug == g])
                   $ map fst groups0
  BS.writeFile filename $ BS.concat [
    "relations_data = ", encode relations, ";\n",
    "groups_data = ", encode groups, ";\n",
    "items_data = ", encode steps, ";\n"
    ]



group i
    | Just (Nested _ title i' :: Nested String String) <- cast i =
        title ++ "/" ++ level i'
    | otherwise = level i

upperGroup i =
  case cast i of
    Just (Nested _ _ i' :: Nested String String) ->
      case cast i' of
        Just (_ :: FB String) -> Nothing
        _                     -> Just $ (takeWhile (/= '/') (group i)) ++ "/Function block"
    _ -> Nothing
