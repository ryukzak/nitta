{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module Main where

import           Base
import           Data.Default
import           Data.Dynamic
import qualified Data.Graph   as G
import           Data.List    (find, sortBy)
import           Data.Map     (fromList)
import qualified Data.Map     as M
import           Data.Maybe   (fromMaybe)
import qualified FB           as FB
import           FRAM

main = do
  let fram0 = DPU (def { memoryState=fromList [ (addr, def) | addr <- [0..1] ]
                       } :: FRAM Int String (Key Int) Int)
  let Just fram1 = evaluate fram0 (FB.reg "a" ["b"])
  let fram2 = step fram1 (Push "a") 1 2
  let Just fram3 = evaluate fram2 (FB.reg "c" ["d", "e"])
  let fram4 = step fram3 (Push "c") 5 6
  let fram5 = step fram4 (Pull ["e"]) 8 10
  let fram6 = step fram5 (Pull ["b"]) 12 13
  let fram7 = step fram6 (Pull ["d"]) 15 17
  return ()
  -- printDPU fram7

  --
  -- print $ (variants fram4 :: [(Action String, Times Int)])
  --


printDPU dpu = do
  putStrLn (show dpu ++ "\n")
  putStrLn $ concat $ take 6 $ repeat "0123456789"
  mapM_ putStrLn $ map show $ reverse $ steps $ process dpu
  mapM_ putStrLn $ map show $ reverse $ relations $ process dpu
  dumpSteps $ process dpu
  putStrLn "------------------------------------------------------------"


data TimeLineRow key time
  = TimeLineRow
  { rowKey    :: key
  , originKey :: key
  , title     :: String
  , begin     :: time
  , end       :: time
  } deriving (Eq)

instance (Show key, Show time, Num time) => Show (TimeLineRow key time) where
  show TimeLineRow{..} = concat [
    "['", show rowKey, "', ",
    "'", title, "', ",
    "null, ",
    "new Date(", show begin, "), ",
    "new Date(", show (end + 1), ")]"
    ]

dumpSteps p@Process{..} = do
  let p'@Process{..} = sortSteps p
  let tl = timeLine p'
  let steps = "data = [\n" ++ concat (map (\e -> "    " ++ show e ++ ",\n") tl) ++ "];\n"
  -- let hierarchy' = "hierarchy = {\n" ++ concat (
        -- map (\(k, v) -> "'" ++ show k ++ "': " ++ show v ++ ",\n") $ M.assocs hierarchy
        -- ) ++ "};\n"
  let dump = steps -- ++ hierarchy'
  putStrLn dump
  writeFile "resource/data.json" dump

timeLine p@Process{..} =
  let dt = map ( \Step{..} -> (uncurry $ TimeLineRow key key (show desc)) $ interval time ) steps
      seqs = [x | x@(Seq _) <- relations]
      dt' = map (\x@TimeLineRow{..} ->
                   case find (\(Seq seq) -> rowKey `elem` seq) seqs of
                     Just (Seq k') -> x{ rowKey=Composite k' }
                     Nothing       -> x
                ) dt
  in dt'
  where
    interval (Event a)      = (a, a)
    interval (Interval a b) = (a, b)



sortSteps p@Process{..} =
  let hierarchy = foldl (\m (Vertical a b) -> M.adjust (b :) a m)
                      (fromList [(k, []) | k <- map key steps])
                      [x | x@(Vertical _ _) <- relations]
      (graph, v2k, k2v) = G.graphFromEdges $ map (\(a, b) -> ((), a, b)) $ M.assocs hierarchy
      steps' = sortBy (\Step{ key=a } Step{ key=b } ->
                       case (k2v a, k2v b) of
                         (Just a', Just b') ->
                           let ab = G.path graph a' b'
                               ba = G.path graph b' a'
                           in if ab || ba then if ab then LT
                                                     else GT
                              else compare a b
                         _ -> compare a b
                   ) steps
  in p{ steps=steps' }
