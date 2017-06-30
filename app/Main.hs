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
import           NITTA

main = do
  let fram00 = fram
  let Just fram01 = evaluate fram00 (FB.framInput 0 ["a0"])
  let Just fram02 = evaluate fram01 (FB.framOutput 0 "b0")
  let Just fram1 = evaluate fram02 (FB.reg "a" ["b"])
  -- putStrLn $ show $ variants fram1
  let fram2 = step fram1 (Push "a", Moment 1 2)
  let Just fram3 = evaluate fram2 (FB.reg "c" ["d", "e"])
  let fram4 = step fram3 (Push "c", Moment 5 6)
  let fram5 = step fram4 (Pull ["e"], Moment 8 10)
  let fram6 = step fram5 (Pull ["b"], Moment 12 13)
  let fram7 = step fram6 (Pull ["d"], Moment 15 17)
  -- putStrLn $ show $ variants fram7
  let fram8 = step fram7 (Pull ["a0"], Moment 20 20)
  let fram9 = step fram8 (Push "b0", Moment 25 26)
  printDPU fram9
  return ()

fram = PU (def { frMemory=fromList [ (addr, def) | addr <- [0..10] ]
               } :: FRAM Int (Variant String Int) (Action String Int) String Int (Key Int))
       :: PU                 (Variant String Int) (Action String Int) String Int (Key Int)

alg = [ FB.framInput 0 ["a"]
      , FB.reg "a" ["b"]
      , FB.framOutput 0 "b"
      ]

-- test = let ni0 = def{ dpus=M.fromList
--                            [ ("fram1", fram)
--                            , ("fram2", fram)
--                            ] } :: NITTA String (Key Int) String Int
--            dvs = delegationVariants ni0
--            Just ni1 = foldl (\(Just s) n -> evaluate s n) (Just ni0) alg
--            -- Just ni1 = foldl (\(Just s) n -> evaluate s n) (Just ni0) alg
--            ni2 = foldl (\s (fb, dpu) -> delegate fb dpu s) ni1 [ (alg !! 0, "fram1")
--                                                                , (alg !! 1, "fram2")
--                                                                , (alg !! 2, "fram1")
--                                                                ]
--        -- in nittaVariants ni2
--            ni3 = nittaStep ni2 Transport{ pullFrom="fram1"
--                                         , pullAt=Moment 0 1
--                                         , push=M.fromList [ ("a", Just ("fram2", Moment 0 1)) ]
--                                         }
--            ni4 = nittaStep ni3 Transport{ pullFrom="fram2"
--                                         , pullAt=Moment 5 1
--                                         , push=M.fromList [ ("b", Just ("fram1", Moment 5 1)) ]
--                                         }
--        in nittaVariants ni4




printDPU dpu = do
  let p = process dpu
  putStrLn $ concat $ take 6 $ repeat "0123456789"
  mapM_ putStrLn $ map show $ reverse $ steps p
  mapM_ putStrLn $ map show $ reverse $ relations p
  dumpSteps p
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
