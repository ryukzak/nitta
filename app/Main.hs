{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}

module Main where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Encoding
import           Data.Array
import qualified Data.ByteString.Lazy    as BS
import           Data.Default
import qualified Data.Graph              as G
import           Data.List               (find, groupBy, nub, sortBy, takeWhile)
import           Data.Map                (fromList)
import qualified Data.Map                as M
import           Data.Maybe              (catMaybes, fromMaybe)
import qualified Data.Text               as T
import           Data.Typeable           (cast, typeOf)
import           GHC.Generics
import           NITTA.Base
import           NITTA.BusNetwork
import qualified NITTA.FunctionBlocks    as FB
import           NITTA.ProcessUnits
import           NITTA.ProcessUnits.FRAM


eval pu fbs =
  let Just pu' = foldl (\(Just s) n -> evaluate s n) (Just pu) fbs
  in pu'

doSteps pu acts = foldl (\s n -> step s n) pu acts





main = do
  let fram0 = eval fram [ FB.framOutput 0 "a" -- save 0main
                        , FB.framInput 0 ["a'"] -- load 0
                        , FB.reg "b" ["b'"]
                        , FB.reg "c" ["c'", "c''"]
                        ]
  let fram1 = doSteps fram0 [ PUAct (Pull ["a'"])  $ Event 1  2
                            , PUAct (Push "c")     $ Event 5  2
                            , PUAct (Push "a")     $ Event 10 2
                            , PUAct (Pull ["c'"])  $ Event 15 2
                            , PUAct (Push "b")     $ Event 20 2
                            , PUAct (Pull ["c''"]) $ Event 25 2
                            , PUAct (Pull ["b'"])  $ Event 30 2
                            ]
  -- printDPU fram1

  let veri = concat $ concat $ catMaybes
        $ map (\Step{..} -> case (cast info :: Maybe (Instruction (FRAM Int) String Int)) of
                  Just info' -> let v = "#2" ++ verilog info' ++ "\n"
                                in Just $ take (eDuration time) $ repeat v
                  _          -> Nothing
                  )
        $ steps $ process fram1

  -- mapM_ putStrLn $ map show $ reverse $ steps $ process test



  -- dumpSteps $ process test
  -- putStrLn veri
  writeFile "hdl/dpu_fram_tb_process.v" veri
  test <- foldM (\s _ -> naiveStep s) bindedNet $ take 6 $ repeat 0
  dumpSteps $ process test

  -- mapM_ putStrLn $ map show $ steps $ process test -- $ niPus test M.! "fram1"

  let v = verilog test
  writeFile "hdl/fram_net_tb_process.v" v
  putStrLn v

  return ()


deriving instance Show (Variant (Network String) String Int)
deriving instance Show (Action (Network String) String Int)

naiveStep pu@BusNetwork{..} = do
  let vars = variants pu
  when (length vars == 0) $ error "No variants!"
  -- mapM_ putStrLn $ map ((++) "" . show) vars
  -- putStrLn "-----------------------------"
  let act = v2a $ head vars
  -- putStrLn $ show act
  -- putStrLn "============================="
  return $ step pu act
  where
    -- mostly mad implementation
    v2a NetworkVariant{ vPullAt=TimeConstrain{..}, ..} = NetworkAction
      { aPullFrom=vPullFrom
      , aPullAt=Event tcFrom tcDuration
      , aPush=M.map (fmap $ \(title, TimeConstrain{..}) ->
                        (title, Event pushStartAt tcDuration)
                    ) vPush
      }
      where
        pushStartAt = tcFrom + tcDuration








fram = PU (def { frMemory=fromList [ (addr, def) | addr <- [0..10] ]
               } :: FRAM Int Passive String Int)




net = busNetwork
  [ ("fram1", fram)
  , ("fram2", fram)
  ]
  $ array (0, 15) [ (15, [("fram1", S $ (OE :: Signals (FRAM Int)))])
                  , (14, [("fram1", S $ (WR :: Signals (FRAM Int)))])
                  , (13, [])
                  , (12, [])

                  , (11, [("fram1", S $ (ADDR 3 :: Signals (FRAM Int)))])
                  , (10, [("fram1", S $ (ADDR 2 :: Signals (FRAM Int)))])
                  , ( 9, [("fram1", S $ (ADDR 1 :: Signals (FRAM Int)))])
                  , ( 8, [("fram1", S $ (ADDR 0 :: Signals (FRAM Int)))])

                  , ( 7, [("fram2", S $ (OE :: Signals (FRAM Int)))])
                  , ( 6, [("fram2", S $ (WR :: Signals (FRAM Int)))])
                  , ( 5, [])
                  , ( 4, [])

                  , ( 3, [("fram2", S $ (ADDR 3 :: Signals (FRAM Int)))])
                  , ( 2, [("fram2", S $ (ADDR 2 :: Signals (FRAM Int)))])
                  , ( 1, [("fram2", S $ (ADDR 1 :: Signals (FRAM Int)))])
                  , ( 0, [("fram2", S $ (ADDR 0 :: Signals (FRAM Int)))])
                  ]


bindedNet =
  let alg = [ FB.framInput 3 [ "a" ]
            , FB.framInput 8 [ "b"
                             , "c"
                             ]
            , FB.reg "a" ["x"]
            , FB.reg "b" ["y"]
            , FB.reg "c" ["z"]
            , FB.framOutput 0 "x"
            , FB.framOutput 1 "y"
            , FB.framOutput 2 "z"
            ]
      ni0 = eval (net :: BusNetwork String (Network String) String Int) alg
      ni1 = foldl (\s (fb, dpu) -> delegate fb dpu s) ni0 [ (alg !! 0, "fram1")
                                                          , (alg !! 1, "fram1")
                                                          , (alg !! 2, "fram2")
                                                          , (alg !! 3, "fram2")
                                                          , (alg !! 4, "fram2")
                                                          , (alg !! 5, "fram1")
                                                          , (alg !! 6, "fram1")
                                                          , (alg !! 7, "fram1")
                                                          ]
  in ni1


test = doSteps bindedNet [ NetworkAction "fram1" (Event 0 1) $ M.fromList
                           [ ("a", Just ("fram2", Event 1 1)) ]
                         , NetworkAction "fram1" (Event 1 1) $ M.fromList
                           [ ("b", Just ("fram2", Event 2 1)) ]
                         , NetworkAction "fram1" (Event 2 1) $ M.fromList
                           [ ("c", Just ("fram2", Event 3 1)) ]
                         ]




printDPU dpu = do
  let p = process dpu
  -- putStrLn $ concat $ take 6 $ repeat "0123456789"
  -- mapM_ putStrLn $ map show $ reverse $ steps p
  -- mapM_ putStrLn $ map show $ reverse $ relations p
  dumpSteps p
  putStrLn "------------------------------------------------------------"






instance ToJSON (Step String Int) where
  toJSON st@Step{ time=Event{..}, ..} =
    object $ [ "id" .= uid
             , "start" .= eStart
             , "content" .= show' info
             , "group" .= group info
             , "title" .= show st
             ] ++ case eDuration of
                    0 -> [ "type" .= ("point" :: String) ]
                    x -> [ "end" .= (eStart + eDuration) ]
    where
      show' i
        | Just (Nested _ title i' :: Nested String String Int) <- cast i =
            show i'
        | otherwise = show i



instance ToJSON Relation where
  toJSON (Vertical a b) =
    object [ "type" .= ("Vertical" :: String)
           , "a" .= a
           , "b" .= b
           ]

data Group = Group { id :: String, nestedGroups :: [String] }
  deriving (Eq, Ord)
instance ToJSON Group where
  toJSON (Group g [])     = object $ [ "id" .= g ]
  toJSON (Group g nested) = object $ [ "id" .= g, "nestedGroups" .= nested, "showNested" .= False ]




dumpSteps p@Process{..} = do
  let groups0 = nub $ map (\Step{..} -> (group info, upperGroup info)) steps
  let groups = map (\g -> Group g $ nub [ng | (ng, Just ug) <- groups0, ug == g])
                   $ map fst groups0
  BS.writeFile "resource/data.json" $ BS.concat [
    "relations_data = ", encode relations, ";\n",
    "groups_data = ", encode groups, ";\n",
    "items_data = ", encode steps, ";\n"
    ]

group i
    | Just (Nested _ title i' :: Nested String String Int) <- cast i =
        title ++ "/" ++ level i'
    | otherwise = level i

upperGroup i =
  case cast i of
    Just (Nested _ _ i' :: Nested String String Int) ->
      case cast i' of
        Just (_ :: FB.FB String) -> Nothing
        _                        -> Just $ (takeWhile (/= '/') (group i)) ++ "/Function block"
    _ -> Nothing
