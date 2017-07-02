{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE RecordWildCards        #-}

module Main where

import           Data.Default
import           Data.Dynamic
import qualified Data.Graph              as G
import           Data.List               (find, groupBy, sortBy)
import           Data.Map                (fromList)
import qualified Data.Map                as M
import           Data.Maybe              (fromMaybe)
import           NITTA.Base
import qualified NITTA.FunctionBlocks    as FB
import           NITTA.NITTA
import           NITTA.ProcessUnits
import           NITTA.ProcessUnits.FRAM


main = do
  let fram00 = fram
  let Just fram01 = evaluate fram00 (FB.framInput 0 ["a0"])
  let Just fram02 = evaluate fram01 (FB.framOutput 0 "b0")
  let Just fram1 = evaluate fram02 (FB.reg "a" ["b"])
  -- putStrLn $ show $ variants fram1
  let fram2 = step fram1 $ Interaction (Push "a") $ Event 1 2
  let Just fram3 = evaluate fram2 (FB.reg "c" ["d", "e"])
  let fram4 = step fram3 $ Interaction (Push "c") $ Event 5 2
  let fram5 = step fram4 $ Interaction (Pull ["e"]) $ Event 8 2
  let fram6 = step fram5 $ Interaction (Pull ["b"]) $ Event 12 2
  let fram7 = step fram6 $ Interaction (Pull ["d"]) $ Event 15 2
  -- putStrLn $ show $ variants fram7
  let fram8 = step fram7 $ Interaction (Pull ["a0"]) $ Event 20 2
  let fram9 = step fram8 $ Interaction (Push "b0") $ Event 25 2
  printDPU fram9
  print "ni"
  mapM_ putStrLn $ map show $ reverse $ steps $ process test
  dumpSteps $ process test
  return ()

fram = PU (def { frMemory=fromList [ (addr, def) | addr <- [0..10] ]
               } :: FRAM Int PuVariant PuAction PuStep String Int Int)

alg = [ FB.framInput 0 ["a"]
      , FB.reg "a" ["b"]
      , FB.framOutput 0 "b"
      ]

test = let ni0 = def{ niPus=M.fromList
                           [ ("fram1", fram)
                           , ("fram2", fram)
                           ]
                    } :: NITTA String
                         (NiVariant String) (NiAction String)
                         (NiStep String)
                         String Int Int
           dvs = delegationVariants ni0
           Just ni1 = foldl (\(Just s) n -> evaluate s n) (Just ni0) alg
           ni2 = foldl (\s (fb, dpu) -> delegate fb dpu s) ni1 [ (alg !! 0, "fram1")
                                                               , (alg !! 1, "fram2")
                                                               , (alg !! 2, "fram1")
                                                               ]
           ni3 = step ni2 Transport{ pullFrom="fram1"
                                   , pullAt=Event 0 1
                                   , push=M.fromList [ ("a", Just ("fram2", Event 0 1)) ]
                                   }
           ni4 = step ni3 Transport{ pullFrom="fram2"
                                   , pullAt=Event 5 1
                                   , push=M.fromList [ ("b", Just ("fram1", Event 5 1)) ]
                                   }
       in ni4




printDPU dpu = do
  let p = process dpu
  putStrLn $ concat $ take 6 $ repeat "0123456789"
  mapM_ putStrLn $ map show $ reverse $ steps p
  mapM_ putStrLn $ map show $ reverse $ relations p
  dumpSteps p
  putStrLn "------------------------------------------------------------"





class TimeLines a k | a -> k where
  timeLines :: a -> [TimeLineRow k]

timeScale = 10

data TimeLineRow k
  = TimeLineRow
  { rowKey    :: String
  , originKey :: k
  , title     :: String
  , begin     :: Int
  , end       :: Int
  } deriving (Eq)

instance (Show key) => Show (TimeLineRow key) where
  show TimeLineRow{..} = concat [
    "['", rowKey, "', ",
    "'", title, "', ",
    "null, ",
    "new Date(", show begin, "), ",
    "new Date(", show end, ")]"
    ]


instance (Var v, Key k) => TimeLines (Process PuStep v Int k) k where
  timeLines p = map step2row $ steps
    $ distributeMomentEvents stepIsMoment stepMove stepStartAt p
    where
      step2row Step{..} = uncurry (TimeLineRow (level info) key (show info)) $ interval time


stepIsMoment Step{ time=time@Event{..} } = eDuration == 0
stepMove st@Step{ time=time } t' = st{ time=time{ eStart=t' } }
stepStartAt Step{ time=Event{ eStart=t } } = (t, ())


instance (Var v, Key k) => TimeLines (Process (NiStep String) v Int k) k where
  -- timeLines p = map step2row $ steps $ distributeMomentEvents isMoment move startAt $ sortSteps p
  timeLines p = map step2row $ steps
    $ distributeMomentEvents isMoment move startAt p
    where
      isMoment NStep{ nTime=time@Event{..} } = eDuration == 0
      isMoment Nested{ nested=step }         = stepIsMoment step

      move st@NStep{ nTime=time } t'   = st{ nTime=time{ eStart=t' } }
      move st@Nested{ nested=step } t' = st{ nested=stepMove step t' }

      startAt NStep{ nTime=Event{ eStart=t } } = (t, Nothing)
      startAt Nested{ nested=step, .. }            = (fst $ stepStartAt step, Just nId)

      step2row NStep{..} = uncurry (TimeLineRow (level nInfo) nKey (show nInfo)) $ interval nTime
      step2row Nested{ nested=Step{..}, .. } =
        uncurry (TimeLineRow (nId ++ "." ++ level info) nKey (show info)) $ interval time


distributeMomentEvents isMoment move startAt p@Process{..} =
  p{ steps=snd $ foldr
     (\step (eOffset, rs) ->
        if isMoment step
        then let eStart = startAt step
                 eOffset' = M.alter (offsetUpd $ fst eStart) eStart eOffset
                 eStart' = eOffset' M.! eStart
             in (eOffset', (move step eStart') : rs)
        else (eOffset, step : rs))
     (M.fromList [], []) steps
   }

offsetUpd _ (Just x) = Just (x + 1)
offsetUpd x Nothing  = Just (timeScale * x)

interval (Event a d) | d == 0 = (a, a + 1)
interval (Event a d) = (timeScale * a, timeScale * (a + d))


dumpSteps p@Process{..} = do
  let tl = timeLines p
  let steps = "data = [\n" ++ concat (map (\e -> "    " ++ show e ++ ",\n") tl) ++ "];\n"
  let dump = steps -- ++ hierarchy'
  putStrLn dump
  writeFile "resource/data.json" dump
